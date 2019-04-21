library(tsibble)
library(tidyverse)
library(lubridate)

# Melbourne pedestrian sensor data ----
pedestrian <- rwalkr::melb_walk_fast(
  year = 2015:2016,
  sensor = c(
    "Birrarung Marr",
    "Southern Cross Station",
    "Bourke Street Mall (North)",
    "QV Market-Elizabeth St (West)"
  ), na.rm = TRUE)

pedestrian <- as_tsibble(
  pedestrian, key = Sensor, index = Date_Time
)
usethis::use_data(pedestrian, overwrite = TRUE, compress = "xz")

# Australia: Domestic Overnight Trips ('000) ----
domestic_trips <- read_csv(
  "data-raw/domestic-trips.csv", skip = 11,
  col_names = c("Quarter", "Region", "Holiday", "Visiting", "Business", "Other"),
  n_max = 6804
)

# fill NA in "Quarter" using the last obs
fill_na <- domestic_trips %>%
  fill(Quarter, .direction = "down") %>% 
  filter(Quarter != "Total")

# separate State from "Region"
state <- c(
  "New South Wales", "Victoria", "Queensland", "South Australia",
  "Western Australia", "Tasmania", "Northern Territory", "ACT"
)
state_na <- fill_na %>%
  mutate(State = if_else(Region %in% state, Region, NA_character_)) %>%
  fill(State, .direction = "up") %>%
  filter(!(Region %in% state))

# gather Stopover purpose of visit
long_data <- state_na %>%
  gather("Purpose", "Trips", Holiday:Other)

# maniputate Quarter
qtr_data <- long_data %>%
  mutate(
    Quarter = paste(gsub(" quarter", "", Quarter), "01"),
    Quarter = yearquarter(myd(Quarter))
  )

# convert to tsibble
tourism <- qtr_data %>%
  as_tsibble(key = c(Region, State, Purpose), index = Quarter)
usethis::use_data(tourism, overwrite = TRUE, compress = "xz")

# hms data ----
# x <- seq(0, 8.99, by = 1/20)
# y <- seq(0, 5.99, by = 1/30)
# sin_x <- round(sinpi(x) + rnorm(180, sd = 0.1), 2)
# cos_x <- round(cospi(y) + rnorm(180, sd = 0.3), 2)
# secs <- rep(rep.int(0:59, 3), 2)
# mins <- rep(rep(0:2, each = 60), 2)
# sincos <- tibble(
#   length = hms::hms(seconds = secs, minutes = mins),
#   trig = rep(c("sine", "cosine"), each = 180),
#   value = c(sin_x, cos_x)
# )
# sincos <- as_tsibble(sincos, trig, index = length)
# devtools::use_data(sincos, overwrite = TRUE)
