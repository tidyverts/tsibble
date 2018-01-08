library(tsibble)
library(tidyverse)
library(lubridate)

# Melbourne pedestrian sensor data ----
pedestrian <- rwalkr::run_melb(
  year = 2015:2016,
  sensor = c(
    "Birrarung Marr",
    "Southern Cross Station",
    "Bourke Street Mall (North)",
    "QV Market-Elizabeth St (West)"
  ),
  na.rm = TRUE)

pedestrian <- as_tsibble(
  pedestrian, key = tsibble::id(Sensor), index = Date_Time
)
devtools::use_data(pedestrian, compress = "xz", overwrite = TRUE)

# Australia: Domestic Overnight Trips ('000) ----
domestic_trips <- readxl::read_excel(
  "data-raw/domestic-trips.xlsx", skip = 12,
  col_names = c("Quarter", "Region", "Holiday", "Visiting", "Business", "Other"),
  n_max = 6384
)

# fill NA in "Quarter" using the last obs
fill_na <- domestic_trips %>%
  fill(Quarter, .direction = "down")

# separate State from "Region"
state <- c(
  "New South Wales", "Victoria", "Queensland", "South Australia",
  "Western Australia", "Northern Territory", "ACT"
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
  mutate(Quarter = paste(gsub(" quarter", "", Quarter), "01")) %>%
  mutate(Quarter = yearquarter(myd(Quarter)))

# convert to tsibble
tourism <- qtr_data %>%
  as_tsibble(key = tsibble::id(Region | State, Purpose), index = Quarter)
devtools::use_data(tourism, compress = "xz", overwrite = TRUE)

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
