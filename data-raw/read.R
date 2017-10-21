library(tsibble)
library(tidyverse)
library(lubridate)

# Melbourne pedestrian sensor data
pedestrian <- rwalkr::run_melb(
  year = 2015:2016,
  sensor = c(
    "Birrarung Marr",
    "Southern Cross Station",
    "Bourke Street Mall (North)",
    "QV Market-Elizabeth St (West)"
  ),
  na.rm = TRUE)

pedestrian <- as_tsibble(pedestrian, Sensor, index = Date_Time)
devtools::use_data(pedestrian, overwrite = TRUE)

# Australia: Domestic Overnight Trips ('000)
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
  mutate(Quarter = yearqtr(myd(Quarter)))

# convert to tsibble
tourism <- qtr_data %>% 
  as_tsibble(Region | State, Purpose, index = Quarter)
devtools::use_data(tourism, overwrite = TRUE)
