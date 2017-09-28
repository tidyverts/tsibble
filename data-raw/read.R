library(tsibble)
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
