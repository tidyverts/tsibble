# Loading libraries -----------------------------------------------------------
library(tidyverse)
library(lubridate)

# Exchange rates
# ----------------------------written by @cpsievert---------------------------
# (1) Get a free plan from https://openexchangerates.org/signup/free
# (2) Tell this function your API key -- Sys.setenv("OER_KEY", "your-key-here")
# Sys.setenv("OER_KEY" = "")
getDay <- function(day) {
  u <- sprintf(
    "https://openexchangerates.org/api/historical/%s.json?app_id=%s",
    day, Sys.getenv("OER_KEY")
  )
  res <- jsonlite::fromJSON(u)
  res$rates$date <- as.POSIXct(res$timestamp, origin = "1970-01-01")
  data.frame(res$rates)
}

getRates <- function(start = end - 3, end = Sys.Date()) {
  days <- seq(start, end, by = "1 day")
  Reduce("rbind", lapply(days, getDay))
}

xrates <- getRates(start = Sys.Date() - 60)
# --------------------------END------------------------------------------------

# BoM data
# devtools::install_github("toowoombatrio/bomrang")
library(bomrang)
sydney <- get_current_weather("Sydney Airport Amo")
melbourne <- get_current_weather("Melbourne Airport")
brisbane <- get_current_weather("Brisbane Aero")
perth <- get_current_weather("Perth Airport")
adelaide <- get_current_weather("Adalaide Airport")
hobart <- get_current_weather("Hobart Airport")
canberra <- get_current_weather("Canberra Airport")
darwin <- get_current_weather("Darwin Airport")
au_weather <- bind_rows(
  sydney, melbourne, brisbane, perth, adelaide, hobart, canberra, darwin
 )

# tidyverse core pkgs daily downloads
# devtools::install_github("metacran/cranlogs")
library(cranlogs)
start <- "2015-01-01"
end <- "2016-12-31"
pkgs <- c("ggplot2", "tibble", "tidyr", "readr", "purrr", "dplyr")
tidypkgs <- map_df(pkgs, ~ cran_downloads(.x, from = start, to = end))
devtools::use_data(tidypkgs, overwrite = TRUE)
