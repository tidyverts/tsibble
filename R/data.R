#' Pedestrian counts in the city of Melbourne
#'
#' A dataset containing the hourly pedestrian counts from 2015-01-01 to 
#' 2016-12-31 at 4 sensors in the city of Melbourne.
#'
#' @format A tsibble with 66,071 rows and 5 variables:
#' * **Sensor**: Sensor names (key)
#' * **Date_Time**: Date time when the pedestrian counts are recorded (index)
#' * **Date**: Date when the pedestrian counts are recorded
#' * **Time**: Hour associated with Date_Time
#' * **Counts**: Hourly pedestrian counts
#' @references [Melbourne Open Data Portal](https://data.melbourne.vic.gov.au/Transport-Movement/Pedestrian-volume-updated-monthly-/b2ak-trbp)
#' @docType data
#' @name pedestrian
#' @usage pedestrian
#' @examples
#' data(pedestrian)
#' # make implicit missingness to be explicit ----
#' pedestrian %>% fill_gaps()
#' # compute daily maximum counts across sensors ----
#' pedestrian %>% 
#'   group_by(Sensor) %>% 
#'   index_by(Date) %>% # group by Date and use it as new index
#'   summarise(MaxC = max(Count))
"pedestrian"

#' Australian domestic overnight trips
#'
#' A dataset containing the quarterly overnight trips from 1998 Q1 to 2016 Q4
#' across Australia.
#'
#' @format A tsibble with 23,408 rows and 5 variables:
#' * **Quarter**: Year quarter (index)
#' * **Region**: The tourism regions are formed through the aggregation of 
#' Statistical Local Areas (SLAs) which are defined by the various State and 
#' Territory tourism authorities according to their research and marketing 
#' needs
#' * **State**: States and territories of Australia
#' * **Purpose**: Stopover purpose of visit: 
#'   - "Holiday"
#'   - "Visiting friends and relatives" 
#'   - "Business" 
#'   - "Other reason"
#' * **Trips**: Overnight trips in thousands
#' @references [Tourism Research Australia](https://www.tra.gov.au)
#' @docType data
#' @name tourism
#' @usage tourism
#' @examples
#' data(tourism)
#' # nesting and crossed structure
#' key(tourism)
#' # Total trips over geographical regions
#' tourism %>% 
#'   group_by(Region, State) %>% 
#'   summarise(Total_Trips = sum(Trips))
"tourism"
