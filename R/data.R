#' Pedestrian counts in Melbourne city
#'
#' A dataset containing the hourly pedestrian counts from 2015-01-01 to 
#' 2016-12-31 at 4 sensors in the city of Melbourne. The variables are as follows:
#'
#' @format A tsibble with 66071 rows and 5 variables:
#' \describe{
#'   \item{Sensor}{Sensor names (key variable)}
#'   \item{Date_Time}{Date time when the pedestrian counts are recorded (index)}
#'   \item{Date}{Date when the pedestrian counts are recorded}
#'   \item{Time}{Hour associated with Date_Time}
#'   \item{Counts}{Hourly pedestrian counts}
#' }
#' @docType data
#' @name pedestrian
#' @usage pedestrian
#' @examples
#'    pedestrian
"pedestrian"
