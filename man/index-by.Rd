% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/index-by.R
\name{index_by}
\alias{index_by}
\title{Group by time index and collapse with \code{summarise()}}
\usage{
index_by(.data, ...)
}
\arguments{
\item{.data}{A \code{tbl_ts}.}

\item{...}{If empty, grouping the current index. If not empty, a single
expression is required for either an existing variable or a name-value pair.
A lambda expression is supported, for example \code{~ as.Date(.)} where \code{.} refers
to the index variable.
The index functions that can be used, but not limited:
\itemize{
\item \link[lubridate:year]{lubridate::year}: yearly aggregation
\item \link{yearquarter}: quarterly aggregation
\item \link{yearmonth}: monthly aggregation
\item \link{yearweek}: weekly aggregation
\item \link{as.Date} or \link[lubridate:as_date]{lubridate::as_date}: daily aggregation
\item \link[lubridate:round_date]{lubridate::ceiling_date}, \link[lubridate:round_date]{lubridate::floor_date}, or \link[lubridate:round_date]{lubridate::round_date}:
fine-resolution aggregation
\item Extract time components functions, such as \code{\link[lubridate:hour]{lubridate::hour()}} & \code{\link[lubridate:day]{lubridate::day()}}
\item other index functions from other packages or self-defined functions
}}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

\code{index_by()} is the counterpart of \code{group_by()} in temporal context, but it
only groups the time index. The following operation is applied to each partition
of the index, similar to \code{group_by()} but dealing with index only.
\code{index_by()} + \code{summarise()} will update the grouping index variable to be
the new index. Use \code{ungroup()} to remove the index grouping vars.
}
\details{
\itemize{
\item A \code{index_by()}-ed tsibble is indicated by \code{@} in the "Groups" when
displaying on the screen.
}
}
\examples{
pedestrian \%>\% index_by()
# Monthly counts across sensors
library(dplyr, warn.conflicts = FALSE)
monthly_ped <- pedestrian \%>\%
  group_by_key() \%>\%
  index_by(Year_Month = ~ yearmonth(.)) \%>\%
  summarise(
    Max_Count = max(Count),
    Min_Count = min(Count)
  )
monthly_ped
index(monthly_ped)

# Using existing variable
pedestrian \%>\%
  group_by_key() \%>\%
  index_by(Date) \%>\%
  summarise(
    Max_Count = max(Count),
    Min_Count = min(Count)
  )

# Attempt to aggregate to 4-hour interval, with the effects of DST
pedestrian \%>\%
  group_by_key() \%>\%
  index_by(Date_Time4 = ~ lubridate::floor_date(., "4 hour")) \%>\%
  summarise(Total_Count = sum(Count))

library(lubridate, warn.conflicts = FALSE)
# Annual trips by Region and State
tourism \%>\%
  index_by(Year = ~ year(.)) \%>\%
  group_by(Region, State) \%>\%
  summarise(Total = sum(Trips))

# Rounding to financial year, using a custom function
financial_year <- function(date) {
  year <- year(date)
  ifelse(quarter(date) <= 2, year, year + 1)
}
tourism \%>\%
  index_by(Year = ~ financial_year(.)) \%>\%
  summarise(Total = sum(Trips))
}
