# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.5.0 (2018-04-23) |
|system   |x86_64, darwin15.6.0         |
|ui       |RStudio (1.1.453)            |
|language |(EN)                         |
|collate  |en_AU.UTF-8                  |
|tz       |Australia/Melbourne          |
|date     |2018-07-06                   |

## Packages

|package |*  |version |date       |source                             |
|:-------|:--|:-------|:----------|:----------------------------------|
|ggplot2 |   |3.0.0   |2018-07-06 |Github (tidyverse/ggplot2@4f272fe) |
|tsibble |*  |0.3.0   |2018-05-29 |cran (@0.3.0)                      |

# Check results

1 packages with problems

|package   |version | errors| warnings| notes|
|:---------|:-------|------:|--------:|-----:|
|sugrrants |0.1.2   |      1|        1|     0|

## sugrrants (0.1.2)
Maintainer: Earo Wang <earo.wang@gmail.com>  
Bug reports: https://github.com/earowang/sugrrants/issues

1 error  | 1 warning  | 0 notes

```
checking examples ... ERROR
Running examples in ‘sugrrants-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: frame_calendar
> ### Title: Rearrange a temporal data frame to a calendar-based data format
> ###   using linear algebra
> ### Aliases: frame_calendar prettify
> 
... 31 lines ...
+     x = Time, y = Hourly_Counts, date = Date, sunday = TRUE
+   )
> 
> p2 <- grped_calendar %>%
+   ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date)) +
+   geom_line() +
+   facet_wrap(~ Sensor_Name, nrow = 2)
Error in grouped_indices_grouped_df_impl(.data) : 
  Need at least one column for `hash()`
Calls: %>% ... group_indices.grouped_df -> grouped_indices_grouped_df_impl
Execution halted

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...

Attaching package: 'dplyr'

The following objects are masked from 'package:stats':

    filter, lag

The following objects are masked from 'package:base':

    intersect, setdiff, setequal, union

Loading required package: viridisLite
Loading required package: ggplot2
Quitting from lines 115-125 (frame-calendar.Rmd) 
Error: processing vignette 'frame-calendar.Rmd' failed with diagnostics:
Need at least one column for `hash()`
Execution halted

```

