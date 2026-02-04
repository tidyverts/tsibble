# Australian domestic overnight trips

A dataset containing the quarterly overnight trips from 1998 Q1 to 2016
Q4 across Australia.

## Usage

``` r
tourism
```

## Format

A tsibble with 23,408 rows and 5 variables:

- **Quarter**: Year quarter (index)

- **Region**: The tourism regions are formed through the aggregation of
  Statistical Local Areas (SLAs) which are defined by the various State
  and Territory tourism authorities according to their research and
  marketing needs

- **State**: States and territories of Australia

- **Purpose**: Stopover purpose of visit:

  - "Holiday"

  - "Visiting friends and relatives"

  - "Business"

  - "Other reason"

- **Trips**: Overnight trips in thousands

## References

[Tourism Research Australia](https://www.tra.gov.au)

## Examples

``` r
library(dplyr)
data(tourism)
# Total trips over geographical regions
tourism %>%
  group_by(Region, State) %>%
  summarise(Total_Trips = sum(Trips))
#> # A tsibble: 6,080 x 4 [1Q]
#> # Key:       Region, State [76]
#> # Groups:    Region [76]
#>    Region   State           Quarter Total_Trips
#>    <chr>    <chr>             <qtr>       <dbl>
#>  1 Adelaide South Australia 1998 Q1        659.
#>  2 Adelaide South Australia 1998 Q2        450.
#>  3 Adelaide South Australia 1998 Q3        593.
#>  4 Adelaide South Australia 1998 Q4        524.
#>  5 Adelaide South Australia 1999 Q1        548.
#>  6 Adelaide South Australia 1999 Q2        569.
#>  7 Adelaide South Australia 1999 Q3        538.
#>  8 Adelaide South Australia 1999 Q4        562.
#>  9 Adelaide South Australia 2000 Q1        646.
#> 10 Adelaide South Australia 2000 Q2        563.
#> # ℹ 6,070 more rows
```
