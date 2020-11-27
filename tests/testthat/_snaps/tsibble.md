# Year week with 1 week interval

    # A tsibble: 5 x 2 [1W]
          yrwk  value
        <week>  <dbl>
    1 2017 W05 -1.53 
    2 2017 W06 -1.94 
    3 2017 W07  0.487
    4 2017 W08  1.25 
    5 2017 W09 -0.217

# Year month with 1 month interval

    # A tsibble: 5 x 2 [1M]
         yrmth  value
         <mth>  <dbl>
    1 2017 Jan -0.780
    2 2017 Feb  0.349
    3 2017 Mar  0.682
    4 2017 Apr -0.531
    5 2017 May -0.677

# A single key

    # A tsibble: 10 x 3 [1D]
    # Key:       group [2]
       date       group   value
       <date>     <chr>   <dbl>
     1 2017-02-01 a      2.66  
     2 2017-02-02 a     -1.29  
     3 2017-02-03 a      0.0151
     4 2017-02-04 a     -0.0397
     5 2017-02-05 a      2.19  
     6 2017-02-01 b     -1.98  
     7 2017-02-02 b      0.464 
     8 2017-02-03 b     -0.262 
     9 2017-02-04 b     -1.14  
    10 2017-02-05 b      0.211 

