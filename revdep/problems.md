# nullabor

Version: 0.3.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘forecast’ ‘rlang’ ‘tidyverse’ ‘tsibble’
      All declared Imports should be used.
    ```

# sugrrants

Version: 0.2.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
                 ordered = tsibble::is_ordered(data))
      15: withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      16: eval(quote(`_fseq`(`_lhs`)), env, env)
      17: eval(quote(`_fseq`(`_lhs`)), env, env)
      18: `_fseq`(`_lhs`)
      19: freduce(value, `_function_list`)
      20: withVisible(function_list[[k]](value))
      21: function_list[[k]](value)
      
      ══ testthat results  ═══════════════════════════════════════════════
      OK: 38 SKIPPED: 0 FAILED: 1
      1. Error: The tsibble data (@test-calendar.R#126) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘gtable’
      All declared Imports should be used.
    ```

