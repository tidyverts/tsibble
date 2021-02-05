# brolgar

<details>

* Version: 0.1.0
* GitHub: https://github.com/njtierney/brolgar
* Source code: https://github.com/cran/brolgar
* Date/Publication: 2020-12-16 15:30:05 UTC
* Number of recursive dependencies: 144

Run `revdep_details(, "brolgar")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      `expected`: TRUE 
      ── Failure (test-stratify-key.R:95:3): stratify_keys returns the same number of keys per strata ──
      all(strata_equal_2$n == 6) is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      ── Failure (test-stratify-key.R:96:3): stratify_keys returns the same number of keys per strata ──
      all(strata_equal_3$n %in% c(6, 7, 6, 6)) is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 5 | WARN 76 | SKIP 4 | PASS 276 ]
      Error: Test failures
      Execution halted
    ```

