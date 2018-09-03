## helpers
# ref: tibble:::big_mark
big_mark <- function(x, ...) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x, big.mark = mark, ...)
}

# ref: tibble:::cat_line
cat_line <- function(...) {
  cat(paste0(..., "\n"), sep = "")
}

dim_tbl_ts <- function(x) {
  dim_x <- dim(x)
  format_dim <- purrr::map_chr(dim_x, big_mark)
  paste(format_dim, collapse = " x ")
}

split_period <- function(x) {
  output <- lubridate::seconds_to_period(x)
  list(
    year = output$year, month = output$month, day = output$day,
    hour = output$hour, minute = output$minute, second = output$second
  )
}

paste_comma <- function(...) {
  paste(..., collapse = ", ")
}

first_arg <- function(x) {
  purrr::compact(purrr::map(x, ~ dplyr::first(call_args(.))))
}

# regular time interval is obtained from the greatest common divisor of positive
# time distances.
gcd_interval <- function(x) {
  if (has_length(x, 1)) { # only one time index
    NA_real_
  } else {
    gcd_vector(x)
  }
}

validate_vars <- function(j, x) { # j = quos/chr/dbl
  tidyselect::vars_select(.vars = x, !!! j)
}

surround <- function(x, bracket = "(") {
  if (bracket == "(") {
    paste0("(", x, ")")
  } else if (bracket == "[") {
    paste0("[", x, "]")
  } else if (bracket == "<") {
    paste0("<", x, ">")
  } else {
    paste0("`", x, "`")
  }
}

min0 <- function(...) {
  min(..., na.rm = TRUE)
}

max0 <- function(...) {
  max(..., na.rm = TRUE)
}

dont_know <- function(x, FUN) {
  cls <- class(x)[1]
  msg <- sprintf(
    "`%s()` doesn't know how to coerce the `%s` class yet.", FUN, cls
  )
  abort(msg)
}

unknown_interval <- function(x) {
  no_zeros <- !purrr::map_lgl(x, function(x) x == 0)
  if (sum(no_zeros) == 0) abort("Cannot deal with data of unknown interval.")
}

is_even <- function(x) {
  (abs(x) %% 2) == 0
}

exceed_rows <- function(x, n = 1L) {
  nr <- NROW(x)
  if (n > nr) abort(sprintf("Must not exceed the rows (%i).", nr))
}

# inlined from https://github.com/r-lib/cli/blob/master/R/utf8.R
is_utf8_output <- function() {
  opt <- getOption("cli.unicode", NULL)
  if (! is.null(opt)) {
    isTRUE(opt)
  } else {
    l10n_info()$`UTF-8` && !is_latex_output()
  }
}

is_latex_output <- function() {
  if (!("knitr" %in% loadedNamespaces())) return(FALSE)
  get("is_latex_output", asNamespace("knitr"))()
}
