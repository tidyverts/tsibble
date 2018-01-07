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

# regular time interval is obtained from the minimal time distance.
# duplicated time entries result in 0L.
# if validate = FALSE in as_tsibble, skip to check duplicated entries
min_interval <- function(x, duplicated = TRUE) {
  if (has_length(x, 1)) { # only one time index
    return(NA_integer_)
  }
  abs_diff <- abs(diff(as.numeric(x), na.rm = TRUE))
  if (duplicated) {
    return(minp(abs_diff)) # faster than min(abs_diff[abs_diff > 0])
  } else {
    min(abs_diff)
  }
}

validate_vars <- function(j, x) { # j = quos/chr/dbl
  tidyselect::vars_select(.vars = x, !!! j)
}

surround <- function(x, bracket = "(") {
  if (bracket == "(") {
    return(paste0("(", x, ")"))
  } else if (bracket == "[") {
    return(paste0("[", x, "]"))
  } else {
    paste0("<", x, ">")
  }
}

quo_text2 <- function(x) {
  quo_text(x, width = 500L)
}

min0 <- function(x) {
  min(x, na.rm = TRUE)
}

max0 <- function(x) {
  max(x, na.rm = TRUE)
}
