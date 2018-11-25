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
  format_dim <- map_chr(dim_x, big_mark)
  paste(format_dim, collapse = " x ")
}

paste_comma <- function(...) {
  paste(..., collapse = ", ")
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

format_tz <- function(x) {
  tz <- attr(x, "tzone")[[1]]
  if (is_null(tz) || is.character(tz) && !nzchar(tz)) {
    "?"
  } else {
    tz
  }
}
