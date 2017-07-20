## helper functions ------------------
min_na <- function(x) {
  min(x, na.rm = TRUE)
}

max_na <- function(x) {
  max(x, na.rm = TRUE)
}

expand.grid2 <- function(...) {
  expand.grid(..., KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
}

is_constant <- function(x) {
  return(diff(range(x, na.rm = TRUE)) < .Machine$double.eps ^ 0.5)
}

# Normalise the numerics to range from 0 to 1
normalise <- function(x, xmin = NULL, xmax = NULL) {
  if (is_constant(x)) return(x)
  if (is.null(xmin)) xmin <- min_na(x)
  if (is.null(xmax)) xmax <- max_na(x)
  return((x - xmin) / (xmax - xmin))
}

min_diff <- function(x) {
  return(min(abs(diff(x, na.rm = TRUE))))
}
