possibly_quosure <- function(x) {
  check <- try(is_quosure(x), silent = TRUE)
  ifelse(class(check) != "try-error", TRUE, FALSE)
}

possibly_identity <- function(x) { # x can be a list or a vector
  any(vapply(x, function(x) x == 1, logical(1)))
}

dots2str <- function(...) { # list
  peel <- map(..., get_expr) # quosure to expr
  strs <- map_chr(peel, deparse) # expr to string
  return(strs) # return a vector of characters
}
