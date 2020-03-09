is_index_null <- function(x) {
  if (is_null(x %@% "index")) {
    abort("The `index` has been dropped somehow. Please reconstruct tsibble.")
  }
}

dont_know <- function(x, FUN) {
  cls <- class(x)[1]
  msg <- sprintf(
    "`%s()` doesn't know how to handle the %s class yet.", FUN, cls
  )
  abort(msg)
}

abort_unknown_interval <- function(x) {
  if (unknown_interval(x)) {
    abort(sprintf("`%s` can't proceed with tsibble of unknown interval.",
      deparse(sys.call(-1))))
  }
}

abort_if_irregular <- function(x) {
  if (!is_regular(x)) {
    abort(sprintf("`%s` can't handle tsibble of irregular interval.",
      deparse(sys.call(-1))))
  }
}

not_tsibble <- function(x) {
  if (!is_tsibble(x)) {
    abort(sprintf("%s is not a tsibble.", deparse(substitute(x))))
  }
}

pkg_not_available <- function(pkg, min_version = NULL) {
  pkg_lgl <- requireNamespace(pkg, quietly = TRUE)
  if (!pkg_lgl) {
    abort(sprintf("Package `%s` required.\nPlease install and try again.", pkg))
  } else if (pkg_lgl && is_null(min_version)) {
    return()
  } else if (utils::packageVersion(pkg) < min_version) {
    abort(sprintf(
      "Package `%s` (>= v%s) required.\nPlease install and try again.", 
      pkg, min_version))
  }
}
