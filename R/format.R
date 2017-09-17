#' @export
print.tbl_ts <- function(x, ...) {
  int_x <- interval(x)
  grp_var <- key(x)
  if (is_empty(grp_var)) {
    cat("# A tsibble of", display_int(int_x), "time interval", "\n")
  } else {
    cat(
      "# A tsibble of", display_int(int_x), "time interval", "for", 
      cat_chr(x, grp_var), "\n"
    )
  }
  NextMethod()
  invisible(x)
}

cat_chr <- function(.data, ...) {
  UseMethod("cat_chr")
}

cat_chr.tbl_ts <- function(.data, ...) { # ... is quos
  paste(dots2str(...), collapse = ", ")
}

cat_chr.tbl_hts <- function(.data, ...) { # ... is quos
  paste(dots2str(...), collapse = " | ")
}

cat_chr.tbl_gts <- function(.data, ...) { # ... is quos
  paste(dots2str(...), collapse = " * ")
}

mts2tbl <- function(x, tz = "UTC") {
  tbl <- bind_cols(time = time2date(x, tz = tz), as_tibble(x))
  gather(tbl, key = key, value = value, -time)
}

# recursive function to repeat nodes for hts
rep_nodes <- function(nodes, index = seq_along(nodes[[level]]), level = 1L) {
  index <- rep.int(index, nodes[[level]])
  if (level == length(nodes)) {
    return(index)
  } else {
    rep_nodes(nodes, index, level + 1L)
  }
}

