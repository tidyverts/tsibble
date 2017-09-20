globalVariables(c("time"))

#' @rdname as-tsibble
#' @param tz Time zone.
as_tsibble.ts <- function(x, tz = "UTC", ...) {
  idx <- time2date(x, tz = tz)
  value <- unclass(x) # rm its ts class

  output <- as_tsibble(tibble::tibble(time = idx, value = value), index = time)
  colnames(output)[2] <- deparse(substitute(x))
  output
}

#' @rdname as-tsibble
as_tsibble.mts <- function(x, tz = "UTC", ...) {
  long_tbl <- mts2tbl(x, tz = tz)
  colnames(long_tbl)[3] <- deparse(substitute(x))
  as_tsibble(long_tbl, index = time, key)
}

#' @rdname as-tsibble
as_tsibble.hts <- function(x, tz = "UTC", ...) {
  bts <- x$bts
  nodes <- x$nodes[-1]
  labels <- x$labels[-1]
  labels <- labels[-length(labels)]
  nr <- nrow(bts)
  chr_labs <- purrr::map2(
    labels, seq_along(labels), ~ .x[rep_nodes(nodes, level = .y)]
  )
  full_labs <- purrr::map(rev.default(chr_labs), ~ rep(., each = nr))
  names(full_labs) <- names(labels)

  tbl <- mts2tbl(bts, tz = tz) %>% 
    dplyr::select(time, value, key)
  colnames(tbl)[3] <- deparse(substitute(x))
  out_hts <- dplyr::bind_cols(tbl, full_labs)
  # this would work around the special character issue in headers for parse()
  sym_key <- syms(colnames(out_hts)[c(3, ncol(out_hts))])
  as_tsibble(out_hts, index = time, sym_key)
}

#' @rdname as-tsibble
as_tsibble.gts <- function(x, tz = "UTC", ...) {
  bts <- x$bts
  group <- x$group[-1, , drop = FALSE]
  group <- group[-nrow(group), , drop = FALSE]
  labels <- x$labels
  if (is_empty(labels)) {
    abort("I don't know how to handle a grouped time series with no group.")
  }
  seq_labs <- seq_along(labels)
  grp_label <- purrr::map(seq_labs, ~ labels[[.]][group[., ]])
  chr_labs <- vector(mode = "list", length = length(labels))
  for (i in seq_labs) {
    chr_labs[[i]] <- purrr::map_chr(
      strsplit(grp_label[[i]], split = "/", fixed = TRUE), ~ .[2]
    )
  }
  nr <- nrow(bts)
  full_labs <- purrr::map(chr_labs, ~ rep(., each = nr))
  names(full_labs) <- names(labels)

  tbl <- mts2tbl(bts, tz = tz) %>% 
    dplyr::select(time, value)
  colnames(tbl)[2] <- deparse(substitute(x))
  out_hts <- dplyr::bind_cols(tbl, full_labs)
  # this would work around the special character issue in headers for parse()
  sym_key <- syms(colnames(out_hts)[c(3, ncol(out_hts))])
  as_tsibble(out_hts, index = time, sym_key)
}

mts2tbl <- function(x, tz = "UTC") {
  tbl <- dplyr::bind_cols(
    time = time2date(x, tz = tz), tibble::as_tibble(x)
  )
  tidyr::gather(tbl, key = key, value = value, -time)
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
