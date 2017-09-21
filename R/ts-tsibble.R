globalVariables(c("time"))

#' @rdname as-tsibble
#' @param tz Time zone.
#' 
#' @examples
#' # coerce ts to tsibble
#' as_tsibble(AirPassengers)
#' as_tsibble(sunspot.year)
#' as_tsibble(sunspot.month)
#' as_tsibble(austres)
#'
#' @export
as_tsibble.ts <- function(x, tz = "UTC", ...) {
  idx <- time_to_date(x, tz = tz)
  value <- unclass(x) # rm its ts class
  tbl <- tibble::tibble(index = idx, value = value)
  as_tsibble(tbl, index = index, validate = FALSE)
}

#' @rdname as-tsibble
#'
#' @examples
#' # coerce mts to tsibble
#' z <- ts(matrix(rnorm(300), 100, 3), start = c(1961, 1), frequency = 12)
#' as_tsibble(z)
#'
#' @export
as_tsibble.mts <- function(x, tz = "UTC", ...) {
  long_tbl <- gather_ts(x, tz = tz)
  as_tsibble(long_tbl, index = index, key, validate = FALSE)
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

  tbl <- gather_ts(bts, tz = tz) %>% 
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

  tbl <- gather_ts(bts, tz = tz) %>% 
    dplyr::select(time, value)
  colnames(tbl)[2] <- deparse(substitute(x))
  out_hts <- dplyr::bind_cols(tbl, full_labs)
  # this would work around the special character issue in headers for parse()
  sym_key <- syms(colnames(out_hts)[c(3, ncol(out_hts))])
  as_tsibble(out_hts, index = time, sym_key)
}

gather_ts <- function(x, tz = "UTC") {
  tbl <- dplyr::bind_cols(
    index = time_to_date(x, tz = tz), tibble::as_tibble(x)
  )
  tidyr::gather(tbl, key = "key", value = "value", -index)
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
