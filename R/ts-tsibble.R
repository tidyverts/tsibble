globalVariables(c("time"))

#' @rdname as-tsibble
#' @param tz Time zone. May be useful when a `ts` object is more frequent than
#' daily.
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
#' @param gather TRUE gives a "long" data form, otherwise as "wide" as `x`.
#'
#' @examples
#' # coerce mts to tsibble
#' z <- ts(matrix(rnorm(300), 100, 3), start = c(1961, 1), frequency = 12)
#' as_tsibble(z)
#' as_tsibble(z, gather = FALSE)
#'
#' @export
as_tsibble.mts <- function(x, tz = "UTC", gather = TRUE, ...) {
  if (gather) {
    long_tbl <- gather_ts(x, tz = tz)
    return(as_tsibble(long_tbl, key = id(key), index = index, validate = FALSE))
  } else {
    wide_tbl <- bind_time(x, tz = tz)
    as_tsibble(wide_tbl, index = index, validate = FALSE)
  }
}

#' @rdname as-tsibble
#'
#' @examples
#' # coerce hts from the "hts" package to tsibble
#' if (!requireNamespace("hts", quietly = TRUE)) {
#'   stop("Please install the hts package to run these following examples.")
#' }
#' as_tsibble(hts::htseg1)
#' as_tsibble(hts::htseg2)
#' @export
as_tsibble.hts <- function(x, tz = "UTC", ...) {
  full_labs <- extract_labels(x)
  tbl <- gather_ts(x, tz = tz) %>% 
    dplyr::select(index, value)
  tbl_hts <- dplyr::bind_cols(tbl, full_labs)
  # this would work around the special character issue in headers for parse()
  lst_key <- list(syms(colnames(tbl_hts)[3:ncol(tbl_hts)]))
  as_tsibble(tbl_hts, key = lst_key, index = index, validate = FALSE)
}

# as_tsibble.gts <- function(x, tz = "UTC", ...) {
#   bts <- x$bts
#   group <- x$group[-1, , drop = FALSE]
#   group <- group[-nrow(group), , drop = FALSE]
#   labels <- x$labels
#   if (is_empty(labels)) {
#     abort("I don't know how to handle a grouped time series with no group.")
#   }
#   seq_labs <- seq_along(labels)
#   grp_label <- purrr::map(seq_labs, ~ labels[[.]][group[., ]])
#   chr_labs <- vector(mode = "list", length = length(labels))
#   for (i in seq_labs) {
#     chr_labs[[i]] <- purrr::map_chr(
#       strsplit(grp_label[[i]], split = "/", fixed = TRUE), ~ .[2]
#     )
#   }
#   nr <- nrow(bts)
#   full_labs <- purrr::map(chr_labs, ~ rep(., each = nr))
#   names(full_labs) <- names(labels)
#
#   tbl <- gather_ts(bts, tz = tz) %>% 
#     dplyr::select(time, value)
#   colnames(tbl)[2] <- deparse(substitute(x))
#   out_hts <- dplyr::bind_cols(tbl, full_labs)
#   # this would work around the special character issue in headers for parse()
#   sym_key <- syms(colnames(out_hts)[c(3, ncol(out_hts))])
#   as_tsibble(out_hts, index = time, sym_key)
# }

as_tibble.gts <- function(x, ...) {
  tibble::as_tibble(x$bts)
}

bind_time <- function(x, tz = "UTC") {
  dplyr::bind_cols(
    index = time_to_date(x, tz = tz), tibble::as_tibble(x, validate = FALSE)
  )
}

gather_ts <- function(x, tz = "UTC") {
  tbl <- bind_time(x, tz = tz)
  tidyr::gather(tbl, key = "key", value = "value", -index)
}

# recursive function to repeat nodes for hts
rep_nodes <- function(x, level = 1L, index = seq_along(x[[level]])) {
  if (has_length(x[[1]], 1)) {
    x <- x[-1]
  }
  index <- rep.int(index, x[[level]])
  if (has_length(x, level)) {
    return(index)
  } else {
    rep_nodes(x, level + 1L, index)
  }
}

extract_labels <- function(x) {
  UseMethod("extract_labels")
}

extract_labels.hts <- function(x) {
  nodes <- x$nodes
  old_labels <- x$labels
  btm_labels <- old_labels[[length(old_labels)]]
  new_labels <- old_labels[-c(1, length(old_labels))]
  chr_labs <- purrr::map2(
    new_labels, seq_along(new_labels), ~ .x[rep_nodes(nodes, level = .y)]
  )
  nr <- nrow(x$bts)
  full_labs <- purrr::map(chr_labs, ~ rep(., each = nr))
  full_labs <- c(full_labs, list(rep(btm_labels, each = nr)))
  names(full_labs) <- names(old_labels[-1])
  rev.default(full_labs)
}
