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
#' @export
as_tsibble.ts <- function(x, ..., tz = "UTC") {
  idx <- time_to_date(x, tz = tz)
  value <- as.numeric(x) # rm its ts class
  tbl <- tibble(index = idx, value = value)
  build_tsibble(tbl,
    key = NULL, index = index, ordered = TRUE, validate = FALSE
  )
}

#' @rdname as-tsibble
#' @param pivot_longer `TRUE` gives a "longer" form of the data, otherwise as is.
#' @param gather \lifecycle{defunct} Please use `pivot_longer` instead.
#'
#' @examples
#' # coerce mts to tsibble
#' z <- ts(matrix(rnorm(300), 100, 3), start = c(1961, 1), frequency = 12)
#' as_tsibble(z)
#' as_tsibble(z, pivot_longer = FALSE)
#' @export
as_tsibble.mts <- function(x, ..., tz = "UTC", pivot_longer = TRUE, 
                           gather = deprecated()) {
  if (!is_missing(gather)) {
    lifecycle::deprecate_stop("0.8.0", 
      "as_tsibble(gather = )", "as_tsibble(pivot_longer = )")
  }
  if (pivot_longer) {
    long_tbl <- pivot_longer_tsibble(x, tz = tz)
    build_tsibble(
      long_tbl,
      key = key, index = index, ordered = TRUE, validate = FALSE
    )
  } else {
    wide_tbl <- bind_time(x, tz = tz)
    build_tsibble(
      wide_tbl,
      key = NULL, index = index, ordered = TRUE, validate = FALSE
    )
  }
}

#' @keywords internal
#' @export
as_tsibble.msts <- function(x, ..., tz = "UTC", pivot_longer = TRUE) {
  if (NCOL(x) == 1) {
    as_tsibble.ts(x, ..., tz = tz)
  } else {
    as_tsibble.mts(x, ..., tz = tz, pivot_longer = pivot_longer)
  }
}

#' @rdname as-tsibble
#' @usage NULL
#' @export
as_tsibble.hts <- function(x, ..., tz = "UTC") {
  full_labs <- extract_labels(x)
  tbl <- pivot_longer_tsibble(x, tz = tz) %>%
    select(index, "value")
  tbl_hts <- bind_cols(tbl, full_labs)
  # this would work around the special character issue in headers for parse()
  key <- colnames(tbl_hts)[3:ncol(tbl_hts)]
  build_tsibble(tbl_hts,
    key = !!key, index = index, ordered = TRUE,
    validate = FALSE
  )
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
#   grp_label <- map(seq_labs, ~ labels[[.]][group[., ]])
#   chr_labs <- vector(mode = "list", length = length(labels))
#   for (i in seq_labs) {
#     chr_labs[[i]] <- map_chr(
#       strsplit(grp_label[[i]], split = "/", fixed = TRUE), ~ .[2]
#     )
#   }
#   nr <- nrow(bts)
#   full_labs <- map(chr_labs, ~ rep(., each = nr))
#   names(full_labs) <- names(labels)
#
#   tbl <- pivot_longer_tsibble(bts, tz = tz) %>%
#     dplyr::select(time, value)
#   colnames(tbl)[2] <- deparse(substitute(x))
#   out_hts <- dplyr::bind_cols(tbl, full_labs)
#   # this would work around the special character issue in headers for parse()
#   sym_key <- syms(colnames(out_hts)[c(3, ncol(out_hts))])
#   as_tsibble(out_hts, index = time, sym_key)
# }

as_tibble.gts <- function(x, ...) {
  as_tibble(x$bts)
}

bind_time <- function(x, tz = "UTC") {
  bind_cols(index = time_to_date(x, tz = tz), as_tibble(x))
}

pivot_longer_tsibble <- function(x, tz = "UTC") {
  idx <- time_to_date(x)
  key <- colnames(x)
  res <- 
    tibble(
      "index" := rep(idx, NCOL(x)), 
      "key" := rep(key, each = NROW(x))
    )
  mutate(res, "value" := c(x))
}

# recursive function to repeat nodes for hts
rep_nodes <- function(x, level = 1L, index = seq_along(x[[level]])) {
  if (has_length(x[[1]], 1)) {
    x <- x[-1]
  }
  index <- rep.int(index, x[[level]])
  if (has_length(x, level)) {
    index
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
  chr_labs <- map2(
    new_labels, seq_along(new_labels), ~ .x[rep_nodes(nodes, level = .y)]
  )
  nr <- nrow(x$bts)
  full_labs <- map(chr_labs, ~ rep(., each = nr))
  full_labs <- c(full_labs, list(rep(btm_labels, each = nr)))
  names(full_labs) <- names(old_labels[-1])
  full_labs
}
