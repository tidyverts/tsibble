globalVariables(c("key", "value"))

#' Create a tsibble object
#'
#' @param ... A set of name-value pairs.
#' @param key Unquoted variable(s) indicating the key variables for tsibble, 
#'    used in combination with `key_vars()`.
#' @param index An unquoted variable indicating the time index variable.
#'
#' @return A tsibble object.
#' @author Earo Wang
#' @rdname tsibble
#' @seealso [tibble::tibble]
#'
#' @examples
#'    ts_df <- tsibble(
#'      Date = rep(seq(as.Date("2017-01-01"), by = 1, length = 10), 2),
#'      Group = rep(c("A", "B"), each = 10),
#'      Value = rnorm(20),
#'      key = key_vars(Group), index = Date
#'    ) 
#'    print(ts_df)
#'
#' @export
tsibble <- function(..., key = key_vars(), index) {
  index <- enquo(index)
  tsibble_(lst(...), key = key, index = index)
}

#' Coerce to a tsibble object
#'
#' @param x Other objects to be coerced to tsibble.
#' @param ... Other arguments to be passed.
#'
#' @return A tsibble object.
#' @author Earo Wang
#' @seealso [tibble::as_tibble]
#' @rdname as-tsibble
#'
#' @examples
#'    # coerce data.frame to tsibble
#'    # as_tsibble(tidypkgs, key = key_vars(package), index = date) 
#'
#'    # coerce ts to tsibble
#'    as_tsibble(AirPassengers)
#'    as_tsibble(sunspot.year)
#'    as_tsibble(sunspot.month)
#'    as_tsibble(austres)
#'
#' @export
as_tsibble <- function(x, ...) {
  UseMethod("as_tsibble")
}

#' @rdname as-tsibble
#' @param key Unquoted variable(s) indicating the key variables for tsibble, 
#'    used in combination with `key_vars()`.
#' @param index An unquoted variable indicating the time index variable
#' @export
as_tsibble.default <- function(x, key = key_vars(), index, ...) {
  index <- enquo(index)
  tsibble_(x, key = key, index = index)
}

#' @rdname as-tsibble
#' @param tz Time zone.
#' @export
as_tsibble.ts <- function(x, tz = "UTC", ...) {
  idx <- time2date(x, tz = tz)
  value <- unclass(x) # rm its ts class

  output <- tsibble(
    time = idx, value = value, 
    key = key_vars(), index = time,
  )
  colnames(output)[2] <- deparse(substitute(x))
  return(output)
}

#' @rdname as-tsibble
#' @export
as_tsibble.mts <- function(x, tz = "UTC", ...) {
  long_tbl <- mts2tbl(x, tz = tz)
  colnames(long_tbl)[3] <- deparse(substitute(x))
  as_tsibble.default(long_tbl, key = key_vars(key), index = time)
}

#' @rdname as-tsibble
#' @export
as_tsibble.hts <- function(x, tz = "UTC", ...) {
  bts <- x$bts
  nodes <- x$nodes[-1]
  labels <- x$labels[-1]
  labels <- labels[-length(labels)]
  nr <- nrow(bts)
  chr_labs <- map(seq_along(labels), ~ rep_nodes(labels[[.]], nodes, level = .))
  full_labs <- map(rev.default(chr_labs), ~ rep(., each = nr))
  names(full_labs) <- names(labels)

  tbl <- mts2tbl(bts, tz = tz) %>% 
    dplyr::select(time, value, key)
  colnames(tbl)[3] <- deparse(substitute(x))
  out_hts <- bind_cols(tbl, full_labs)
  # this would work around the special character issue in headers for parse()
  sym_key <- syms(colnames(out_hts)[c(3, ncol(out_hts))])
  chr_key <- paste(sym_key, collapse = ":")
  as_tsibble.default(
    out_hts, key = key_vars(!!parse_expr(chr_key), "|"), index = time
  )
}

#' @rdname as-tsibble
#' @export
as_tsibble.gts <- function(x, tz = "UTC", ...) {
  bts <- x$bts
  group <- x$group[-1, , drop = FALSE]
  group <- group[-nrow(group), , drop = FALSE]
  labels <- x$labels
  if (is_empty(labels)) {
    abort("I don't know how to handle a grouped time series with no group.")
  }
  seq_labs <- seq_along(labels)
  grp_label <- map(seq_labs, ~ labels[[.]][group[., ]])
  chr_labs <- vector(mode = "list", length = length(labels))
  for (i in seq_labs) {
    chr_labs[[i]] <- map_chr(
      strsplit(grp_label[[i]], split = "/", fixed = TRUE), ~ .[2]
    )
  }
  nr <- nrow(bts)
  full_labs <- map(chr_labs, ~ rep(., each = nr))
  names(full_labs) <- names(labels)

  tbl <- mts2tbl(bts, tz = tz) %>% 
    dplyr::select(time, value)
  colnames(tbl)[2] <- deparse(substitute(x))
  out_hts <- bind_cols(tbl, full_labs)
  # this would work around the special character issue in headers for parse()
  sym_key <- syms(colnames(out_hts)[c(3, ncol(out_hts))])
  chr_key <- paste(sym_key, collapse = ":")
  as_tsibble.default(
    out_hts, key = key_vars(!!parse_expr(chr_key), "*"), index = time
  )
}

## tsibble is a special class of tibble that handles temporal data. It
## requires a sequence of time index to be unique across every identifier.
## The way to distinguish univariate or multivariate series is based on "key".
## Although the "index" arg is possible to automate the detection of time
## objects, it would fail when tsibble contain multiple time objects. Better
## to let user specify.
tsibble_ <- function(..., key = key_vars(), index) {
  tbl <- as_tibble(...)
  cls_tbl <- class(tbl)

  # check time index type
  eval_idx <- eval_tidy(index, data = tbl)
  cls_idx <- class(eval_idx)
  if (is_false(any(cls_idx %in% support_cls()))) {
    abort(paste(cls_idx, "class is not supported."))
  }

  pkey <- parse_key(tbl, key = key)
  if (class(pkey) == "key_ts" && is_empty(pkey)) { # univariate
    if (anyDuplicated(eval_idx) != 0) {
      abort("'index' must contain unique time index.")
    }
    tbl_interval <- pull_interval(eval_idx)
    cls_tbl <- c("tbl_ts", cls_tbl)
  } else {
    if (class(pkey) == "key_ts") { # multivariate
        tbl_nest <- tbl %>%
          group_by(!!!pkey) %>%
          nest()
      cls_tbl <- c("tbl_ts", cls_tbl)
    } else if (class(pkey) == "key_hts") {
        tbl_nest <- tbl %>% 
          group_by(!!!pkey[1]) %>%  # the bottom level group
          nest()
      cls_tbl <- c("tbl_hts", "tbl_gts", "tbl_ts", cls_tbl)
    } else { # key_gts
        tbl_nest <- tbl %>% 
          group_by(!!!pkey) %>%  # the comb of all the groups
          nest()
      cls_tbl <- c("tbl_gts", "tbl_ts", cls_tbl)
    }
    eval_lst_idx <- tbl_nest$data %>%
      map(function(data) eval_tidy(index, data = data))
    lst_interval <- vapply(eval_lst_idx, 
      function(x) gen_interval(x), numeric(1))
    check_idx <- map_int(eval_lst_idx, anyDuplicated)
    if (!is_constant(lst_interval)) {
      abort("Each key variable must have the same time interval in 'tsibble'.")
    } else if (any(check_idx != 0)) {
      abort("'index' must contain unique time index for each key variable.")
    } else {
      tbl_interval <- pull_interval(eval_lst_idx[[1]])
    }
  } 

  attr(tbl, "key") <- pkey
  attr(tbl, "index") <- index
  attr(tbl, "interval") <- tbl_interval
  structure(tbl, class = cls_tbl)
}

get_key <- function(tbl_ts) {
  attr(tbl_ts, "key")
}

get_interval <- function(tbl_ts) {
  attr(tbl_ts, "interval")
}

get_index <- function(tbl_ts) {
  attr(tbl_ts, "index")
}

#' @export
print.tbl_ts <- function(x, ...) {
  int_x <- get_interval(x)
  grp_var <- get_key(x)
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

#' @param ... Unquoted variable(s).
#' @rdname tsibble
#' @keywords internal
#' @export
key_vars <- function(...) {
  return(quos(...))
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
  long_tbl <- tbl %>% 
    gather(key = key, value = value, -time)
  return(long_tbl)
}

# recursive function to repeat nodes for hts
rep_nodes <- function(labels, nodes, level) {
  labels <- rep(labels, nodes[[level]])
  if (level == length(nodes)) {
    return(labels)
  } else {
    return(rep_nodes(labels, nodes, level + 1))
  }
}

