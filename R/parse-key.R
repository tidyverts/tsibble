#' Return key variables
#'
#' `key()` returns a list of symbols; `key_vars()` gives a character vector.
#'
#' @param x A data frame.
#'
#' @rdname key
#' @examples
#' # A single key for pedestrian data ----
#' key(pedestrian)
#' key_vars(pedestrian)
#'
#' # Nested and crossed keys for tourism data ----
#' key(tourism)
#' key_vars(tourism)
#' @export
key <- function(x) {
  UseMethod("key")
}

#' @export
key.default <- function(x) {
  abort(sprintf("Can't find the attribute key in class %s", class(x)[1]))
}

#' @export
key.tbl_ts <- function(x) {
  attr(x, "key")
}

#' @export
`[[.key` <- function(x, i, j, ..., exact = TRUE) {
  NextMethod()
}

#' @export
`[.key` <- function(x, i, j, drop = FALSE) {
  NextMethod()
}

#' @rdname key
#' @export
key_vars <- function(x) {
  UseMethod("key_vars")
}

#' @export
key_vars.tbl_ts <- function(x) {
  key_flatten(key(x))
}

#' @rdname key
#' @export
#' @examples
#' # unkey() only works for a tsibble with 1 key size ----
#' sx <- pedestrian %>%
#'   filter(Sensor == "Southern Cross Station")
#' unkey(sx)
unkey <- function(x) {
  UseMethod("unkey")
}

#' @export
unkey.tbl_ts <- function(x) {
  nkey <- n_keys(x)
  if (nkey < 2 || nkey == NROW(x)) {
    attr(x, "key") <- structure(id(), class = "key")
    x
  } else {
    abort("Can't apply to a tsibble of more than 1 key size.")
  }
}

#' Compute sizes of key variables
#'
#' @param x A data frame.
#'
#' @examples
#' key_size(pedestrian)
#' n_keys(pedestrian)
#'
#' @rdname key-size
#' @export
key_size <- function(x) {
  UseMethod("key_size")
}

#' @export
key_size.tbl_ts <- function(x) {
  idx <- key_indices(x)
  out <- as.integer(rowsum.default(rep_len(1L, length(idx)), idx, reorder = FALSE))
  if (has_length(out, 1)) {
    NROW(x)
  } else {
    out
  }
}

#' @rdname key-size
#' @export
n_keys <- function(x) {
  UseMethod("n_keys")
}

#' @export
n_keys.tbl_ts <- function(x) {
  key <- key_vars(x)
  if (is_empty(key)) {
    1L
  } else {
    NROW(distinct(ungroup(as_tibble(x)), !!! syms(key)))
  }
}

#' @rdname key-size
#' @export
key_indices <- function(x) {
  UseMethod("key_indices")
}

#' @export
key_indices.tbl_ts <- function(x) {
  key_vars <- key_vars(x)
  grped_key <- grouped_df(x, key_vars)
  group_indices(grped_key)
}

key_distinct <- function(x) { # x = a list of keys (symbols)
  if (is_empty(x)) return(x)
  reconstruct_key(x, ~ map(., ~ .[[1]]), ~ .)
}

grp_drop <- function(x, index2 = NULL) {
  x <- setdiff(x, index2)
  len <- length(x)
  new_grps <- x[-len] # drop one grouping level
  if (is_empty(new_grps)) {
    id()
  } else {
    syms(new_grps)
  }
}

grp_update <- function(x, .vars) {
  chr <- intersect(group_vars(x), .vars)
  if (is_empty(chr)) {
    id()
  } else {
    syms(chr)
  }
}

# this returns a vector of groups/key characters
key_flatten <- function(x) {
  if (is.null(x)) {
    x <- id()
  }
  unname(map_chr(flatten(x), as_string))
}

#' Change/update key variables for a given `tbl_ts`
#'
#' @param .data A `tbl_ts`.
#' @param ... Expressions used to construct the key:
#' * unspecified: drop every single variable from the old key.
#' * `|` and `,` for nesting and crossing factors.
#' @inheritParams as_tsibble
#'
#' @examples
#' # tourism could be identified by Region and Purpose ----
#' tourism %>%
#'   key_update(Region, Purpose)
#' @export
key_update <- function(.data, ..., validate = TRUE) {
  not_tsibble(.data)
  quos <- enexprs(...)
  key <- validate_key(.data, quos)
  if (validate) {
    build_tsibble(
      .data, key = key, index = !! index(.data), index2 = !! index2(.data),
      groups = groups(.data), regular = is_regular(.data), validate = validate,
      ordered = is_ordered(.data), interval = interval(.data)
    )
  } else {
    build_tsibble_meta(
      .data, key = key, index = !! index(.data), index2 = !! index2(.data),
      groups = groups(.data), regular = is_regular(.data),
      ordered = is_ordered(.data), interval = interval(.data)
    )
  }
}

# remove some variables from the key
key_remove <- function(.data, .vars, validate = TRUE) {
  old_key <- key(.data)
  old_chr <- key_flatten(old_key)
  key_idx <- which(.vars %in% old_chr)
  key_vars <- .vars[key_idx]
  old_lgl <- FALSE
  if (!is_empty(old_key)) {
    old_lgl <- rep(is_nest(old_key), map(old_key, length))
  }
  new_lgl <- old_lgl[match(key_vars, old_chr)]

  new_nest_key <- syms(key_vars[new_lgl])
  new_cross_key <- syms(key_vars[!new_lgl])
  new_key <- reconstruct_key(
    old_key,
    ~ map(., ~ .[flatten(.) %in% new_nest_key]),
    ~ .[. %in% new_cross_key]
  )
  key_update(.data, !!! new_key, validate = validate)
}

key_rename <- function(.data, .vars) {
  # key (key of the same size (bf & af))
  old_key <- key(.data)
  if (is_empty(old_key)) return(id())
  old_chr <- key_flatten(old_key)
  .vars <- .vars[match(old_chr, .vars)]
  names <- names(.vars)
  new_chr <- names[.vars %in% old_chr]
  lgl <- FALSE
  if (!is_empty(old_key)) {
    lgl <- rep(is_nest(old_key), map(old_key, length))
  }
  new_nest_key <- syms(new_chr[lgl])
  reconstruct_key(
    old_key,
    ~ `[<-`(., list(new_nest_key)),
    ~ `[<-`(., syms(new_chr[!lgl]))
  )
}

grp_rename <- function(.data, .vars) {
  names <- names(.vars)
  old_grp_chr <- group_vars(.data)
  new_grp_chr <- names[.vars %in% old_grp_chr]
  syms(new_grp_chr)
}

# The function takes a nested key/group str, i.e. `|` sym
# it also handles `-` and `:` and `c`
# return: a list of symbols (if (is_nest) a list of list)
validate_key <- function(data, key) {
  if (inherits(key, "key")) return(key)
  cn <- names(data)
  keys <- parse_key(key)
  if (is_empty(keys)) return(keys)

  reconstruct_key(
    keys,
    ~ map(., ~ syms(validate_vars(flatten(.), cn))),
    ~ syms(validate_vars(., cn))
  )
}

reconstruct_key <- function(key, nesting, crossing) {
  nest_lgl <- is_nest(key)
  crossing_fun <- as_function(crossing)
  cross_vars <- crossing_fun(key[!nest_lgl])
  ttl_len <- length(cross_vars) + sum(nest_lgl)
  valid_key <- vector(mode = "list", length = ttl_len)
  cross_idx <- seq_len(ttl_len)
  if (any(nest_lgl)) {
    nest_idx <- which(nest_lgl)
    nesting_fun <- as_function(nesting)
    nest_vars <- nesting_fun(key[nest_lgl])
    valid_key[nest_idx] <- nest_vars
    cross_idx <- cross_idx[-nest_idx]
  }
  valid_key[cross_idx] <- cross_vars
  # if there's only one variable in nesting lists --> flatten
  len_1 <- map_lgl(valid_key, ~ is_list(.) && has_length(., 1))
  valid_key[len_1] <- flatten(valid_key[len_1])
  purrr::compact(valid_key)
}

is_nest <- function(lst_syms) {
  if (is_empty(lst_syms)) {
    FALSE
  } else {
    unname(map_lgl(lst_syms, is_list)) # expected to be a list not call
  }
}

parse_key <- function(x) {
  if (is_empty(x)) {
    id()
  } else {
    map(x, flatten_nest)
  }
}

# interpret a nested calls A | B | C
flatten_nest <- function(key) { # call
  if (!is_bare_list(key) && has_length(key, 2) && key[[1]] != sym("-")) {
    return(flatten_nest(key[[2]]))
  }
  if (is_bare_list(key, 2) || length(key) < 3)
    return(key)
  op <- key[[1]]
  x <- key[[2]]
  y <- key[[3]]
  if (op == sym("|")) {
    c(flatten_nest(x), flatten_nest(y))
  } else if (op == sym("/")) {
    c(flatten_nest(y), flatten_nest(x))
  } else if (op == sym("-")) {
    c(flatten_nest(x), expr(-flatten_nest(y)))
  } else {
    key
  }
}

