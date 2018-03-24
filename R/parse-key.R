#' Split data into a list by variables
#'
#' @param x A `tbl_ts`.
#' @param ... Unquoted variables to split by.
#' @rdname split-by
#' @export
split_by <- function(x, ...) {
  UseMethod("split_by")
}

#' @rdname split-by
#' @export
#' @examples
#' pedestrian %>% 
#'   split_by(Sensor)
split_by.tbl_ts <- function(x, ...) {
  quos <- enquos(...)
  if (is_empty(quos)) {
    return(list(x))
  }
  vars_split <- as.character(validate_key(x, quos))
  idx <- attr(grouped_df(x, vars = vars_split), "indices")
  lapply(idx, function(idx) x[idx + 1, ])
}

reduce_key <- function(x) { # x = a list of keys (symbols)
  if (is_empty(x)) {
    return(x)
  }
  nest_lgl <- is_nest(x)
  comb_keys <- x[!nest_lgl]
  if (any(nest_lgl)) {
    nest_keys <- purrr::map(x[nest_lgl], ~ .[[1]])
    comb_keys <- c(nest_keys, comb_keys)
  }
  quos_auto_name(comb_keys)
}

drop_group <- function(x) {
  len <- length(x)
  new_grps <- x[-len] # drop one grouping level
  last_x <- dplyr::last(x)
  if (is_empty(new_grps)) {
    return(id())
  } else if (length(last_x) > 1) {
    return(c(new_grps, list(last_x[-length(last_x)])))
  } else {
    new_grps
  }
}

# this returns a vector of groups/key characters
flatten_key <- function(x) {
  if (is.null(x)) {
    x <- id()
  }
  purrr::map_chr(flatten(x), quo_text2)
}

key_update <- function(.data, ...) {
  quos <- enquos(...)
  key <- validate_key(.data, quos)
  build_tsibble(
    .data, key = key, index = !! index(.data), groups = groups(.data),
    regular = is_regular(.data), validate = TRUE, ordered = is_ordered(.data),
    interval = interval(.data)
  )
}

# drop some keys
key_reduce <- function(.data, .vars) {
  old_key <- key(.data)
  old_chr <- flatten_key(old_key)
  new_idx <- sort(match(.vars, old_chr)) # nesting goes first
  new_chr <- old_chr[new_idx]
  old_lgl <- rep(is_nest(old_key), purrr::map(old_key, length))
  new_lgl <- old_lgl[new_idx]

  new_key <- syms(new_chr[!new_lgl])
  if (any(old_lgl)) {
    new_key <- c(list(syms(new_chr[new_lgl])), new_key)
  }
  build_tsibble(
    .data, key = new_key, index = !! index(.data), groups = groups(.data),
    regular = is_regular(.data), validate = TRUE, ordered = is_ordered(.data),
    interval = interval(.data)
  )
}

# rename key
key_rename <- function(.data, ...) {
  quos <- enquos(...)
  old_key <- key(.data)
  old_chr <- flatten_key(old_key)
  rhs <- purrr::map_chr(quos, quo_get_expr)
  lhs <- names(rhs)
  new_idx <- match(old_chr, rhs)
  new_chr <- lhs[new_idx]
  na_idx <- which(is.na(new_idx), useNames = FALSE)
  new_chr[na_idx] <- old_chr[na_idx]
  lgl <- FALSE
  if (!is_empty(old_key)) {
    lgl <- rep(is_nest(old_key), purrr::map(old_key, length))
  }

  new_key <- syms(new_chr[!lgl])
  if (is_empty(new_chr)) {
    new_key <- id()
  } else if (any(lgl)) {
    new_key <- c(list(syms(new_chr[lgl])), syms(new_chr[!lgl]))
  }
  dat_key_pos <- match(old_chr, names(.data))
  names(.data)[dat_key_pos] <- new_chr
  build_tsibble(
    .data, key = new_key, index = !! index(.data),
    regular = is_regular(.data), validate = FALSE, 
    ordered = is_ordered(.data), interval = interval(.data)
  )
}

# The function takes a nested key/group str, i.e. `|` sym
# it also handles `-` and `:` and `c`
# return: a list of symbols (if (is_nest) a list of list)
validate_key <- function(data, key) {
  col_names <- colnames(data)
  keys <- parse_key(key)
  if (is_empty(keys)) {
    return(keys)
  }
  nest_lgl <- is_nest(keys)
  valid_keys <- syms(validate_vars(keys[!nest_lgl], col_names))
  if (any(nest_lgl)) {
    nest_keys <- purrr::map(
      keys[nest_lgl],
      ~ syms(validate_vars(flatten(.), col_names))
    )
    valid_keys <- c(nest_keys, valid_keys)
  }
  valid_keys
}

is_nest <- function(lst_syms) {
  if (is_empty(lst_syms)) {
    return(FALSE)
  }
  unname(purrr::map_lgl(lst_syms, is_list)) # expected to be a list not call
}

parse_key <- function(x) {
  if (is_empty(x)) {
    return(id())
  }
  # purrr::map(x, ~ flatten_nest(.[[-1]]))
  purrr::map(x, flatten_nest)
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
  } else if (op == sym("-")) {
    c(flatten_nest(x), expr(-flatten_nest(y)))
  } else {
    key
  }
}

