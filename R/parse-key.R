reduce_key <- function(lst_keys) {
  if (is_empty(lst_keys)) {
    return(lst_keys)
  }
  nest_lgl <- is_nest(lst_keys)
  comb_keys <- lst_keys[!nest_lgl]
  if (any(nest_lgl)) {
    nest_keys <- purrr::map(lst_keys[nest_lgl], ~ .[[1]])
    comb_keys <- c(nest_keys, comb_keys)
  }
  quos_auto_name(comb_keys)
}

validate_key <- function(data, ...) {
  col_names <- colnames(data)
  keys <- parse_key(...)
  nest_lgl <- is_nest(keys)
  valid_keys <- syms(dplyr::select_vars(col_names, !!! keys[!nest_lgl]))
  if (any(nest_lgl)) {
    nest_keys <- purrr::map(
      keys[nest_lgl], 
      ~ syms(dplyr::select_vars(col_names, !!! flatten(.)))
    )
    valid_keys <- c(nest_keys, valid_keys)
  }
  valid_keys
}

is_nest <- function(lst_syms) {
  purrr::map_lgl(lst_syms, is_list) # expected to be a list not call
}

parse_key <- function(...) {
  key_quos <- quos(...) # quosures
  purrr::map(key_quos, ~ flatten_nest(.[[-1]]))
}

# A | B | C nested calls
flatten_nest <- function(key) { # call
  if (length(key) == 2 && key[[1]] != sym("-")) {
    return(flatten_nest(key[[2]]))
  }
  if (length(key) < 3) 
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

