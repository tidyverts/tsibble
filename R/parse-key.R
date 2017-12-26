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

# this returns a vector of groups/key characters
flatten_key <- function(lst_keys) {
  if (is.null(lst_keys)) {
    lst_keys <- list()
  }
  purrr::map_chr(flatten(lst_keys), quo_text2)
}

# update by character names
update_key <- function(x, y) { # y = a vector of flat characters
  old_chr <- flatten_key(x)
  new_idx <- sort(match(y, old_chr)) # nesting goes first
  new_chr <- old_chr[new_idx]
  old_lgl <- rep(is_nest(x), purrr::map(x, length))
  new_lgl <- old_lgl[new_idx]

  if (any(old_lgl)) {
    return(c(list(syms(new_chr[new_lgl])), syms(new_chr[!new_lgl])))
  } else {
    syms(new_chr[!new_lgl])
  }
}

# update by matching positions
update_key2 <- function(x, rhs, lhs) { # rhs is quos
  old_chr <- flatten_key(x)
  new_idx <- match(old_chr, rhs)
  new_chr <- lhs[new_idx]
  na_idx <- which(is.na(new_idx), useNames = FALSE)
  new_chr[na_idx] <- old_chr[na_idx]
  lgl <- rep(is_nest(x), purrr::map(x, length))

  if (any(lgl)) {
    return(c(list(syms(new_chr[lgl])), syms(new_chr[!lgl])))
  } else {
    syms(new_chr[!lgl])
  }
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
  unname(purrr::map_lgl(lst_syms, is_list)) # expected to be a list not call
}

parse_key <- function(lst_keys) {
  if (is_empty(lst_keys)) {
    return(lst_keys)
  }
  # purrr::map(lst_keys, ~ flatten_nest(.[[-1]]))
  purrr::map(lst_keys, flatten_nest)
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

