# cr: dplyr::across() and DataMask
context_tsibble_env <- new_environment()

poke_tsibble_mask <- function(mask) {
  old <- context_tsibble_env[["..tsibble_mask"]]
  context_tsibble_env[["..tsibble_mask"]] <- mask
  old
}

peek_tsibble_mask <- function() {
  context_tsibble_env[["..tsibble_mask"]] %||% 
    abort("No tsibble data mask registered.")
}

scoped_tsibble_mask <- function(mask, frame = caller_env()) {
  old_mask <- poke_tsibble_mask(mask)

  expr <- call2(on.exit, expr(poke_tsibble_mask(!!old_mask)), add = TRUE)
  eval_bare(expr, frame)
}

new_tsibble_data_mask <- function(data) {
  scoped_tsibble_mask(data, caller_env(n = 2))
  data
}
