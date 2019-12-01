# cr: dplyr::across() and DataMask
context_tsibble_env <- new_environment()

poke_tsibble_mask <- function(mask) {
  old <- context_tsibble_env[["..tsibble_mask"]]
  context_tsibble_env[["..tsibble_mask"]] <- mask
  old
}

peek_tsibble_mask <- function() {
  context_tsibble_env[["..tsibble_mask"]] %||% 
    abort(sprintf("`%s` should only be called in `mutate()` for a tsibble.",
      deparse(sys.call(-1))))
}

scoped_tsibble_mask <- function(mask, frame = caller_env()) {
  old_mask <- poke_tsibble_mask(mask)

  expr <- call2(on.exit, expr(poke_tsibble_mask(!!old_mask)), add = TRUE)
  eval_bare(expr, frame)
}

TsibbleDataMask <- R6Class("TsibbleDataMask",
  public = list(
    initialize = function(data) {
      frame <- caller_env(n = 2)
      scoped_vars(names(data), frame)
      scoped_tsibble_mask(self, frame)

      private$data <- data
    },

    keyed_fn = function(var, data, fn, ...) {
      idx_chr <- index_var(data)
      grped_df <- new_grouped_df(data, groups = key_data(data))
      if (n_keys(data) == 1) {
        grped_df <- ungroup(grped_df)
      }
      res_df <- mutate(grped_df, 
        !!idx_chr := fn(!!enquo(var), ..., order_by = !!index(data)))
      res_df[[idx_chr]]
    },

    tsibble_data = function() {
      private$data
    }
  ),

  private = list(data = NULL)
)
