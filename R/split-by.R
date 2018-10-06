#' Split a data frame into a list of subsets by variables
#'
#' @param x A data frame.
#' @param ... A list of unquoted variables, separated by commas, to split a dataset.
#' @rdname split-by
#' @export
#' @examples
#' pedestrian %>% 
#'   split_by(Sensor)
split_by <- function(x, ...) {
  UseMethod("split_by")
}

#' @export
split_by.tbl_ts <- function(x, ...) {
  quos <- enquos(...)
  if (is_empty(quos)) return(list(x))

  vars_split <- validate_vars(quos, names(x))
  grped_df <- grouped_df(x, vars = vars_split)
  res <- split(x, group_indices(grped_df))
  unname(res)
}

#' @export
split_by.data.frame <- split_by.tbl_ts

#' @export
split_by.tbl_df <- split_by.tbl_ts
