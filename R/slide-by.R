slide_by <- function(.data, .size = 1, .step = 1) {
  UseMethod("slide_by")
}

slide_by.tbl_ts <- function(.data, .size = 1, .step = 1) {
  key_data <- key_data(.data)
  key_data[[".rows"]] <- as_list_of(map(key_rows(.data),
    slider2, .size = .size, .step = .step))
  new_roll_tsibble(.data, key_data = key_data)
}

slide_by.grouped_ts <- function(.data, .size = 1, .step = 1) {
  warn("Ignoring grouping structure.")
  slide_by.tbl_ts(.data, .size, .step)
}

slider2 <- function(x, .size = 1, .step = 1) {
  stopifnot(.size > 0 && .step > 0)
  len_x <- vec_size(x)
  lst_idx <- seq.int(1L, len_x - .size + 1, by = .step)
  as_list_of(map(lst_idx, function(idx) x[idx:(idx + .size - 1)]))
}

new_roll_tsibble <- function(.data, key_data) {
  attr(.data, "key") <- key_data
  new_tsibble(.data, class = "tbl_rts")
}
