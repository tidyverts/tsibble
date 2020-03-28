# nocov start
.onLoad <- function(...) {
  s3_register("dplyr::filter", "tbl_ts")
  s3_register("dplyr::distinct", "tbl_ts")
  # s3_register("dplyr::group_split", "tbl_ts")
  # s3_register("dplyr::group_split", "grouped_ts")
  # s3_register("dplyr::group_trim", "grouped_ts")

  s3_register("tidyr::gather", "tbl_ts")
  s3_register("tidyr::spread", "tbl_ts")
  s3_register("tidyr::fill", "grouped_ts")
  s3_register("tidyr::fill", "tbl_ts")
  s3_register("tidyr::drop_na", "tbl_ts")
  s3_register("tidyr::drop_na", "grouped_ts")
  s3_register("tidyr::nest", "tbl_ts")
  s3_register("tidyr::nest", "grouped_ts")
  s3_register("tidyr::unnest", "tbl_ts")

  s3_register("ggplot2::scale_type", "yearquarter")
  s3_register("ggplot2::scale_type", "yearmonth")
  s3_register("ggplot2::scale_type", "yearweek")
}
# nocov end
