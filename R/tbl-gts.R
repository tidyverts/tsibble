mts2tbl <- function(x, tz = "UTC") {
  tbl <- dplyr::bind_cols(
    time = time2date(x, tz = tz), tibble::as_tibble(x)
  )
  tidyr::gather(tbl, key = key, value = value, -time)
}

# recursive function to repeat nodes for hts
rep_nodes <- function(nodes, index = seq_along(nodes[[level]]), level = 1L) {
  index <- rep.int(index, nodes[[level]])
  if (level == length(nodes)) {
    return(index)
  } else {
    rep_nodes(nodes, index, level + 1L)
  }
}


