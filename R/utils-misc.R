dock_id <- function(ns = NULL) {

  res <- "dock"

  if (is.null(ns)) {
    return(res)
  }

  ns(res)
}

block_panel_id <- function(block_id, dock_id = NULL) {

  stopifnot(is.character(block_id), has_length(block_id))

  res <- paste0("block-", block_id)

  if (is.null(dock_id)) {
    return(res)
  }

  stopifnot(is_string(dock_id))

  paste0(dock_id, "-", res)
}

is_block_panel_id <- function(x) {
  grepl("^block-", x)
}
