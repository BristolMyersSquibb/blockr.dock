block_panel_id <- function(block_id, dock_id = NULL) {

  if (is_board(block_id)) {
    block_id <- board_blocks(block_id)
  }

  if (is_blocks(block_id)) {
    block_id <- names(block_id)
  }

  stopifnot(is.character(block_id))

  if (!length(block_id)) {
    return(character())
  }

  res <- paste0("block-", block_id)

  if (is.null(dock_id)) {
    return(res)
  }

  stopifnot(is_string(dock_id))

  paste0(dock_id, "-", res)
}

block_panel_id_to_block_id <- function(x) {
  stopifnot(all(is_block_panel_id(x)))
  sub("^block-", "", x)
}

extension_panel_id <- function(ext_id, dock_id = NULL) {

  if (is_dock_board(ext_id)) {
    ext_id <- dock_extensions(ext_id)
  }

  if (is_dock_extension(ext_id)) {
    ext_id <- list(ext_id)
  }

  if (is.list(ext_id)) {
    ext_id <- chr_ply(ext_id, extension_id)
  }

  stopifnot(is.character(ext_id))

  if (!length(ext_id)) {
    return(character())
  }

  res <- paste0("ext-", ext_id)

  if (is.null(dock_id)) {
    return(res)
  }

  stopifnot(is_string(dock_id))

  paste0(dock_id, "-", res)
}

ext_panel_id_to_ext_id <- function(x) {
  stopifnot(all(is_ext_panel_id(x)))
  sub("^ext-", "", x)
}

is_zero_len <- function(x) {
  length(x) == 0L
}

is_empty <- function(x) {
  is_zero_len(x) || all(is.na(x) | !nchar(x))
}

filter_empty <- function(x) Filter(Negate(is_empty), x)
