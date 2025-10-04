#' Dock board
#'
#' Using the docking layout manager provided by dockViewR, a `dock_board`
#' extends [blockr.core::new_board()].
#'
#' @inheritParams blockr.core::new_board
#' @param layout Dock layout
#' @param extensions Dock extensions
#'
#' @return A `board` object
#'
#' @rdname dock
#' @export
new_dock_board <- function(..., layout = new_dock_layout(),
                           extensions = list(),
                           options = dock_board_options(),
                           class = character()) {

  if (is_dock_extension(extensions)) {
    extensions <- list(extensions)
  }

  if (is.null(names(extensions))) {
    names(extensions) <- sub(
      "_extension$",
      "",
      chr_ply(extensions, extension_id)
    )
  }

  new_board(..., layout = as_dock_layout(layout), extensions = extensions,
            options = options, class = c(class, "dock_board"))
}

#' @param x Board object
#' @rdname dock
#' @export
is_dock_board <- function(x) {
  inherits(x, "dock_board")
}

#' @export
validate_board.dock_board <- function(x) {
  x <- NextMethod()
  validate_dock_layout(dock_layout(x), board_block_ids(x))
  x
}

#' @rdname dock
#' @export
dock_layout <- function(x) {
  stopifnot(is_dock_board(x))
  validate_dock_layout(x[["layout"]], board_block_ids(x))
}

#' @param value Replacement value
#' @rdname dock
#' @export
`dock_layout<-` <- function(x, value) {
  stopifnot(is_dock_board(x))
  x[["layout"]] <- validate_dock_layout(value, board_block_ids(x))
  x
}

#' @rdname dock
#' @export
dock_extensions <- function(x) {
  stopifnot(is_dock_board(x))
  validate_dock_extensions(x[["extensions"]])
}

#' @param value Replacement value
#' @rdname dock
#' @export
`dock_extensions<-` <- function(x, value) {
  stopifnot(is_dock_board(x))
  x[["extensions"]] <- validate_dock_extensions(value)
  x
}

#' @rdname dock
#' @export
dock_board_options <- function() {
  new_board_options(
    new_board_name_option(category = "Board options"),
    if (need_llm_cfg_opts()) new_llm_model_option(category = "Board options"),
    new_n_rows_option(category = "Table options"),
    new_page_size_option(category = "Table options"),
    new_filter_rows_option(category = "Table options"),
    new_thematic_option(category = "Theme options"),
    new_dark_mode_option(
      blockr_option("dark_mode", FALSE),
      category = "Theme options"
    ),
    new_show_conditions_option(category = "Board options"),
    new_blocks_position_option(category = "Layout options")
  )
}

validate_dock_extensions <- function(x) {

  if (!is.list(x) || !all(lgl_ply(x, is_dock_extension))) {
    blockr_abort(
      "Expecting a set of extensions to be represented by a list of objects, ",
      "where each inherits from `dock_extension`.",
      class = "dock_extension_invalid"
    )
  }

  if (!identical(anyDuplicated(chr_ply(x, extension_id)), 0L)) {
    blockr_abort(
      "Expecting a set of extensions to consist of unique extension types.",
      class = "dock_extension_invalid"
    )
  }

  nm <- names(x)

  if (is.null(nm) || any(!nchar(nm)) || !identical(anyDuplicated(nm), 0L)) {
    blockr_abort(
      "Expecting a set of extensions to have unique names.",
      class = "dock_extension_invalid"
    )
  }

  invisible(
    lapply(x, validate_extension)
  )
}
