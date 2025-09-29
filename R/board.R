#' Dock board
#'
#' Using the docking layout manager provided by dockViewR, a `dock_board`
#' extends [blockr.core::new_board()].
#'
#' @inheritParams blockr.core::new_board
#'
#' @return A `board` object
#'
#' @rdname dock
#' @export
new_dock_board <- function(..., options = dock_board_options(),
                           class = character()) {
  new_board(..., options = options, class = c(class, "dock_board"))
}

#' @param x Board object
#' @rdname dock
#' @export
is_dock_board <- function(x) {
  inherits(x, "dock_board")
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
