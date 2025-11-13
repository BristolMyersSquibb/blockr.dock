#' @export
blockr_app_options.dock_board <- function(x, ...) {
  combine_board_options(
    board_options(x),
    lapply(dock_extensions(x), board_options),
    lapply(board_blocks(x), board_options),
    lapply(available_blocks(), board_options)
  )
}

#' @export
blockr_app_ui.dock_board <- function(id, x, plugins, options, ...) {

  args <- list(...)

  do.call(
    page_fillable,
    c(
      list(
        padding = 0,
        gap = 0,
        shinyjs::useShinyjs(),
        board_ui(id, x, plugins, options)
      ),
      unname(args)
    )
  )
}

#' @export
blockr_app_server.dock_board <- function(id, x, plugins, options, ...) {
  board_server(id, x, plugins, options, callbacks = board_server_callback,
               callback_location = "start", ...)
}
