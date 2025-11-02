#' @export
board_plugins.dock_board <- function(x, which = NULL, ...) {
  plugins <- NextMethod(which = c("notify_user", "generate_code"))

  if (!is_dock_locked()) {
    plugins <- c(plugins, preserve_board(ui = ser_deser_ui))
  }

  plugins <- c(
    plugins,
    edit_block(server = edit_block_server, ui = edit_block_ui)
  )

  if (is.null(which)) {
    return(plugins)
  }

  plugins[which]
}
