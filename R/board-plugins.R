#' @export
board_plugins.dock_board <- function(x, which = NULL, ...) {
  plugins <- NextMethod(which = c("notify_user", "generate_code", "edit_block"))

  if (!is_dock_locked()) {
    plugins <- c(plugins, preserve_board(ui = ser_deser_ui))
  }

  if (is.null(which)) {
    return(plugins)
  }

  plugins[which]
}
