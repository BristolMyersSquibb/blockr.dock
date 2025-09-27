#' @export
board_plugins.dock_board <- function(x, which = NULL, ...) {

	plugins <- NextMethod(
    which = c("preserve_board", "notify_user", "generate_code")
  )

  if (is.null(which)) {
    return(plugins)
  }

  plugins[which]
}
