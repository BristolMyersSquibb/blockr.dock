#' @export
board_plugins.dock_board <- function(x, which = NULL, ...) {

  core_plugins <- c("generate_code")
  core_plugins <- coal(intersect(which, core_plugins), core_plugins)

  if (length(core_plugins)) {
    plugins <- NextMethod(which = core_plugins)
  } else {
    plugins <- plugins()
  }

  if (!is_dock_locked() && (is.null(which) || "preserve_board" %in% which)) {
    plugins <- c(plugins, preserve_board(ui = ser_deser_ui))
  }

  if (is.null(which) || "edit_block" %in% which) {

    cbs <- lapply(dock_extensions(x), extension_block_callback)
    srv <- edit_block_server(Filter(Negate(is.null), cbs))

    plugins <- c(
      plugins,
      edit_block(srv, edit_block_ui, edit_block_validator)
    )
  }

  if (is.null(which)) {
    return(plugins)
  }

  stopifnot(is.character(which), all(which %in% names(plugins)))

  plugins[which]
}
