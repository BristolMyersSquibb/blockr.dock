board_args <- function(...) {
  generate_plugin_args(
    new_dock_board(...),
    mode = "read"
  )[["board"]]
}
