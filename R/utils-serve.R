#' @export
serve.dock_board <- function(x, id = rand_names(), ...) {

  stopifnot(is_string(id))

  args <- list(...)

  ui <- function() {

    log_debug("building ui for board {id}")

    do.call(
      bslib::page_fillable,
      c(
        list(
          padding = 0,
          gap = 0,
          shinyjs::useShinyjs(),
          board_ui(id, get_serve_obj())
        ),
        unname(args)
      )
    )
  }

  server <- function(input, output, session) {

    withr::local_envvar(BLOCKR_BOARD_RESTORE = "")
    withr::local_options(blockr.board_restore = "v2")

    board_server(
      id,
      get_serve_obj(),
      callbacks = board_server_callback,
      callback_location = "start"
    )
  }

  shinyApp(ui, server)
}
