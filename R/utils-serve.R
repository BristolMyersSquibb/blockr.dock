#' @export
serve.dock_board <- function(x, id = rand_names(), ...) {

  stopifnot(is_string(id))

  args <- list(...)

  ui <- function() {

    log_debug("building ui for board {id}")

    do.call(
      page_fillable,
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

    onStop(enable_v2_restore(), session)

    board_server(
      id,
      get_serve_obj(),
      callbacks = board_server_callback,
      callback_location = "start"
    )
  }

  shinyApp(ui, server)
}

enable_v2_restore <- function() {

  log_debug("setting v2 restore")

  cur_opt <- options(blockr.board_restore = "v2")
  cur_env <- Sys.getenv("BLOCKR_BOARD_RESTORE")

  Sys.unsetenv("BLOCKR_BOARD_RESTORE")

  function() {
    log_debug("resetting restore version")
    options(cur_opt)
    Sys.setenv(BLOCKR_BOARD_RESTORE = cur_env)
  }
}
