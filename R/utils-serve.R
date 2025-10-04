#' @export
serve.dock_board <- function(x, id = rand_names(), ...) {

  stopifnot(is_string(id))

  opts <- as_board_options(x)

  if ("board_name" %in% names(opts)) {
    title <- board_option_value(opts[["board_name"]])
  } else {
    title <- id
  }

  ui <- do.call(
    bslib::page_fillable,
    c(
      list(
        padding = 0,
        gap = 0,
        title = title,
        shinyjs::useShinyjs(),
        board_ui(id, x)
      ),
      unname(list(...))
    )
  )

  server <- function(input, output, session) {
    board_server(
      id,
      x,
      callbacks = board_server_callback,
      callback_location = "start",
      layout = reactiveVal()
    )
  }

  shinyApp(ui, server)
}
