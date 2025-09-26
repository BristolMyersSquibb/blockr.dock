#' @export
serve.dock_board <- function(x, id = rand_names(), ...) {

  opts <- as_board_options(x)

  if ("board_name" %in% names(opts)) {
    title <- board_option_value(opts[["board_name"]])
  } else {
    title <- id
  }

  plugins <- board_plugins(
    x,
    c(
      "preserve_board",
      "notify_user",
      "generate_code"
    )
  )

  ui <- do.call(
    bslib::page_fillable,
    c(
      list(
        padding = 0,
        gap = 0,
        title = title,
        shinyjs::useShinyjs(),
        board_ui(id, x, plugins)
      ),
      unname(
        list(...)
      )
    )
  )

  server <- function(input, output, session) {
    board_server(id, x, plugins, ...)
  }

  shinyApp(ui, server)
}
