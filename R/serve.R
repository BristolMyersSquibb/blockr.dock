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
      callbacks = dock_board_server_callback,
      callback_location = "start",
      layout = reactiveVal()
    )
  }

  shinyApp(ui, server)
}

dock_board_server_callback <- function(board, update, ...,
                                       session = get_session()) {

  layout <- manage_dock(board, session)

  exts <- isolate(
    dock_extensions(board$board)
  )

  intercom <- set_names(
    replicate(length(exts), reactiveVal()),
    exts
  )

  ext_state <- set_names(
    vector("list", length(exts)),
    names(exts)
  )

  for (i in names(exts)) {
    ext_state[[i]] <- do.call(
      extension_server(exts[[i]]),
      c(
        list(board = board, update = update, layout = layout),
        intercom,
        list(...),
        list(session = session)
      )
    )
  }

  c(
    list(layout = layout),
    ext_state
  )
}
