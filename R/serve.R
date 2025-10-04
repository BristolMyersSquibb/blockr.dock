#' @export
serve.dock_board <- function(x, id = rand_names(), extensions = list(), ...) {

  stopifnot(is_string(id), is_extension_set(extensions))

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
      callbacks = dock_board_server_callback(extensions),
      callback_location = "start",
      layout = reactiveVal()
    )
  }

  shinyApp(ui, server)
}

dock_board_server_callback <- function(extensions) {

  stopifnot(is_extension_set(extensions))

  function(board, update, ..., session = get_session()) {

    layout <- manage_dock(board, extensions, session)

    intercom <- set_names(
      replicate(length(extensions), reactiveVal()),
      extensions
    )

    ext_state <- set_names(
      vector("list", length(extensions)),
      names(extensions)
    )

    for (i in names(extensions)) {
      ext_state[[i]] <- do.call(
        extension_server(extensions[[i]]),
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
}

is_extension_set <- function(x) {
  is.list(x) && all(lgl_ply(x, is_dock_extenstion)) &&
    identical(anyDuplicated(chr_ply(x, extension_id)), 0L)
}
