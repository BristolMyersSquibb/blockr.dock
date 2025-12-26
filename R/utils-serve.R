#' @export
blockr_app_options.dock_board <- function(x, ...) {
  combine_board_options(
    board_options(x),
    lapply(dock_extensions(x), board_options),
    lapply(board_blocks(x), board_options),
    lapply(available_blocks(), board_options)
  )
}

#' @export
blockr_app_ui.dock_board <- function(id, x, plugins, options, ...) {

  args <- list(...)

  tagList(
    suppressDependencies("bootstrap"),
    tags$head(
      tags$meta(charset = "UTF-8"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
    ),
    blockr_dock_dep(),
    shinyjs::useShinyjs(),
    useBusyIndicators(spinners = FALSE, pulse = TRUE),
    busyIndicatorOptions(
      pulse_background = "#5e626b",
      pulse_height = "5px"
    ),
    # Shiny modal container (normally created by Bootstrap)
    tags$div(id = "shiny-modal"),
    tags$div(
      class = "blockr-app",
      navbar_ui(id),
      board_ui(id, x, plugins, options),
      args
    )
  )
}

#' @export
blockr_app_server.dock_board <- function(id, x, plugins, options, ...) {
  board_server(id, x, plugins, options, callbacks = board_server_callback,
               callback_location = "start", ...)
}
