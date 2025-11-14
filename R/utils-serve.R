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

  do.call(
    page_fillable,
    c(
      list(
        padding = 0,
        gap = 0,
        theme = bs_theme(
          version = 5,
          # button have the same color as dockView tabs
          "btn-active-border-shade-amount" = "5%",
          "btn-active-bg-shade-amount" = "5%",
          "enable-negative-margins" = "true"
        ),
        # Use shiny's busy indicator
        useBusyIndicators(spinners = FALSE, pulse = TRUE),
        busyIndicatorOptions(
          pulse_background = "#5e626b",
          pulse_height = "5px"
        ),
        shinyjs::useShinyjs(),
        board_ui(id, x, plugins, options)
      ),
      unname(args)
    )
  )
}

#' @export
blockr_app_server.dock_board <- function(id, x, plugins, options, ...) {
  board_server(id, x, plugins, options, callbacks = board_server_callback,
               callback_location = "start", ...)
}
