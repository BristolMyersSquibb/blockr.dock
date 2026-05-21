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

  # `options` is part of blockr.core's `blockr_app_ui` contract but the dock
  # UI no longer renders the options accordion at UI-build time — the
  # settings sidebar is rendered lazily server-side, see
  # `blockr_app_server.dock_board()` and `settings_observer()`.

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
        board_ui(id, x, plugins)
      ),
      unname(args)
    )
  )
}

#' @export
blockr_app_server.dock_board <- function(id, x, plugins, options, ...) {
  # `options` is consumed by `blockr.core::board_server()` for its own
  # `board_options_to_userdata` wiring and is NOT forwarded into the
  # `callbacks = ` list. To keep `serve(board, options = custom_options(...))`
  # working in the settings sidebar (#TBD), re-pass the user-supplied
  # options as a renamed extra dot (`app_options = options`) so it lands
  # in `board_server_callback`'s `...` and can be threaded into
  # `settings_observer()` / `settings_body()`.
  board_server(id, x, plugins, options, callbacks = board_server_callback,
               callback_location = "start", app_options = options, ...)
}
