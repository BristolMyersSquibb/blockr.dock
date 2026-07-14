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

  # `options` is forwarded to `board_ui()` so the settings sidebar's
  # pre-rendered body reflects `serve(board, options = custom_options(...))`
  # overrides at page-build time.

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
        board_ui(id, x, plugins, options = options)
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

# Round-trip stability: every view's stored grid must be the fixed point the
# client echoes back, so the settled echo the restore push provokes is
# absorbed by the mirror's `all.equal(tolerance = grid_size_tol())` guard rather
# than committing. It holds when, for every view, the stored grid is
# that-tolerance-equal to the live grid the client reports -- the comparison the
# mirror commits on. `stored` and `live` are both `dock_grids`.
grids_stable <- function(stored, live) {

  view_stable <- function(v) {
    isTRUE(all.equal(stored[[v]], live[[v]], tolerance = grid_size_tol()))
  }

  all(lgl_ply(names(live), view_stable))
}

# Test-only server exports for the loop-safety sentinel, via blockr.core's
# `blockr_test_exports` hook. `shiny::exportTestValues()` is gated by Shiny's
# test mode (which shinytest2 and `testServer()` set), so a running app
# registers nothing -- no output, no DOM, no trace. `rv` is the board server's
# return: `rv$board` the read-only board reactives, `rv$view_data` the dock's
# live views + grids reactive.
#
#   * `commit_count` -- blockr.core's monotonic per-session update tally
#     (`last_update$seq`). The sentinel drives one gesture and asserts it ticks
#     once and quiescence adds none; a count still climbing after the app goes
#     idle is the loop.
#   * `roundtrip_stable` -- `TRUE` once every view's stored grid matches the
#     grid the client echoes (`NA` until every view has reported a layout), so
#     a restore push provokes no spurious commit.
#' @exportS3Method blockr.core::blockr_test_exports
blockr_test_exports.dock_board <- function(x, rv, ...) {

  exportTestValues(
    commit_count = coal(rv[["board"]]$last_update$seq, 0L, fail_all = FALSE),
    roundtrip_stable = {
      vd <- rv[["view_data"]]()
      if (is.null(vd)) {
        NA
      } else {
        grids_stable(board_grids(rv[["board"]]$board), vd[["grids"]])
      }
    }
  )
}
