#' @export
board_ui.dock_board <- function(
  id,
  x,
  plugins = board_plugins(x),
  options = board_options(x),
  ...
) {
  stopifnot(is_string(id))

  views <- board_views(x)

  # View nav in the navbar -- always present, since boards always carry a
  # `dock_layouts` (single-page boards have one auto-named "Page" view).
  v_nav <- view_nav_ui(id, views)

  # One dock output per view, stacked inside the view container; visibility
  # is toggled by CSS based on the active view.
  dock_outputs <- dock_outputs_ui(id, views)

  tagList(
    show_hide_block_dep(),
    blockr_dock_dep(),
    off_canvas(
      id = NS(id, "blocks_offcanvas"),
      title = "Offcanvas blocks",
      block_ui(
        id,
        x,
        plugins[["edit_block"]],
        ctrl_ui = if ("ctrl_block" %in% names(plugins)) plugins[["ctrl_block"]]
      )
    ),
    div(
      class = "blockr-navbar",
      div(
        class = "blockr-navbar-left",
        if ("preserve_board" %in% names(plugins)) {
          board_ui(id, plugins[["preserve_board"]], x)
        }
      ),
      div(
        class = "blockr-navbar-right",
        v_nav,
        actionButton(
          inputId = NS(id, "settings_btn"),
          label = bsicons::bs_icon("gear"),
          class = "blockr-navbar-icon-btn"
        )
      )
    ),
    dock_outputs,
    off_canvas(
      id = NS(id, "exts_offcanvas"),
      position = "bottom",
      title = "Offcanvas extensions",
      lapply(
        dock_extensions(x),
        extension_ui,
        id = id,
        board = x
      )
    ),
    # Sidebar mount: fixed top-level DOM id so any deeply-nested action
    # handler can target it via `blockr.ui::show_sidebar("main_sidebar", ...)`
    # through `rootScope` walking. Single sidebar per app at v0.
    blockr.ui::sidebar_ui("main_sidebar", mode = "push")
  )
}

# View container -- one dock output per view is inserted into this element
# at server time by `init_view_docks()`. Each dock output sits inside a
# nested moduleServer namespace so `manage_dock(id, ...)` can render to
# `session$output[[dock_id()]]` and dockViewR inputs resolve correctly.
dock_outputs_ui <- function(id, views) {
  div(
    id = NS(id, "view_container"),
    class = "blockr-view-container",
    style = "position: relative; height: calc(100vh - 48px);"
  )
}

#' Build the body of the board-options sidebar.
#'
#' Returns a tagList containing the same options accordion that the
#' Bootstrap offcanvas used to render. Called at server time from
#' `board_server_callback()` when the user clicks the navbar gear, and
#' passed to `blockr.ui::show_sidebar()`.
#'
#' Uses `blockr.core::blockr_app_options(x)` (rather than `board_options(x)`)
#' so the accordion includes options contributed by blocks on the board and
#' by registered block constructors — same set `serve()` passes to the
#' top-level `board_ui()` call.
#'
#' @param id Board module id.
#' @param x Current board (`board$board`).
#' @param plugins Board plugins.
#' @param options Augmented board options (board + block contributions).
#' @noRd
settings_body <- function(
  id,
  x,
  plugins = board_plugins(x),
  options = blockr.core::blockr_app_options(x)
) {
  opt_ui_or_null <- function(plg, plgs, x) {
    if (plg %in% names(plgs)) board_ui(id, plgs[[plg]], x)
  }

  stopifnot(is_board_options(options))

  opts <- split(options, chr_ply(options, attr, "category"))

  tagList(
    div(
      id = "generate_code",
      opt_ui_or_null("generate_code", plugins, x)
    ),
    hr(),
    do.call(
      accordion,
      c(
        list(
          id = NS(id, "board_options"),
          multiple = TRUE,
          open = FALSE,
          class = "accordion-flush"
        ),
        map(
          do.call,
          rep(list(accordion_panel), length(opts)),
          map(
            list,
            title = names(opts),
            lapply(opts, lapply, board_option_ui, id)
          )
        )
      )
    )
  )
}

blockr_dock_dep <- function() {
  htmltools::htmlDependency(
    "blockr-fab",
    pkg_version(),
    src = pkg_file("assets", "css"),
    stylesheet = "blockr-dock.css"
  )
}
