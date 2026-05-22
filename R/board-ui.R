#' @export
board_ui.dock_board <- function(
  id,
  x,
  plugins = board_plugins(x),
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
    # Sidebar mounts. Fixed top-level DOM ids so any deeply-nested
    # consumer can target them via `blockr.ui::show_sidebar(<id>, ...)`
    # through `rootScope` walking. Contract: one sidebar = one concern.
    # We mount two on the right (matching their navbar triggers) but with
    # different modes so they coexist cleanly when both are open:
    #   * "actions_sidebar"  — the six action handlers (add/append/prepend
    #     block, add link, add/edit stack). `push` mode: shifts page
    #     content aside while open.
    #   * "settings_sidebar" — the navbar gear's board-options panel.
    #     `overlay` mode: layers above the page (and above the action
    #     panel when both are pinned) without reflowing content.
    # Reusing one DOM slot across both concerns would let a foreign caller
    # silently swap a pinned panel's body — the JS replaces content in
    # place and never inspects the pin class. Splitting by concern keeps
    # pin semantics intuitive without multi-pin machinery on the JS side.
    blockr.ui::sidebar_ui("actions_sidebar", mode = "push", side = "right"),
    blockr.ui::sidebar_ui("settings_sidebar", mode = "overlay", side = "right")
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
#' Caller-supplied `options` (threaded down from `serve(board, options =
#' custom_options(...))` via `blockr_app_server.dock_board()` →
#' `board_server_callback()` → `settings_observer()`) wins. When the
#' caller passed nothing, falls back to `blockr.core::blockr_app_options(x)`
#' so the accordion still includes options contributed by blocks on the
#' board and by registered block constructors — the same set `serve()`
#' would have computed on the default path.
#'
#' @param id Board module id.
#' @param x Current board (`board$board`).
#' @param plugins Board plugins.
#' @param options Augmented board options (board + block contributions).
#'   `NULL` means "no caller override"; the default is recomputed via
#'   `blockr.core::blockr_app_options(x)`.
#' @noRd
settings_body <- function(
  id,
  x,
  plugins = board_plugins(x),
  options = NULL
) {
  opt_ui_or_null <- function(plg, plgs, x) {
    if (plg %in% names(plgs)) board_ui(id, plgs[[plg]], x)
  }

  # Caller-supplied `options` (threaded from `serve()` through
  # `blockr_app_server.dock_board()` / `settings_observer()`) wins; fall
  # back to the recomputed default only when the caller has nothing to say.
  options <- options %||% blockr.core::blockr_app_options(x)

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
