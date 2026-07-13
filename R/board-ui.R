#' @export
board_ui.dock_board <- function(
  id,
  x,
  plugins = board_plugins(x),
  options = blockr.core::blockr_app_options(x),
  ...
) {
  stopifnot(is_string(id))

  views <- board_views(x)

  # View nav in the navbar -- always present, since boards always carry a
  # `dock_views` collection (single-page boards have one auto-named "Page"
  # view). The nav needs only structure (ids, names, active), not geometry.
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
        if (is_dock_locked()) {
          tags$span(
            class = "blockr-lock-indicator",
            title = "Editing is disabled by this deployment.",
            `aria-label` = "Read-only mode",
            role = "status",
            bsicons::bs_icon("lock-fill"),
            tags$span(
              class = "blockr-lock-indicator-label",
              "Read-only"
            )
          )
        },
        # Pure-JS open trigger via `data-blockr-sidebar-target`. The
        # settings sidebar's body is pre-rendered into its mount below, so
        # no server observer is needed: clicking the gear toggles the
        # panel client-side. `tags$button` (not `actionButton`) because
        # there is no `input$<id>` to wire.
        tags$button(
          type = "button",
          class = "btn action-button blockr-navbar-icon-btn",
          `data-blockr-sidebar-target` = NS(id, "settings_sidebar"),
          `aria-label` = "Board options",
          bsicons::bs_icon("gear")
        )
      )
    ),
    dock_outputs,
    off_canvas(
      id = NS(id, "exts_offcanvas"),
      position = "bottom",
      title = "Offcanvas extensions",
      map(
        extension_ui,
        dock_extensions(x),
        dock_ext_ids(x),
        MoreArgs = list(id = id, board = x)
      )
    ),
    # Sidebar mounts. Ids are namespaced with `NS(id, ...)` so two
    # `board_ui.dock_board()` instances on the same page don't collide on
    # DOM ids. Action handlers reach the matching mount by reading
    # `board$board_id` (set by blockr.core in the board's reactiveValues)
    # and composing `NS(board$board_id, "actions_sidebar")` at server time.
    # Contract: one sidebar = one concern. We mount four on the right
    # (matching their navbar triggers) but with different modes so they
    # coexist cleanly when both are open:
    #   * "actions_sidebar":  the remaining trigger-specific handlers
    #     (add link, add/edit stack, and the dormant prepend block).
    #     Body is populated server-side via `show_sidebar()` because each
    #     ships a freshly-built, trigger-dependent form.
    #   * "add_block_sidebar" / "append_block_sidebar": the block browser
    #     for the add and append flows. Both catalogues are registry-based
    #     (board- / source-independent), so their bodies are pre-rendered
    #     here at UI-build time and the handlers just toggle them open, so
    #     opening never re-renders. The source for append is supplied
    #     server-side at commit.
    #   * "settings_sidebar": the navbar gear's board-options panel.
    #     `overlay` mode: layers above the page (and above the action
    #     panel when both are pinned) without reflowing content. Body is
    #     pre-rendered here at UI-build time and the gear button opens it
    #     via `data-blockr-sidebar-target` (pure JS, no server roundtrip).
    # Reusing one DOM slot across both concerns would let a foreign caller
    # silently swap a pinned panel's body, since the JS replaces content in
    # place and never inspects the pin class. Splitting by concern keeps
    # pin semantics intuitive without multi-pin machinery on the JS side.
    sidebar_ui(
      NS(id, "actions_sidebar"),
      mode = "overlay",
      side = "right"
    ),
    # "add_block_sidebar": the add-block browser. Its catalogue is the
    # registry and never varies, so the body is pre-rendered here once
    # rather than rebuilt on every open (the dynamic actions_sidebar
    # path). The id is composed so the markup lands under the
    # `add_block_action` server's namespace - that handler mounts
    # `block_browser_server("browser")` and just toggles this panel
    # (`show_sidebar()` with no `ui`), so opening it never re-renders.
    sidebar_ui(
      NS(id, "add_block_sidebar"),
      ui = block_browser_ui(
        NS(NS(id, "add_block_action"), "browser")
      ),
      title = "Add new block",
      mode = "overlay",
      side = "right"
    ),
    # "append_block_sidebar": the append browser. Its linkable-block
    # catalogue is registry-based, not source-specific, so it too is
    # pre-rendered once (via the source-less `append_to()` descriptor).
    # The right-clicked source is supplied server-side by the append
    # action at commit; the "Append from X" context goes in the sidebar
    # title. Composed id so the markup lands under the
    # `append_block_action` server's namespace.
    sidebar_ui(
      NS(id, "append_block_sidebar"),
      ui = block_browser_ui(
        NS(NS(id, "append_block_action"), "browser"),
        target = append_to()
      ),
      title = "Append new block",
      mode = "overlay",
      side = "right"
    ),
    sidebar_ui(
      NS(id, "settings_sidebar"),
      ui = settings_body(id, x, options = options),
      title = "Board options",
      mode = "overlay",
      side = "right"
    )
  )
}

# View container -- one dock output per view is inserted into this element
# at server time by the reconcile pass (`create_view()`). Each dock
# output sits inside a nested moduleServer namespace so `manage_dock(id, ...)`
# can render to `session$output[[dock_id()]]` and dockViewR inputs resolve.
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
#' passed to `show_sidebar()`.
#'
#' Caller-supplied `options` (threaded down from `serve(board, options =
#' custom_options(...))` via `blockr_app_server.dock_board()` →
#' `board_server_callback()` → `settings_observer()`) wins. When the
#' caller passed nothing, falls back to `blockr.core::blockr_app_options(x)`
#' so the accordion still includes options contributed by blocks on the
#' board and by registered block constructors, the same set `serve()`
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
  options <- coal(options, blockr.core::blockr_app_options(x))

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
