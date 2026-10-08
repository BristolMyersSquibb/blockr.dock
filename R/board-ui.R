#' @export
board_ui.dock_board <- function(
  id,
  x,
  plugins = board_plugins(x),
  options = blockr.core::blockr_app_options(x),
  navbar = blockr_app_navbar(x, plugins),
  ...
) {
  stopifnot(is_string(id))

  validate_navbar_items(navbar)

  # One dock output per view, stacked inside the view container; visibility
  # is toggled by CSS based on the active view.
  dock_outputs <- dock_outputs_ui(id, board_views(x))

  tagList(
    # Ahead of blockr_dock_dep(), so the shared tokens and theme land first
    # and this package's own rules override them by source order.
    blockr.ui::theme_dep(),
    blockr.ui::controls_dep(),
    show_block_dep(),
    attr_output_dep(),
    add_block_menu_dep(),
    block_rename_dep(),
    blockr_dock_dep(),
    compact_dep(),
    viewport_probe_ui(id),
    rail_dep(),
    off_canvas(
      id = NS(id, "blocks_offcanvas"),
      title = "Offcanvas blocks",
      # Only the active view's cards are built at startup; off-screen views'
      # cards are inserted on first visit. The build dominates first paint and
      # scales with total block count, not with what is on screen.
      block_ui(
        id,
        x,
        plugins[["edit_block"]],
        blocks = board_blocks(x)[active_view_block_ids(x)],
        ctrl_ui = if ("ctrl_block" %in% names(plugins)) plugins[["ctrl_block"]]
      )
    ),
    navbar_ui(id, navbar, x),
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
    # Contract: one sidebar = one concern. We mount two on the right
    # with different modes so they coexist cleanly when both are open
    # (adding a block is the "+" menu, action-menu.R, not a sidebar):
    #   * "actions_sidebar":  the trigger-specific editors (add and edit
    #     link, add and edit stack, block inputs). Body is populated
    #     server-side via `show_sidebar()` because each ships a
    #     freshly-built, trigger-dependent form.
    #   * "settings_sidebar": the board-options panel of the navbar's options
    #     button.
    #     `overlay` mode: layers above the page (and above the action
    #     panel when both are pinned) without reflowing content. Body is
    #     pre-rendered here at UI-build time and the options button opens it
    #     via `data-blockr-sidebar-target` (pure JS, no server roundtrip).
    # Reusing one DOM slot across both concerns would let a foreign caller
    # silently swap a pinned panel's body, since the JS replaces content in
    # place and never inspects the pin class. Splitting by concern keeps
    # pin semantics intuitive without multi-pin machinery on the JS side.
    # For "actions_sidebar", which cannot be split (its handlers ship
    # trigger-dependent forms), the equivalent guarantee is the ownership
    # stamp: every `show_sidebar()` records the writing module's namespaced
    # id on the panel -- `NS(<board id>, <action id>)` for a board action --
    # reported back as `owner` alongside `open` / `pinned` in the panel's
    # input value, so a holder of a trigger bundle can tell whose form is
    # currently on screen.
    sidebar_ui(
      NS(id, "actions_sidebar"),
      mode = "overlay",
      side = "right"
    ),
    sidebar_ui(
      NS(id, "settings_sidebar"),
      ui = settings_body(id, x, options = options),
      title = "Board options",
      mode = "overlay",
      side = "right",
      back = TRUE
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
    class = "blockr-view-container blockr-attr-output"
  )
}

spinner_delay_ms <- function() {

  ms <- suppressWarnings(as.integer(blockr_option("spinner_delay_ms", 200L)))

  if (length(ms) != 1L || is.na(ms) || ms < 0L) 200L else ms
}

# The viewport-width probe: a hidden element whose input binding reports
# `window.innerWidth` at Shiny's input initialisation, so the width lands in
# the session's first input batch and `is_narrow_viewport()` can be answered
# before the first dock is inserted. It never reports again -- the narrow
# decision is taken once per session.
viewport_probe_ui <- function(id) {
  div(
    id = NS(id, "viewport_width"),
    class = "blockr-viewport-probe",
    style = "display: none;",
    viewport_probe_dep()
  )
}

viewport_probe_dep <- function() {
  htmltools::htmlDependency(
    "blockr-viewport-probe",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "viewport-probe.js"
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
