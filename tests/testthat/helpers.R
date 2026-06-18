board_args <- function(...) {
  generate_plugin_args(
    new_dock_board(...),
    mode = "read"
  )[["board"]]
}

# Resolve a view's stable id from its display label. Views are keyed by
# id internally; tests that know a view by its label use this to reach
# the id (labels are unique within the fixtures).
vid <- function(x, name) {
  if (is_dock_board(x)) {
    x <- board_layouts(x)
  }
  nms <- view_names(x)
  id <- names(nms)[match(name, nms)]
  if (is.na(id)) {
    stop("no view named ", name)
  }
  id
}

# Display label of the active view (derived from the id when unnamed),
# or NULL when none is active.
active_name <- function(x) {
  if (is_dock_board(x)) {
    x <- board_layouts(x)
  }
  av <- active_view(x)
  if (is.null(av)) {
    return(NULL)
  }
  unname(view_names(x)[av])
}

# Minimal externally controllable extension: its sole constructor input
# `content` is exposed as a `reactiveVal` in `state`, mirroring the author
# contract a real extension (e.g. blockr.md) implements. `external_ctrl` is
# set internally so it does not leak into the constructor formals (which
# define the controllable variables).
new_ctrl_extension <- function(content = "") {
  new_dock_extension(
    server = function(id, ...) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          list(state = list(content = shiny::reactiveVal(content)))
        }
      )
    },
    ui = function(id) shiny::tagList(),
    name = "Document",
    class = "doc_extension",
    external_ctrl = TRUE
  )
}

# Parse the live view-nav dropdown of a running app into one row per view
# (id, display label, active flag), so a browser test can state the nav
# contract directly: one entry per view, unique ids, no blank labels. Reads
# the rendered DOM (post static UI + any client-side `add`/`remove`), which
# is where the two layers compose -- the seam a unit test can't observe.
#
# `blockr-view-item` is a prefix of the child `-name` / `-actions` classes,
# so match the class token exactly (space-padded) rather than by substring.
read_view_nav <- function(app, board_id = "my_board") {
  by_class <- function(axis, token) {
    sprintf(
      "%s*[contains(concat(' ', normalize-space(@class), ' '), ' %s ')]",
      axis, token
    )
  }

  nav <- xml2::read_html(app$get_html(paste0("#", board_id, "-view_nav")))
  items <- xml2::xml_find_all(nav, by_class("//", "blockr-view-item"))
  name_nodes <- xml2::xml_find_first(
    items, by_class(".//", "blockr-view-item-name")
  )

  data.frame(
    id = xml2::xml_attr(items, "data-view-id"),
    label = trimws(xml2::xml_text(name_nodes)),
    active = grepl("(^| )active( |$)", xml2::xml_attr(items, "class")),
    stringsAsFactors = FALSE
  )
}

# Parse the live view-dock containers into one row per view (id, active flag),
# so a browser test can assert the rendered docks track a view-lifecycle
# change alongside the nav: switching moves the `blockr-view-dock-active`
# class, adding inserts a container, removing drops one. Each view's dock sits
# in `#<board>-view_handle-<id>`; the active one carries the extra class
# (toggled by the `switch-view` client handler). Reads the composed DOM -- the
# seam a unit test can't observe.
read_view_docks <- function(app, board_id = "my_board") {
  by_class <- function(token) {
    sprintf(
      "//*[contains(concat(' ', normalize-space(@class), ' '), ' %s ')]",
      token
    )
  }

  cont <- xml2::read_html(
    app$get_html(paste0("#", board_id, "-view_container"))
  )
  docks <- xml2::xml_find_all(cont, by_class("blockr-view-dock"))
  prefix <- paste0("^", board_id, "-view_handle-")

  data.frame(
    id = sub(prefix, "", xml2::xml_attr(docks, "id")),
    active = grepl(
      "(^| )blockr-view-dock-active( |$)", xml2::xml_attr(docks, "class")
    ),
    stringsAsFactors = FALSE
  )
}

# Wait until the server-rendered dock shell is in place: the view nav, every
# block card and the active view's handle. These render independently of the
# dockview client, so the serialization e2e can read them without racing the
# (client-side) panel layout.
wait_dock_loaded <- function(app, n_blocks, board_id = "my_board") {
  app$wait_for_js(
    sprintf("document.querySelector('#%s-view_nav') !== null", board_id),
    timeout = 30 * 1000
  )
  app$wait_for_js(
    sprintf(
      "document.querySelectorAll('[id^=\"%s-block_handle-\"]').length === %d",
      board_id, n_blocks
    ),
    timeout = 30 * 1000
  )
  app$wait_for_js(
    "document.querySelector('.blockr-view-dock-active') !== null",
    timeout = 30 * 1000
  )
}

# The dock-owned board state observable in server-rendered DOM: the view nav
# (one row per view), the active view's id (the `blockr-view-dock-active`
# handle), and every block's card id. The serialization e2e captures this
# before and after a save / restore reload to assert the round-trip rebuilds it
# identically. Block cards live in a pool the dockview client teleports into
# panels, so query their ids globally rather than from a fixed container.
read_dock_state <- function(app, board_id = "my_board") {
  container <- xml2::read_html(
    app$get_html(paste0("#", board_id, "-view_container"))
  )
  active_node <- xml2::xml_find_all(
    container,
    paste0(
      "//*[contains(concat(' ', normalize-space(@class), ' '), ",
      "' blockr-view-dock-active ')]"
    )
  )
  active_view <- sub(
    paste0("^", board_id, "-view_handle-"), "",
    xml2::xml_attr(active_node, "id")
  )

  handles <- unlist(
    app$get_js(
      sprintf(
        paste0(
          "Array.from(document.querySelectorAll(",
          "'[id^=\"%s-block_handle-\"]')).map(e => e.id)"
        ),
        board_id
      )
    )
  )
  block_ids <- sort(sub(paste0("^", board_id, "-block_handle-"), "", handles))

  list(
    nav = read_view_nav(app, board_id),
    active_view = active_view,
    blocks = block_ids
  )
}

# A block panel's open tab outlives the panel content, which dockview detaches
# while inactive -- so the tab strip, not the (detached) card, is where panel
# presence is observable. Each tab carries its panel id as
# `...-dock-tab-block_panel-<id>`; read them sorted straight off the live DOM.
block_panel_tabs <- function(app, board_id = "my_board") {
  html <- xml2::read_html(
    app$get_html(paste0("#", board_id, "-view_container"))
  )
  nodes <- xml2::xml_find_all(html, "//*[contains(@id, '-tab-block_panel-')]")
  sort(sub(".*-tab-(block_panel-.+)$", "\\1", xml2::xml_attr(nodes, "id")))
}

# Shared helpers for the edit-board extension e2e tests (links, stacks). The
# extension namespaces its inputs under `my_board-edit_board_extension-`. The
# extension panel stays active in those tests (pre-seeded fixtures, no block
# adds), so plain set_inputs/click drive it reliably.
nsid <- function(x) paste0("my_board-edit_board_extension-", x)

# Staging inputs do not drive an output, so do not wait for one.
set_in <- function(app, id, value) {
  do.call(
    app$set_inputs,
    c(set_names(list(value), nsid(id)), list(wait_ = FALSE))
  )
}

click <- function(app, id) app$click(nsid(id))

# A DataTables redraw (e.g. after adding a row) re-renders the cell inputs,
# which the table's `drawCallback` re-binds via `Shiny.bindAll` -- but only
# once the async redraw completes, which can land *after* the server reports
# idle. So idle alone does not mean the new inputs are drivable: wait for idle,
# then poll for the `.shiny-bound-input` marker the rebind adds, both under one
# generous budget. A fixed 15s window let a slow CI runner's redraw outlast it
# and eject green PRs from the merge queue.
wait_bound <- function(app, id, timeout = 30 * 1000) {

  app$wait_for_idle(timeout = timeout)

  app$wait_for_js(
    paste0(
      "document.querySelector('#", nsid(id), ".shiny-bound-input') !== null"
    ),
    timeout = timeout
  )
}

set_color <- function(app, id, hex) {
  app$run_js(
    paste0(
      "var e=document.getElementById('", nsid(id), "'); e.value='", hex,
      "'; e.dispatchEvent(new Event('change'));"
    )
  )
}

field <- function(app, id) {
  app$get_js(
    paste0(
      "(function(){var e=document.getElementById('", nsid(id),
      "'); return e ? e.value : null;})()"
    )
  )
}
