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
# which Shiny re-binds client-side after the server goes idle; wait for the
# binding before driving the new row's inputs.
wait_bound <- function(app, id) {
  app$wait_for_js(
    paste0(
      "document.querySelector('#", nsid(id),
      ".shiny-bound-input') !== null"
    ),
    timeout = 15 * 1000
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
