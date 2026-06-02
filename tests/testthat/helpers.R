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
