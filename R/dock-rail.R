# A rail is a group pinned to one edge of a view -- dockView's edge group. It is
# a board slot of its own, keyed by view and then by edge, because dockView
# serialises `edgeGroups` beside `grid` and a rail's membership is not grid
# geometry. A panel in a rail is simply absent from the grid tree, so every
# placement walk (`restrict_grid()`, `rewrite_grid_leaves()`, the dockView
# casts) is untouched, and a view's members are the union of its grid panels
# and its rail panels. Membership stays authoritative for which panels exist;
# the grid and the rails only say where.
#
# Shape: a `dock_rail` carries the `position` it is pinned to, the `panels` it
# holds, its open `active` tab, whether it is `collapsed` to its bare strip, and
# its `size` and `collapsed_size` in pixels.
# The `size` rides the settled echo like a branch's sizes do, so a sash drag
# persists; the two size fields are pixels rather than ratios because a rail is
# sized against the dock, not against a share of a split.
#
# Visibility is *not* stored. A rail holding panels is shown and an empty one is
# hidden, which is the same invariant the grid already keeps -- membership
# decides existence, geometry only arrangement -- and it leaves no flag to go
# stale. Rails are likewise declared once and never removed: dockView's
# `removeEdgeGroup()` disposes the panels the rail holds, and a declared rail
# stays put as a target a drag can reveal.

rail_positions <- function() {
  c("left", "right", "top", "bottom")
}

new_dock_rail <- function(position = "left", panels = character(),
                          active = NULL, collapsed = FALSE, size = 260,
                          collapsed_size = 35) {

  panels <- as.character(panels)

  if (!length(panels)) {
    active <- NULL
  } else if (is.null(active) || !active %in% panels) {
    active <- panels[[1L]]
  }

  structure(
    list(
      position = position,
      panels = panels,
      active = active,
      collapsed = collapsed,
      size = rail_size(size),
      collapsed_size = rail_size(collapsed_size)
    ),
    class = "dock_rail"
  )
}

# A pixel size is a double however it arrives. JSON reads a whole number back
# as an integer, so without normalising here a restored rail would not compare
# `identical()` to the one it was saved from. A non-number passes through for
# `validate_dock_rail()` to reject.
rail_size <- function(x) {
  if (is.numeric(x)) as.double(x) else x
}

is_dock_rail <- function(x) {
  inherits(x, "dock_rail")
}

validate_dock_rail <- function(x) {

  if (!is_dock_rail(x) || !is.list(x)) {
    blockr_abort(
      "Expecting a `dock_rail` object.",
      class = "dock_rail_structure_invalid"
    )
  }

  unexpected <- setdiff(
    names(x),
    c("position", "panels", "active", "collapsed", "size", "collapsed_size")
  )

  if (length(unexpected)) {
    blockr_abort(
      "Unexpected `dock_rail` component{?s} {unexpected}.",
      class = "dock_rail_structure_invalid"
    )
  }

  pos <- x[["position"]]

  if (!is_string(pos) || !pos %in% rail_positions()) {
    blockr_abort(
      "A `dock_rail` must be pinned to one of {rail_positions()}.",
      class = "dock_rail_position_invalid"
    )
  }

  if (!is.character(x[["panels"]])) {
    blockr_abort(
      "A `dock_rail` must carry its `panels` as a character vector.",
      class = "dock_rail_structure_invalid"
    )
  }

  active <- x[["active"]]

  if (not_null(active) && !(is_string(active) && active %in% x[["panels"]])) {
    blockr_abort(
      "A `dock_rail` `active` tab must be one of its panels.",
      class = "dock_rail_active_invalid"
    )
  }

  if (!is_bool(x[["collapsed"]])) {
    blockr_abort(
      "A `dock_rail` `collapsed` must be `TRUE` or `FALSE`.",
      class = "dock_rail_collapsed_invalid"
    )
  }

  for (fld in c("size", "collapsed_size")) {
    validate_rail_size(x[[fld]], fld)
  }

  invisible(x)
}

validate_rail_size <- function(value, arg) {

  if (is.null(value)) {
    return(invisible(NULL))
  }

  if (!is_number(value) || is.na(value) || !is.finite(value) || value < 0) {
    blockr_abort(
      "A `dock_rail` `{arg}` must be a single non-negative number of pixels.",
      class = "dock_rail_size_invalid"
    )
  }

  invisible(NULL)
}

as_dock_rail <- function(x) {

  if (is_dock_rail(x)) {
    return(x)
  }

  new_dock_rail(
    position = coal(x[["position"]], "left", fail_all = FALSE),
    panels = coal(unlst(x[["panels"]]), character(), fail_all = FALSE),
    active = x[["active"]],
    collapsed = isTRUE(x[["collapsed"]]),
    size = x[["size"]],
    collapsed_size = x[["collapsed_size"]]
  )
}

restrict_rail <- function(rail, members) {

  new_dock_rail(
    rail[["position"]],
    intersect(rail[["panels"]], members),
    active = rail[["active"]],
    collapsed = rail[["collapsed"]],
    size = rail[["size"]],
    collapsed_size = rail[["collapsed_size"]]
  )
}

#' @export
str_value.dock_rail <- function(x, ...) {

  ids <- panel_obj_ids(x[["panels"]])
  held <- if (length(ids)) paste0(ids, collapse = ", ") else "(empty)"

  paste0("<dock_rail> ", x[["position"]], ": ", held)
}

#' @export
format.dock_rail <- function(x, ...) {
  str_value(x)
}

#' @export
print.dock_rail <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

# One view's rails, keyed by the edge each is pinned to: dockView allows at most
# one group per edge, so the edge is the key and a second rail claiming it is an
# authoring error rather than something to merge.
view_rail_set <- function(x) {

  if (is_dock_rail(x)) {
    x <- list(x)
  }

  rails <- lapply(x, as_dock_rail)

  if (!length(rails)) {
    return(list())
  }

  pos <- chr_xtr(rails, "position")
  dup <- unique(pos[duplicated(pos)])

  if (length(dup)) {
    blockr_abort(
      "At most one rail per edge, but {dup} {?is/are} claimed twice.",
      class = "dock_rails_position_clash"
    )
  }

  set_names(rails, pos)
}

# All panel ids held by one view's rails, in edge order.
rail_panel_ids <- function(rails) {
  as.character(unlst(lapply(rails, `[[`, "panels")))
}

new_dock_rails <- function(x = list()) {
  structure(x, class = "dock_rails")
}

#' @rdname view
#' @export
is_dock_rails <- function(x) {
  inherits(x, "dock_rails")
}

#' @rdname view
#' @export
validate_dock_rails <- function(x, views = NULL) {

  if (is.null(x)) {
    return(x)
  }

  if (!is_dock_rails(x) || !is.list(x)) {
    blockr_abort(
      "Expecting a `dock_rails` object or `NULL`.",
      class = "dock_rails_structure_invalid"
    )
  }

  ids <- names(x)

  if (length(x) && (is.null(ids) || any(ids == ""))) {
    blockr_abort(
      "All rails must be keyed by view id.",
      class = "dock_rails_ids_missing"
    )
  }

  if (not_null(views)) {

    unknown <- setdiff(ids, names(views))

    if (length(unknown)) {
      blockr_abort(
        "Rail{?s} {unknown} reference no known view.",
        class = "dock_rails_unknown_view"
      )
    }
  }

  for (id in ids) {
    validate_view_rails(x[[id]], id)
  }

  x
}

validate_view_rails <- function(rails, view_id) {

  if (is.null(rails)) {
    return(invisible(rails))
  }

  if (!is.list(rails) || is_dock_rail(rails)) {
    blockr_abort(
      "The rails of view {view_id} must be a list of `dock_rail` objects.",
      class = "dock_rails_structure_invalid"
    )
  }

  for (rail in rails) {
    validate_dock_rail(rail)
  }

  keys <- names(rails)

  if (length(rails) && !identical(keys, chr_xtr(rails, "position"))) {
    blockr_abort(
      "The rails of view {view_id} must be keyed by the edge they pin to.",
      class = "dock_rails_ids_missing"
    )
  }

  invisible(rails)
}

#' @rdname view
#' @export
board_rails <- function(x) {
  stopifnot(is_dock_board(x))
  x[["rails"]]
}

#' @rdname view
#' @export
`board_rails<-` <- function(x, value) {
  stopifnot(is_dock_board(x))
  x[["rails"]] <- validate_dock_rails(value, board_views(x))
  invisible(x)
}

# A view's rails, member-driven in the same way `view_grid()` is: a rail holds
# the members it names and nothing else, so a ghost left behind by a removed
# block drops out on read. A declared rail survives emptying -- it is the
# reveal target a drag aims at, and emptiness is what hides it.
view_rails <- function(view, rails) {

  if (is.null(rails)) {
    return(list())
  }

  lapply(rails, restrict_rail, members = view_members(view))
}

# The rails of the active view, the counterpart of `active_view_grid()`.
active_view_rails <- function(board) {

  id <- active_view(board)

  view_rails(board_views(board)[[id]], board_rails(board)[[id]])
}

restrict_rails_to_views <- function(rails, views) {

  for (id in names(rails)) {

    if (id %in% names(views)) {
      rails[[id]] <- view_rails(views[[id]], rails[[id]])
    }
  }

  rails
}

coerce_dock_rails <- function(rails, id_map) {

  if (is.null(rails)) {
    return(new_dock_rails())
  }

  if (is_dock_rails(rails)) {
    return(rails)
  }

  new_dock_rails(lapply(rails, resolve_view_rails, id_map = id_map))
}

resolve_view_rails <- function(rails, id_map) {
  lapply(view_rail_set(rails), resolve_rail, id_map = id_map)
}

resolve_rail <- function(rail, id_map) {

  panels <- resolve_panel_ids(rail[["panels"]], id_map)
  active <- rail[["active"]]

  if (not_null(active)) {
    active <- resolve_panel_ids(active, id_map)
  }

  new_dock_rail(
    rail[["position"]],
    panels,
    active = active,
    collapsed = rail[["collapsed"]],
    size = rail[["size"]],
    collapsed_size = rail[["collapsed_size"]]
  )
}

# The default board's rails: extensions park on the left edge, and the right
# edge is declared empty. Declaring an edge is what makes it a reveal target, so
# the empty one is the whole reason a user can park a block on an edge at all --
# and since visibility is derived, it stays invisible until something lands in
# it. The left rail is likewise declared whether or not the board has any
# extensions.
#
# The top and bottom edges are deliberately left out. Each group's tab strip
# sits along its top, so an ordinary drag across the tabs would sweep the top
# reveal band, and a rail running the full width reads as a drawer rather than
# as the side rail this is for.
default_rails <- function(ext = character()) {
  list(
    left = new_dock_rail("left", ext),
    right = new_dock_rail("right")
  )
}

# The members the grid places: everything a rail does not hold. A panel in a
# rail is absent from the grid tree, so the two slots partition the membership
# and no panel is ever placed twice.
grid_members <- function(view, rails) {
  setdiff(view_members(view), rail_panel_ids(rails))
}

#' @rdname layout
#' @export
rail <- function(..., position = c("left", "right", "top", "bottom"),
                 active = NULL, collapsed = FALSE, size = 260,
                 collapsed_size = 35) {

  position <- match.arg(position)
  ids <- chr_ply(list(...), as_panel_string)

  if (not_null(active)) {
    active <- as_panel_string(active)
  }

  if (length(ids) && not_null(active) && !active %in% ids) {
    blockr_abort(
      "`active` must be one of the panel ids.",
      class = "dock_rail_active_invalid"
    )
  }

  validate_dock_rail(
    new_dock_rail(
      position, ids,
      active = active,
      collapsed = collapsed,
      size = size,
      collapsed_size = collapsed_size
    )
  )
}
