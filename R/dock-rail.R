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

# The edges a dock offers. Left and right only: a rail is a side thing, and a
# full-width band across the top or bottom reads as a drawer rather than as the
# rail this is for. The top edge is also where every group's tab strip sits, so
# its reveal band overlaps the one place an ordinary tab drag always passes --
# workable (arm the band only once the pointer has left it since the drag
# began), but not worth carrying for an edge nobody has asked for.
rail_positions <- c("left", "right")

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

  if (!is_string(pos) || !pos %in% rail_positions) {
    blockr_abort(
      "A `dock_rail` must be pinned to one of {rail_positions}.",
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

# A grid's rails, keyed by the edge each is pinned to: dockView allows at most
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

# Whether a rail records anything a grid needs to store: its panels, or geometry
# an author set. An empty rail at its defaults is implied by every dock offering
# every edge, so storing it would be noise that round-trips forever.
rail_is_default <- function(rail) {
  identical(rail, new_dock_rail(rail[["position"]]))
}

# All panel ids held by one view's rails, in edge order.
rail_panel_ids <- function(rails) {
  as.character(unlst(lapply(rails, `[[`, "panels")))
}

# One view's rails, keyed by the edge each is pinned to: dockView allows at most
# one group per edge, so the edge is the key and a second rail claiming it is an
# authoring error rather than something to merge.
validate_grid_rails <- function(rails) {

  if (is.null(rails)) {
    return(invisible(rails))
  }

  if (!is.list(rails) || is_dock_rail(rails)) {
    blockr_abort(
      "A `dock_grid` `rails` component must be a list of `dock_rail` objects.",
      class = "dock_rails_structure_invalid"
    )
  }

  for (rail in rails) {
    validate_dock_rail(rail)
  }

  if (length(rails) && !identical(names(rails), chr_xtr(rails, "position"))) {
    blockr_abort(
      "A `dock_grid` `rails` component must be keyed by the edge each pins to.",
      class = "dock_rails_ids_missing"
    )
  }

  # Keying by position makes duplicates look well-formed to the check above --
  # two left rails carry names and positions that agree. Dockview allows one
  # group per edge, so a second claim on the same edge is malformed however it
  # was built, and the validator has to say so as loudly as the constructor.
  dup <- unique(names(rails)[duplicated(names(rails))])

  if (length(dup)) {
    blockr_abort(
      "At most one rail per edge, but {dup} {?is/are} claimed twice.",
      class = "dock_rails_position_clash"
    )
  }

  invisible(rails)
}

# Every edge, with whatever the grid stores taking precedence. All four are
# always available: which rails a dock *offers* is a constant of the dock, not
# something a constructor call or a saved layout gets to vary, so a grid only
# ever records which are populated. An empty rail is invisible and costs
# nothing, and it has to exist before a drag can reveal it -- deriving the set
# here means a grid stored before rails existed, and a view added at runtime,
# both offer the same edges without a migration.
with_default_rails <- function(rails) {

  out <- set_names(lapply(rail_positions, new_dock_rail), rail_positions)
  out[names(rails)] <- rails

  out
}

# Front `pid` in the rail that holds it, the rail half of `set_grid_active()`.
front_rail <- function(rail, pid) {

  if (!pid %in% rail[["panels"]]) {
    return(rail)
  }

  rail[["active"]] <- pid
  rail
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

# The default board populates one edge: extensions park on the left. The other
# three are offered by `with_default_rails()` like they are on every board, so
# there is nothing to declare here.
default_rails <- function(ext = character()) {

  if (!length(ext)) {
    return(list())
  }

  list(left = new_dock_rail("left", ext))
}

#' @rdname layout
#' @export
rail <- function(..., position = c("left", "right"),
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
