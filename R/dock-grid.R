# A view's geometry has two representations: dockView's client grid (volatile
# leaf ids and absolute pixel sizes, wrapped at the seam as a `dock_layout`)
# and ours, a canonical `dock_grid`. `as_dock_grid()` casts into ours -- from
# another `dock_grid` (identity) or from a `dock_layout` -- and the constructor
# canonicalises: `new_dock_grid()` normalises each branch's sizes to 0-1 ratios
# and reassigns leaf ids deterministically (no wire round-trip), so two casts
# of the same layout are `identical()`. The mirror stores one on every settled
# echo.
#
# Relative sizes ride through verbatim (faithful for serialization and
# programmatic resize); the commit guard tolerates their pixel-rounding jitter
# via `all.equal(tolerance = grid_size_tol())`, so a window resize is absorbed
# while a deliberate sash drag still commits.
#
# The grid holds geometry only. Which panels a view actually shows is its
# membership's call, resolved when the placement is read (`view_grid()`): the
# view's members drive, the grid supplies their arrangement, a member the grid
# omits is appended a default spot, and a grid entry that is no longer a member
# (a ghost) is dropped. Membership is authoritative; the grid never decides
# existence.

new_dock_grid <- function(grid = NULL, active_group = NULL) {

  if (is.null(grid)) {
    grid <- build_grid_tree(NULL)
  }

  canon <- canonicalize_grid(grid, active_group)
  grid <- canon[["grid"]]

  content <- list(grid = grid)

  if (length(grid_panel_ids(grid))) {
    content[["activeGroup"]] <- coal(
      canon[["active_group"]], "1",
      fail_all = FALSE
    )
  }

  structure(content, class = "dock_grid")
}

# The canonical form, computed directly on the tree (never via the wire spec):
# each branch's child sizes are normalised to ratios summing to 1 and leaf ids
# are reassigned deterministically in walk order, so two grids of the same
# shape compare `identical()`. `activeGroup` names a leaf, so the focused
# group is tracked across the re-id by its old id and handed back.
canonicalize_grid <- function(grid, active_group = NULL) {

  if (is.null(grid) || is.null(grid[["root"]])) {
    return(list(grid = grid, active_group = NULL))
  }

  gid <- 0L
  new_active <- NULL

  walk <- function(node) {

    if (identical(node[["type"]], "leaf")) {

      gid <<- gid + 1L
      nid <- as.character(gid)

      if (identical(node[["data"]][["id"]], active_group)) {
        new_active <<- nid
      }

      node[["data"]][["id"]] <- nid

      return(node)
    }

    kids <- lapply(node[["data"]], walk)
    sizes <- normalise_sizes(dbl_xtr(kids, "size"))

    for (i in seq_along(kids)) {
      kids[[i]][["size"]] <- sizes[[i]]
    }

    node[["data"]] <- kids

    node
  }

  grid[["root"]] <- walk(grid[["root"]])

  list(grid = grid, active_group = new_active)
}

#' Canonical view grid
#'
#' A `dock_grid` is a view's geometry in canonical form -- the panel tree with
#' sizes normalised to 0-1 ratios and stable leaf ids, so two casts of the same
#' layout compare `identical()`. It is authored with [dock_grid()][layout] and
#' produced by `as_dock_grid()`, which casts another `dock_grid` (identity) or a
#' [dock_layout][dock-layout] (dockView's client echo) into it and is
#' idempotent. `is_dock_grid()` is the class check; `validate_dock_grid()`
#' returns its input and errors on a malformed or non-canonical grid.
#'
#' @param x Object to cast (a `dock_grid` or a [dock_layout][dock-layout]),
#'   validate, or test.
#' @param ... Passed on to methods.
#' @return `as_dock_grid()` and `validate_dock_grid()` a `dock_grid`;
#'   `is_dock_grid()` a boolean.
#' @name dock-grid
#' @export
as_dock_grid <- function(x, ...) {
  UseMethod("as_dock_grid")
}

#' @rdname dock-grid
#' @export
is_dock_grid <- function(x) {
  inherits(x, "dock_grid")
}

#' @rdname dock-grid
#' @export
validate_dock_grid <- function(x) {

  if (!is_dock_grid(x)) {
    blockr_abort(
      "Expecting a `dock_grid` object.",
      class = "dock_grid_structure_invalid"
    )
  }

  if (!is.list(x) || !"grid" %in% names(x)) {
    blockr_abort(
      "A `dock_grid` must carry a `grid` component.",
      class = "dock_grid_structure_invalid"
    )
  }

  unexpected <- setdiff(names(x), c("grid", "activeGroup"))

  if (length(unexpected)) {
    blockr_abort(
      "Unexpected `dock_grid` component{?s} {unexpected}.",
      class = "dock_grid_structure_invalid"
    )
  }

  if (!grid_sizes_canonical(x[["grid"]])) {
    blockr_abort(
      "A `dock_grid` must be canonical: each branch's sizes sum to 1.",
      class = "dock_grid_not_canonical"
    )
  }

  invisible(x)
}

#' @export
as_dock_grid.dock_grid <- function(x, ...) x

# Each branch splits its extent among its direct children, so their sizes are
# ratios summing to 1; a leaf carries no children to constrain. Empty branches
# (no children) are vacuously canonical.
grid_sizes_canonical <- function(tree) {

  ok <- TRUE

  walk <- function(node) {

    if (!identical(node[["type"]], "branch")) {
      return(invisible())
    }

    kids <- node[["data"]]

    if (length(kids) && !isTRUE(all.equal(sum(dbl_xtr(kids, "size")), 1))) {
      ok <<- FALSE
    }

    lapply(kids, walk)

    invisible()
  }

  walk(tree[["root"]])

  ok
}

# The sash-position noise floor, an absolute tolerance on the 0-1 size ratios: a
# window resize re-derives ratios from integer pixels and jitters them well
# under a per-cent, so half a per-cent absorbs that noise without eating a
# deliberate drag. The default is a guess, so it is a `blockr_option()` -- tune
# it with `options(blockr.dock_grid_size_tol = ...)`, no code change needed.
grid_size_tol <- function() {
  blockr_option("dock_grid_size_tol", 0.005)
}

# Approximate grid equality: structure exact, relative sizes within the supplied
# `tolerance`. Deferring to `all.equal` keeps the stored sizes faithful (only
# the comparison is fuzzy); comparing the unclassed list walks each pane's size
# individually, and `scale = 1` makes the tolerance absolute on the ratio scale.
# The R-default tolerance stays near-exact, so `all.equal()` / `expect_equal()`
# on a grid is unaffected unless a caller passes a tolerance (the mirror does).
#' @export
all.equal.dock_grid <- function(target, current, ..., scale = 1) {
  all.equal(unclass(target), unclass(current), ..., scale = scale)
}

#' @export
str_value.dock_grid <- function(x, ...) {
  str_value_ids(x, "dock_grid")
}

#' @importFrom utils str
#' @export
str.dock_grid <- function(object, ...) {
  cat(" ", str_value(object), "\n", sep = "")
  invisible(object)
}

#' @export
format.dock_grid <- function(x, ..., bare = TRUE) {
  format_grid_tree(x, "dock_grid", bare)
}

#' @export
print.dock_grid <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

# Drop from a grid every panel that is not one of `members`, returning the
# raw (not-yet-canonical) structure. Shared by the read-time placement
# (`place_members()`) and the construction-time cleaner (`restrict_grid()`).
drop_non_members <- function(grid, members) {
  drop_panels_from_layout(grid, setdiff(layout_panel_ids(grid), members))
}

# Restrict a grid to `members`, dropping ghosts and unknowns, as a canonical
# `dock_grid`. The constructor's grid cleaner: it removes, never adds, so a
# member the grid omits stays absent here (defaulted only where the placement
# is read, in `place_members()`).
restrict_grid <- function(grid, members) {

  dropped <- drop_non_members(grid, members)

  new_dock_grid(dropped[["grid"]], active_group = dropped[["activeGroup"]])
}

# The member-driven placement of a view: membership decides *which* panels
# appear, the grid only *how*. A ghost (grid panel no longer a member) is
# dropped; a member the grid omits is appended a default single-panel spot;
# the constructor renormalises the surviving branches and re-ids. Membership
# is authoritative -- the grid never adds or withholds a panel.
place_members <- function(grid, members) {

  dropped <- drop_non_members(grid, members)

  missing <- setdiff(members, grid_panel_ids(dropped[["grid"]]))

  new_dock_grid(
    append_default_leaves(dropped[["grid"]], missing),
    active_group = dropped[["activeGroup"]]
  )
}

default_leaf <- function(pid, size = 1) {
  list(
    type = "leaf",
    data = list(views = list(pid), activeView = pid, id = "0"),
    size = size
  )
}

# Append members the grid never placed as fresh single-panel leaves at the
# root, each sized to the mean of the existing panes so a newcomer blends in
# rather than dominating; the constructor renormalises and re-ids. The
# "default a member the grid omits" half of `place_members()`.
append_default_leaves <- function(tree, pids) {

  if (!length(pids)) {
    return(tree)
  }

  root <- tree[["root"]]

  existing <- if (not_null(root) && identical(root[["type"]], "branch")) {
    dbl_xtr(root[["data"]], "size")
  } else {
    numeric()
  }

  size <- if (length(existing)) mean(existing) else 1
  leaves <- lapply(pids, default_leaf, size = size)

  if (is.null(root) || !length(root)) {
    root <- list(type = "branch", data = leaves, size = 1)
  } else if (identical(root[["type"]], "leaf")) {
    root <- list(type = "branch", data = c(list(root), leaves), size = 1)
  } else {
    root[["data"]] <- c(root[["data"]], leaves)
  }

  tree[["root"]] <- root

  tree
}

new_dock_grids <- function(x = list()) {
  structure(x, class = "dock_grids")
}

#' @rdname view
#' @export
is_dock_grids <- function(x) {
  inherits(x, "dock_grids")
}

#' @param views A `dock_views` collection, used to check that grids key
#'   known views.
#' @rdname view
#' @export
validate_dock_grids <- function(x, views = NULL) {

  if (is.null(x)) {
    return(x)
  }

  if (!is_dock_grids(x) || !is.list(x)) {
    blockr_abort(
      "Expecting a `dock_grids` object or `NULL`.",
      class = "dock_grids_structure_invalid"
    )
  }

  ids <- names(x)

  if (length(x) && (is.null(ids) || any(ids == ""))) {
    blockr_abort(
      "All grids must be keyed by view id.",
      class = "dock_grids_ids_missing"
    )
  }

  if (not_null(views)) {

    unknown <- setdiff(ids, names(views))

    if (length(unknown)) {
      blockr_abort(
        "Grid{?s} {unknown} reference no known view.",
        class = "dock_grids_unknown_view"
      )
    }
  }

  for (id in ids) {

    grid <- x[[id]]

    if (is.null(grid)) {
      next
    }

    validate_dock_grid(grid)
  }

  x
}

#' @rdname view
#' @export
board_grids <- function(x) {
  stopifnot(is_dock_board(x))
  x[["grids"]]
}

#' @rdname view
#' @export
`board_grids<-` <- function(x, value) {
  stopifnot(is_dock_board(x))
  x[["grids"]] <- validate_dock_grids(value, board_views(x))
  invisible(x)
}

# A view's placement geometry, member-driven: membership is authoritative for
# which panels appear, the grid only for their arrangement. No grid (NULL, or
# a member-less view) falls back to a default over the members; otherwise the
# grid's arrangement is kept for the members it places, a member it omits is
# given a default spot, and a ghost (grid panel no longer a member) is dropped.
# This is where placement is resolved, on read.
view_grid <- function(view, grid) {

  members <- view_members(view)

  if (is.null(grid) || !length(members)) {
    default_grid(members)
  } else {
    place_members(grid, members)
  }
}

# The active view's placement grid: which view is active is `active_view()`,
# its geometry an index into `board_grids()` (NULL where unexpressed, so
# `view_grid()` falls back to a default over the members).
active_view_grid <- function(board) {

  id <- active_view(board)

  view_grid(board_views(board)[[id]], board_grids(board)[[id]])
}

default_grid <- function(members) {

  if (!length(members)) {
    return(new_dock_grid())
  }

  new_dock_grid(build_grid_tree(as.list(members)))
}


coerce_dock_grids <- function(grids, id_map) {

  if (is.null(grids)) {
    return(new_dock_grids())
  }

  if (is_dock_grids(grids)) {
    return(grids)
  }

  new_dock_grids(lapply(grids, resolve_grid, id_map = id_map))
}

# Restrict each grid to the (already cleaned) membership of the view it keys,
# dropping ghosts and unknown panels so stored geometry never outlives the
# board. Grids keyed by an unknown view are left for validation to reject.
restrict_grids_to_views <- function(grids, views) {

  for (id in names(grids)) {

    grid <- grids[[id]]

    if (not_null(grid) && id %in% names(views)) {
      grids[[id]] <- restrict_grid(grid, view_members(views[[id]]))
    }
  }

  grids
}
