# A view's geometry has two representations: dockView's `_state` (the client's
# live grid, in volatile leaf ids and absolute pixel sizes, wrapped at the seam
# as a `dock_layout`) and ours, a canonical `dock_grid`. `as_dock_grid()` is the
# cast into ours -- from another `dock_grid` (identity) or from a `dock_layout`,
# which it round-trips through the wire spec, regenerating leaf ids, eliding a
# leaf's active-tab-is-first and the load-default focus, and normalising sizes
# to 0-1 ratios. A `dock_grid` is therefore canonical by construction and two
# casts of the same layout are `identical()`. The mirror stores one on every
# settled echo.
#
# Relative sizes ride through verbatim (faithful for serialization and
# programmatic resize); the commit guard tolerates their pixel-rounding jitter
# via `all.equal(tolerance = grid_size_tol())`, so a window resize is absorbed
# while a deliberate sash drag still commits.
#
# The grid stores geometry verbatim -- never a default detected after the fact
# and projected away. A view's placement is the intersection of its membership
# and its grid: a member absent from the grid is an in-flight add, a panel
# absent from membership an inert ghost, and both are dropped where the
# placement is read (`view_grid()`), not by a live writer. A view with no grid
# at all falls back to `default_grid()` over its members.

new_dock_grid <- function(grid = NULL, active_group = NULL) {

  if (is.null(grid)) {
    grid <- build_grid_tree(NULL)
  }

  content <- list(grid = grid)

  if (length(grid_panel_ids(grid))) {
    content[["activeGroup"]] <- coal(active_group, "1")
  }

  structure(content, class = "dock_grid")
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

# Restrict a stored grid to the panels a view actually holds, dropping inert
# ghosts (grid panels no longer in membership), then re-canonicalise so the
# surviving branches' sizes sum to 1 again. Boundary hygiene for `view_grid()`:
# an un-landed member (in membership, absent from the grid) is simply not in
# the grid, so the placement shows the intersection.
restrict_grid <- function(grid, members) {

  dropped <- drop_panels_from_layout(
    grid,
    setdiff(layout_panel_ids(grid), members)
  )

  spec_to_layout(layout_to_spec(dropped))
}
