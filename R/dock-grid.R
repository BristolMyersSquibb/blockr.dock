# A view's geometry has two representations: dockView's `_state` (the client's
# live grid, in volatile leaf ids and absolute pixel sizes) and ours, a
# canonical `dock_grid`. `as_dock_grid()` is the cast between them -- it
# round-trips through the wire spec, regenerating leaf ids, eliding a leaf's
# active-tab-is-first and the load-default focus, and normalising sizes to 0-1
# ratios -- so a `dock_grid` is canonical by construction and two casts of the
# same layout are `identical()`. The mirror stores one on every settled echo.
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
# placement is read (`compose_layouts()`), not by a live writer. A view with no
# grid at all falls back to `default_grid()` over its members.

new_dock_grid <- function(x) {

  grid <- spec_to_layout(layout_to_spec(x))

  structure(grid, class = c("dock_grid", class(grid)))
}

#' Canonical view grid
#'
#' A `dock_grid` is a view's geometry in canonical form -- the panel tree with
#' sizes normalised to 0-1 ratios and stable leaf ids, so two casts of the same
#' layout compare `identical()`. It is a subclass of [dock_layout()] produced
#' only by `as_dock_grid()`, which casts a `dock_layout` or dockView's `_state`
#' shape (a client echo) into it and is idempotent. `is_dock_grid()` is the
#' inheritance check; `validate_dock_grid()` returns its input and errors on a
#' malformed or non-canonical grid.
#'
#' @param x Object to cast (a `dock_layout` or a dockView `_state` list),
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

  validate_dock_layout(x)

  if (!identical(x, new_dock_grid(x))) {
    blockr_abort(
      "A `dock_grid` must be a canonical fixed point (see `as_dock_grid()`).",
      class = "dock_grid_not_canonical"
    )
  }

  invisible(x)
}

#' @export
as_dock_grid.dock_grid <- function(x, ...) x

#' @export
as_dock_grid.dock_layout <- function(x, ...) new_dock_grid(x)

#' @export
as_dock_grid.list <- function(x, ...) new_dock_grid(dockview_to_layout(x))

# The sash-position noise floor, an absolute tolerance on the 0-1 size ratios: a
# window resize re-derives ratios from integer pixels and jitters them well
# under a per-cent, so half a per-cent absorbs that noise without eating a
# deliberate drag. The default is a guess, so it is a `blockr_option()` -- tune
# it with `options(blockr.dock_grid_size_tol = ...)`, no code change needed.
grid_size_tol <- function() {
  blockr_option("dock_grid_size_tol", 0.005)
}

# Approximate layout equality: structure exact, relative sizes within the
# supplied `tolerance`. Deferring to `all.equal` keeps the stored sizes faithful
# (only the comparison is fuzzy); comparing the unclassed list walks each pane's
# size individually, and `scale = 1` makes the tolerance absolute on the ratio
# scale. The R-default tolerance stays near-exact, so `all.equal()` /
# `expect_equal()` on a layout is unaffected unless a caller passes a tolerance
# (the mirror does).
#' @export
all.equal.dock_layout <- function(target, current, ..., scale = 1) {
  all.equal(unclass(target), unclass(current), ..., scale = scale)
}

# Restrict a stored grid to the panels a view actually holds, dropping inert
# ghosts (grid panels no longer in membership), and demote it to a plain
# `dock_layout` -- the composed handle, no longer a canonical stored grid.
# Boundary hygiene for `compose_layouts()`: an un-landed member (in membership,
# absent from the grid) is left out, so the composed handle shows the
# intersection.
restrict_grid <- function(grid, members) {

  restricted <- drop_panels_from_layout(
    grid,
    setdiff(layout_panel_ids(grid), members)
  )

  class(restricted) <- setdiff(class(restricted), "dock_grid")

  restricted
}
