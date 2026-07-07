# The grid mirror stores a view's grid geometry as a one-directional echo of
# the client's settled `_state`. Canonicalisation round-trips through the wire
# spec -- regenerating leaf ids, eliding a leaf's active-tab-is-first and the
# load-default focus, dropping an even split's sizes -- so volatile detail never
# triggers a write. Relative sizes are kept verbatim, faithful for serialization
# and programmatic resize; the commit guard instead tolerates them via
# `all.equal(tolerance = grid_size_tol())`, so the pixel-rounding jitter of a
# window resize is absorbed while a deliberate sash drag still commits.
#
# `project_grid()` elides a plain default (an even split of its own panels, in
# order) to `NULL`. It does not restrict to membership: the mirror stores the
# settled echo verbatim, and the two slots are related by total semantics -- a
# member absent from the grid is an un-landed intent, a panel absent from
# membership an inert ghost -- reconciled only at the compose / restore
# boundary, never by a live writer.

canonicalize_grid <- function(layout) {
  spec_to_layout(layout_to_spec(layout))
}

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

# Canonicalise a raw layout (a client echo or a constructor / restore layout)
# into a stored grid: drop volatile ids / default tabs / focus, and elide a
# plain default -- an even split of its own panels, in order -- to NULL. Sizes
# ride through verbatim. Idempotent, so it is safe as both the mirror's write
# projection and the fixed point the grid slot enforces.
project_grid <- function(layout) {

  canon <- canonicalize_grid(layout)

  if (is_default_grid(canon)) {
    return(NULL)
  }

  canon
}

is_default_grid <- function(layout) {
  identical(
    layout,
    canonicalize_grid(default_grid(layout_panel_ids(layout)))
  )
}

# Restrict a stored grid to the panels a view actually holds, dropping inert
# ghosts (grid panels no longer in membership). Boundary hygiene for
# `compose_layouts()` -- an un-landed member (in membership, absent from the
# grid) is left out, so the composed handle shows the intersection.
restrict_grid <- function(grid, members) {
  drop_panels_from_layout(grid, setdiff(layout_panel_ids(grid), members))
}

# A stored grid must be a canonical fixed point of the projection. Checked on
# every committed board (via validate_dock_grids), so a non-canonical write is a
# classed error rather than a source of re-echo churn: a grid that does not
# match its own canonical form would structurally differ from the next echo and
# commit spuriously.
validate_grid_value <- function(grid, id) {

  if (!identical(grid, canonicalize_grid(grid))) {
    blockr_abort(
      "Grid for view {id} is not a canonical fixed point.",
      class = "dock_grid_not_canonical"
    )
  }

  invisible(grid)
}
