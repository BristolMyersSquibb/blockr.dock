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
  spec_to_layout(layout_to_spec(strip_provenance(layout)))
}

# Provenance records who wrote a grid value: `authored` for the
# constructor / deserialization push, `echo` for the settled-echo mirror. It
# makes "the mirror is the sole echo writer" runtime-checkable and never
# crosses the wire (stripped before serialization, re-stamped on read).
grid_provenance <- function(x) {
  attr(x, "provenance", exact = TRUE)
}

`grid_provenance<-` <- function(x, value) {
  stopifnot(is_string(value), value %in% c("authored", "echo"))
  attr(x, "provenance") <- value
  x
}

strip_provenance <- function(x) {

  if (is.null(x)) {
    return(NULL)
  }

  attr(x, "provenance") <- NULL
  x
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
# size individually, `scale = 1` makes the tolerance absolute on the ratio
# scale, and provenance is dropped as runtime bookkeeping. The R-default
# tolerance stays near-exact, so `all.equal()` / `expect_equal()` on a layout is
# unaffected unless a caller passes a tolerance (the mirror does).
#' @export
all.equal.dock_layout <- function(target, current, ..., scale = 1) {
  all.equal(
    unclass(strip_provenance(target)),
    unclass(strip_provenance(current)),
    ...,
    scale = scale
  )
}

# Canonicalise a raw layout (a client echo or a constructor / restore layout)
# into a stored grid: drop volatile ids / default tabs / focus, and elide a
# plain default -- an even split of its own panels, in order -- to NULL. Sizes
# ride through verbatim. Idempotent, so it is safe as both the mirror's write
# projection and the fixed point the grid slot enforces.
project_grid <- function(layout, provenance = "echo") {

  canon <- canonicalize_grid(layout)

  if (is_default_grid(canon)) {
    return(NULL)
  }

  grid_provenance(canon) <- provenance
  canon
}

is_default_grid <- function(layout) {
  identical(
    strip_provenance(layout),
    canonicalize_grid(default_grid(layout_panel_ids(layout)))
  )
}

# Restrict a stored grid to the panels a view actually holds, dropping
# inert ghosts (grid panels no longer in membership). Boundary hygiene
# for `compose_layouts()` -- an un-landed member (in membership, absent from the
# grid) is left out, so the composed handle shows the intersection.
restrict_grid <- function(arr, members) {
  drop_panels_from_layout(
    strip_provenance(arr), setdiff(layout_panel_ids(arr), members)
  )
}

# A grid value is well-formed when it is a canonical fixed point of the
# projection and carries a valid provenance. Checked on every committed board
# (via validate_dock_grids), so a non-canonical or unstamped write is a
# classed error rather than a source of re-echo churn.
validate_grid_value <- function(arr, id) {

  prov <- grid_provenance(arr)

  if (!is_string(prov) || !prov %in% c("authored", "echo")) {
    blockr_abort(
      "Grid for view {id} carries no valid provenance.",
      class = "dock_grid_provenance_invalid"
    )
  }

  if (!identical(strip_provenance(arr), canonicalize_grid(arr))) {
    blockr_abort(
      "Grid for view {id} is not a canonical fixed point.",
      class = "dock_grid_not_canonical"
    )
  }

  invisible(arr)
}
