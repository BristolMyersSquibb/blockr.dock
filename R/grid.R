# The grid mirror stores a view's grid geometry as a one-directional
# echo of the client's settled `_state`. Two properties make the echo a stable
# fixed point, so a re-echoed but unchanged layout produces no board commit:
#
#   * Volatile detail is dropped by round-tripping through the wire spec, which
#     regenerates leaf ids, elides a leaf's active-tab-is-first and the
#     load-default focus, and drops an even split's sizes.
#   * Relative sizes are quantised to a coarse grid, above the pixel-rounding
#     jitter a window resize introduces (~1 per-mille) and below deliberate
#     user intent (~1 per-cent), so two echoes of the same drag canonicalise
#     identically.
#
# `project_grid()` elides a plain default (an even split of its own
# panels, in order) to `NULL`. It does not restrict to membership: the mirror
# stores the settled echo verbatim, and the two slots are related by total
# semantics -- a member absent from the arrangement is an un-landed intent, a
# panel absent from membership an inert ghost -- reconciled only at the compose
# / restore boundary, never by a live writer.

# 0.5% grid: coarse enough to absorb the sub-per-mille jitter of a window
# resize, fine enough to preserve a deliberate sash drag.
size_quantum_denom <- function() 200L

quantize_sizes <- function(sizes) {

  if (length(sizes) < 2L) {
    return(sizes)
  }

  grid <- size_quantum_denom()
  counts <- round(normalise_sizes(sizes) * grid)
  counts[counts < 1L] <- 1L

  top <- which.max(counts)
  counts[top] <- counts[top] + (grid - sum(counts))

  counts / grid
}

# Quantise every branch's sizes in a wire spec, dropping the sizes key when
# quantisation lands on an even split. Leaves (bare strings, tabbed `panels`)
# carry no sizes; the root branch and nested branches carry `children`.
quantize_spec <- function(node) {

  if (is.character(node) || not_null(node[["panels"]])) {
    return(node)
  }

  if (not_null(node[["children"]])) {

    node[["children"]] <- lapply(node[["children"]], quantize_spec)

    if (not_null(node[["sizes"]])) {
      quant <- quantize_sizes(node[["sizes"]])
      node[["sizes"]] <- if (sizes_are_even(quant)) NULL else quant
    }
  }

  node
}

canonicalize_grid <- function(layout) {
  spec_to_layout(quantize_spec(layout_to_spec(strip_provenance(layout))))
}

# Provenance records who wrote an arrangement value: `authored` for the
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

# Canonicalise a raw layout (a client echo or a constructor / restore layout)
# into a stored arrangement: quantise sizes, drop volatile ids / default tabs /
# focus, and elide a plain default -- an even split of its own panels, in order
# -- to NULL. Idempotent, so it is safe as both the mirror's write projection
# and the fixed point the arrangement slot enforces.
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

# Restrict a stored arrangement to the panels a view actually holds, dropping
# inert ghosts (arrangement panels no longer in membership). Boundary hygiene
# for `compose_layouts()` -- an un-landed member (in membership, absent from the
# arrangement) is left out, so the composed handle shows the intersection.
restrict_grid <- function(arr, members) {
  drop_panels_from_layout(
    strip_provenance(arr), setdiff(layout_panel_ids(arr), members)
  )
}

# An arrangement value is well-formed when it is a canonical fixed point of the
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
