#' Dock layout: dockView's native representation
#'
#' A `dock_layout` is dockView's own layout representation -- the
#' `{grid, panels, activeGroup}` payload it renders -- the wire form at the
#' client boundary. It is distinct from our [dock_grid][dock-grid] (a view's
#' canonical geometry) and [dock_view()][dock_view] (a view's structure): a
#' `dock_layout` is what the browser echoes and what a restore pushes back,
#' geometry fused with resolved panel content in dockView's own shape. It is
#' produced and consumed only at the dockView seam and is never stored on the
#' board nor passed through the update lifecycle.
#'
#' `new_dock_layout()` wraps a raw dockView state / payload list (a client
#' echo) as a `dock_layout`; `is_dock_layout()` is the class check;
#' `validate_dock_layout()` returns its input and errors on a malformed shape.
#' Cast across the seam with `as_dock_layout()` (build the dockView payload
#' from a [dock_grid][dock-grid] against the board's blocks and extensions),
#' [as_dock_grid()] (canonical geometry from a layout) and
#' [as_dock_view()][dock_view] (membership from a layout).
#'
#' @param x Object to wrap, validate, test, or cast.
#' @param ... Passed on to methods (e.g. `blocks` / `extensions` to resolve
#'   panel content when casting a `dock_grid`).
#'
#' @return `new_dock_layout()`, `validate_dock_layout()` and `as_dock_layout()`
#'   return a `dock_layout`; `is_dock_layout()` a boolean.
#'
#' @name dock-layout
#' @export
new_dock_layout <- function(x = list()) {
  structure(x, class = "dock_layout")
}

#' @rdname dock-layout
#' @export
is_dock_layout <- function(x) {
  inherits(x, "dock_layout")
}

#' @rdname dock-layout
#' @export
validate_dock_layout <- function(x) {

  if (!is_dock_layout(x) || !is.list(x)) {
    blockr_abort(
      "Expecting a `dock_layout` object.",
      class = "dock_layout_structure_invalid"
    )
  }

  if (!"grid" %in% names(x)) {
    blockr_abort(
      "A `dock_layout` must carry a `grid` component.",
      class = "dock_layout_structure_invalid"
    )
  }

  active <- x[["activeGroup"]]

  if (not_null(active) && !is_string(active)) {
    blockr_abort(
      "A `dock_layout` `activeGroup` must be a string or `NULL`.",
      class = "dock_layout_structure_invalid"
    )
  }

  if ("panels" %in% names(x) && !is.list(x[["panels"]])) {
    blockr_abort(
      "A `dock_layout` `panels` component must be a list.",
      class = "dock_layout_structure_invalid"
    )
  }

  invisible(x)
}

#' @rdname dock-layout
#' @export
as_dock_layout <- function(x, ...) {
  UseMethod("as_dock_layout")
}

#' @export
as_dock_layout.dock_layout <- function(x, ...) x

#' @export
as_dock_layout.dock_grid <- function(x, blocks = list(), extensions = list(),
                                     ...) {

  blocks <- as_blocks(blocks)
  ext_list <- as.list(as_dock_extensions(extensions))

  panel_ids <- grid_panel_ids(x[["grid"]])

  blk_pids <- panel_ids[maybe_block_panel_id(panel_ids)]
  ext_pids <- panel_ids[maybe_ext_panel_id(panel_ids)]

  blk_ids <- as_obj_id(new_block_panel_id(blk_pids))
  ext_ids <- as_obj_id(new_ext_panel_id(ext_pids))

  blk_panels <- lapply(split(blocks[blk_ids], seq_along(blk_ids)), block_panel)
  ext_panels <- lapply(ext_list[ext_ids], ext_panel)

  panels <- lapply(c(blk_panels, ext_panels), create_layout_panel)
  names(panels) <- chr_xtr(panels, "id")

  out <- list(grid = x[["grid"]], panels = panels)

  if (!is.null(x[["activeGroup"]])) {
    out[["activeGroup"]] <- x[["activeGroup"]]
  }

  new_dock_layout(out)
}

#' @export
as_dock_grid.dock_layout <- function(x, ...) {

  raw <- new_dock_grid(
    x[["grid"]],
    active_group = coal(
      x[["activeGroup"]], x[["active_group"]],
      fail_all = FALSE
    )
  )

  spec_to_layout(layout_to_spec(raw))
}

#' @param ... Forwarded to methods.
#' @rdname view
#' @export
as_dock_view <- function(x, ...) {
  UseMethod("as_dock_view")
}

#' @export
as_dock_view.dock_view <- function(x, ...) x

#' @export
as_dock_view.dock_layout <- function(x, ...) {
  new_dock_view(grid_panel_ids(x[["grid"]]))
}
