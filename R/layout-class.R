#' @param grid,panels,active_group Layout components
#' @rdname dock
#' @export
new_dock_layout <- function(grid = NULL, panels = NULL, active_group = NULL) {

  if (is.null(grid)) {
    grid <- list(
      root = list(type = "branch", data = list(), size = 0L),
      width = 0L,
      height = 0L,
      orientation = "HORIZONTAL"
    )
  }

  if (is.null(panels)) {
    panels <- set_names(list(), character(0L))
  }

  content <- list(grid = grid, panels = panels)

  if (not_null(active_group)) {
    content <- c(content, list(activeGroup = active_group))
  }

  validate_dock_layout(
    structure(content, class = "dock_layout")
  )
}

#' @rdname dock
#' @export
is_dock_layout <- function(x) {
  inherits(x, "dock_layout")
}

is_empty_layout <- function(x) length(x[["panels"]]) == 0L

#' @param blocks Block IDs
#' @rdname dock
#' @export
validate_dock_layout <- function(x, blocks = character()) {

  if (is.null(x)) {
    return(x)
  }

  if (!is.list(x) || !is_dock_layout(x)) {
    blockr_abort(
      "Expecting the `layout` component of a `dock_board` to be a list ",
      "inheriting from `dock_layout`.",
      class = "dock_layout_invalid"
    )
  }

  required <- c("grid", "panels")

  if (!all(required %in% names(x))) {
    blockr_abort(
      "Expecting a `layout` to contain component{?s} {required}.",
      class = "dock_layout_invalid"
    )
  }

  unexpected <- setdiff(names(x), c(required, "activeGroup"))

  if (length(unexpected)) {
    blockr_abort(
      "Not expecting `layout` component{?s} {unexpected}.",
      class = "dock_layout_invalid"
    )
  }

  panel_ids <- chr_xtr(x[["panels"]], "id")

  is_blk_pn <- is_block_panel_id(panel_ids)
  is_ext_pn <- is_ext_panel_id(panel_ids)

  if (!all(is_blk_pn | is_ext_pn)) {
    blockr_abort(
      "Malformed layout panel ID{?s} {panel_ids[!is_id_ok]}.",
      class = "dock_layout_invalid"
    )
  }

  if (length(blocks)) {

    extra <- setdiff(panel_ids[is_blk_pn], block_panel_id(blocks))

    if (length(extra)) {
      blockr_abort(
        "Unknown layout panel{?s} {extra}.",
        class = "dock_layout_invalid"
      )
    }
  }

  x
}

#' @rdname dock
#' @export
as_dock_layout <- function(x, ...) {
  UseMethod("as_dock_layout")
}

#' @export
as_dock_layout.dock_layout <- function(x, ...) x

#' @export
as_dock_layout.board <- function(x, ...) {
  dock_layout(x)
}

#' @export
as_dock_layout.list <- function(x, ...) {

  if ("activeGroup" %in% names(x)) {
    names(x)[names(x) == "activeGroup"] <- "active_group"
  }

  do.call(new_dock_layout, x)
}

#' @rdname dock
#' @export
layout_panel_block_ids <- function(x) {
  x <- as_dock_layout(x)
  sub("^block-", "", chr_xtr(x[["panels"]], "id"))
}

strip_params <- function(x) {

  x <- as_dock_layout(x)

  x[["panels"]] <- lapply(
    x[["panels"]],
    function(y) {
      y[["params"]] <- list()
      y
    }
  )

  x
}
