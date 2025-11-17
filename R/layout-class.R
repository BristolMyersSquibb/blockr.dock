#' @param grid,panels,active_group Layout components
#' @rdname dock
#' @export
new_dock_layout <- function(grid = NULL, panels = NULL, active_group = NULL) {
  if (!length(grid)) {
    grid <- list(
      root = list(type = "branch", data = list(), size = 0L),
      width = 0L,
      height = 0L,
      orientation = "HORIZONTAL"
    )
  }

  if (!length(panels)) {
    panels <- set_names(list(), character(0L))
  }

  content <- list(grid = grid, panels = panels)

  if (!length(active_group)) {
    content <- c(content, list(activeGroup = active_group))
  }

  validate_dock_layout(
    structure(content, class = "dock_layout")
  )
}

#' @rdname dock
#' @export
default_dock_layout <- function(blocks = list(), extensions = list()) {
  preproc_panel <- function(x) {
    remove <- x[["remove"]]
    tab_component <- if (!remove[["enable"]]) "custom" else "default"
    remove_callback <- NULL
    if (remove[["enable"]] && !is.null(remove[["callback"]])) {
      remove_callback <- list(
        `__IS_FUNCTION__` = TRUE,
        source = unclass(remove[["callback"]])
      )
    }

    c(
      x[c("id", "title")],
      list(
        contentComponent = "default",
        tabComponent = tab_component,
        params = list(
          content = list(html = c(x[["content"]][["html"]])),
          style = x[["style"]],
          removeCallback = remove_callback
        )
      )
    )
  }

  new_leaf <- function(views, id) {
    list(
      type = "leaf",
      data = list(views = as.list(views), activeView = views[1L], id = id),
      size = 0.5
    )
  }

  new_branch <- function(...) {
    list(type = "branch", data = filter_empty(list(...)))
  }

  blk_panels <- lapply(names(blocks), function(nme) {
    block_panel(blocks[nme])
  })
  ext_panels <- lapply(extensions, ext_panel)

  panels <- lapply(c(blk_panels, ext_panels), preproc_panel)
  names(panels) <- chr_xtr(panels, "id")

  if (length(panels)) {
    grid <- new_branch(
      if (length(extensions)) {
        new_leaf(chr_ply(extensions, as_ext_panel_id), id = "1")
      },
      if (length(blocks)) {
        new_leaf(
          unclass(as_block_panel_id(blocks)),
          id = if (length(extensions)) "2" else "1"
        )
      }
    )

    grid <- list(
      root = grid,
      orientation = "HORIZONTAL"
    )

    grup <- "1"
  } else {
    grid <- NULL
    grup <- NULL
  }

  new_dock_layout(
    grid = grid,
    panels = panels,
    active_group = grup
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

  panel_ids <- layout_panel_ids(x)

  is_blk_pn <- maybe_block_panel_id(panel_ids)
  is_ext_pn <- maybe_ext_panel_id(panel_ids)

  if (!all(is_blk_pn | is_ext_pn)) {
    blockr_abort(
      "Malformed layout panel ID{?s} {panel_ids[!(is_blk_pn | is_ext_pn)]}.",
      class = "dock_layout_invalid"
    )
  }

  if (length(blocks)) {
    extra <- setdiff(panel_ids[is_blk_pn], as_block_panel_id(blocks))

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
layout_panel_ids <- function(x) {
  x <- as_dock_layout(x)
  chr_xtr(x[["panels"]], "id")
}
