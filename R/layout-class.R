#' Dock layout
#'
#' A `dock_layout` is the panel arrangement for a single view: a tree of
#' block / extension IDs, with at most one leaf marked as initially
#' active. A board holds a `dock_layouts` collection (one layout per
#' view); panel content is derived on demand from the board's blocks and
#' extensions, so only the arrangement is stored in a `dock_layout`. See
#' [is_dock_layouts()] for the collection-level helpers.
#'
#' The constructor `dock_layout(...)` accepts bare IDs and nested lists
#' for non-trivial arrangements. Pass `active = TRUE` to mark this
#' layout as the initially-active view in a `dock_layouts` collection.
#' `default_layout(blocks, extensions)` produces the default two-row
#' arrangement (extensions on top, blocks below) for a board.
#'
#' A *view* is the conceptual page-level container; a *layout* is the
#' panel arrangement inside a view. The dockview-shape `grid + panels`
#' payload that dockViewR consumes is an internal projection of a
#' `dock_layout` against the board's blocks and extensions; it is not a
#' public type.
#'
#' @param ... For `dock_layout()`, block / extension IDs (single strings
#'   or nested lists for non-trivial arrangements). For `as_dock_layout()`,
#'   reserved for generic consistency.
#' @param active Logical; mark this layout as the initially-active view.
#'
#' @examples
#' blks <- c(
#'   a = blockr.core::new_dataset_block(),
#'   b = blockr.core::new_head_block()
#' )
#'
#' exts <- list(
#'   edit = new_edit_board_extension()
#' )
#'
#' default_layout(blks, exts)
#'
#' dock_layout("a", "b", active = TRUE)
#'
#' @return `dock_layout()` and `default_layout()` return a `dock_layout`
#' object. `as_dock_layout()` coerces a board (returning its active
#' view's layout) or a serialized payload. `is_dock_layout()` returns
#' a boolean. `validate_dock_layout()` returns its input and throws
#' on error.
#'
#' @rdname layout
#' @export
dock_layout <- function(..., active = FALSE) {
  new_dock_layout(
    grid = draw_panel_tree(list(...)),
    active = active
  )
}

new_dock_layout <- function(grid = NULL, active_group = NULL,
                            active = FALSE) {

  if (is.null(grid)) {
    grid <- draw_panel_tree(NULL)
  }

  content <- list(grid = grid)

  if (length(grid_panel_ids(grid))) {
    content[["activeGroup"]] <- active_group %||% "1"
  }

  set_active_view(
    validate_dock_layout(
      structure(content, class = "dock_layout")
    ),
    active
  )
}

#' @param x Object
#' @rdname layout
#' @export
is_dock_layout <- function(x) {
  inherits(x, "dock_layout")
}

#' @rdname layout
#' @export
as_dock_layout <- function(x, ...) {
  UseMethod("as_dock_layout")
}

#' @export
as_dock_layout.dock_layout <- function(x, ...) x

#' @export
as_dock_layout.board <- function(x, ...) {
  active_layout(x)
}

#' @export
as_dock_layout.list <- function(x, ...) {
  active <- isTRUE(attr(x, "active"))
  active_group <- x[["activeGroup"]] %||% x[["active_group"]]
  set_active_view(
    new_dock_layout(grid = x[["grid"]], active_group = active_group),
    active
  )
}

#' @param blocks,extensions Dock board components
#' @rdname layout
#' @export
default_layout <- function(blocks, extensions) {

  blks <- names(as_blocks(blocks))
  exts <- names(as_dock_extensions(extensions))

  spec <- if (length(exts)) {
    list(exts, blks)
  } else if (length(blks)) {
    list(blks)
  } else {
    list()
  }

  do.call(dock_layout, spec)
}

#' @rdname layout
#' @export
validate_dock_layout <- function(x, blocks = character()) {

  if (is.null(x)) {
    return(x)
  }

  if (is_blocks(blocks)) {
    blocks <- names(blocks)
  }

  if (!is.list(x) || !is_dock_layout(x)) {
    blockr_abort(
      "Expecting the `layout` component of a `dock_board` to be a list ",
      "inheriting from `dock_layout`.",
      class = "dock_layout_invalid"
    )
  }

  if (!"grid" %in% names(x)) {
    blockr_abort(
      "Expecting a `layout` to contain a `grid` component.",
      class = "dock_layout_invalid"
    )
  }

  unexpected <- setdiff(names(x), c("grid", "activeGroup"))

  if (length(unexpected)) {
    blockr_abort(
      "Not expecting `layout` component{?s} {unexpected}.",
      class = "dock_layout_invalid"
    )
  }

  panel_ids <- layout_panel_ids(x)

  is_blk_pn <- maybe_block_panel_id(panel_ids)
  is_ext_pn <- maybe_ext_panel_id(panel_ids)

  raw_ids <- !(is_blk_pn | is_ext_pn)

  if (length(blocks) && any(raw_ids)) {
    blockr_abort(
      "Malformed layout panel ID{?s} {panel_ids[raw_ids]}.",
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

draw_panel_tree <- function(x) {

  group_id <- 0L

  new_leaf <- function(views, size = 1) {

    group_id <<- group_id + 1L

    list(
      type = "leaf",
      data = list(
        views = as.list(views),
        activeView = views[1L],
        id = as.character(group_id)
      ),
      size = size
    )
  }

  new_branch <- function(x, size = 1) {
    list(type = "branch", data = filter_empty(x), size = size)
  }

  draw_tree <- function(x, size = 1) {

    if (is.list(x)) {

      if (length(x)) {
        size <- 1 / length(x)
      } else {
        size <- 0
      }

      new_branch(lapply(x, draw_tree, size = size), size)

    } else {

      new_leaf(x, size)
    }
  }

  if (is.null(x)) {
    x <- list()
  } else if (is.character(x)) {
    x <- as.list(x)
  }

  list(
    root = draw_tree(x),
    orientation = "HORIZONTAL"
  )
}

grid_panel_ids <- function(grid) {

  if (is.null(grid) || !length(grid[["root"]])) {
    return(character())
  }

  walk <- function(node) {
    if (is.null(node) || !length(node)) {
      return(NULL)
    }
    if (identical(node[["type"]], "leaf")) {
      return(unlist(node[["data"]][["views"]]))
    }
    unlist(lapply(node[["data"]], walk))
  }

  res <- walk(grid[["root"]])

  if (is.null(res)) character() else res
}

layout_panel_ids <- function(x) {
  x <- as_dock_layout(x)
  grid_panel_ids(x[["grid"]])
}

resolve_dock_layout <- function(blocks = list(), extensions = list(),
                                layout = default_layout(blocks, extensions)) {

  blocks <- as_blocks(blocks)
  ext_coll <- as_dock_extensions(extensions)

  layout <- as_dock_layout(layout)

  id_map <- set_names(
    c(as_ext_panel_id(ext_coll), as_block_panel_id(blocks)),
    c(names(ext_coll), names(blocks))
  )

  panel_ids <- grid_panel_ids(layout[["grid"]])

  if (length(panel_ids) &&
        all(panel_ids %in% names(id_map)) &&
        any(!panel_ids %in% id_map)) {

    if (anyDuplicated(names(id_map)) > 0L) {

      blockr_warn(
        "Cannot use extension names that overlap with block names.",
        class = "extension_block_name_clash"
      )

      was_active <- is_active_view(layout)

      spec <- list()
      if (length(ext_coll)) {
        spec <- c(spec, list(as.character(as_ext_panel_id(ext_coll))))
      }
      if (length(blocks)) {
        spec <- c(spec, list(as.character(as_block_panel_id(blocks))))
      }

      layout <- set_active_view(do.call(dock_layout, spec), was_active)

    } else {

      layout[["grid"]] <- rewrite_grid_leaves(layout[["grid"]], id_map)
    }
  }

  validate_dock_layout(layout, blocks)
}

rewrite_grid_leaves <- function(grid, id_map) {

  walk <- function(node) {
    if (is.null(node) || !length(node)) {
      return(node)
    }
    if (identical(node[["type"]], "leaf")) {
      views <- chr_ply(node[["data"]][["views"]], identity)
      mapped <- chr_ply(views, function(v) id_map[[v]] %||% v)
      node[["data"]][["views"]] <- as.list(mapped)
      node[["data"]][["activeView"]] <- mapped[1L]
      return(node)
    }
    node[["data"]] <- lapply(node[["data"]], walk)
    node
  }

  grid[["root"]] <- walk(grid[["root"]])
  grid
}

create_layout_panel <- function(x) {

  remove <- x[["remove"]]

  if (isTRUE(remove[["enable"]])) {
    tab_component <- "manual"
  } else {
    tab_component <- "custom"
  }

  if (isTRUE(remove[["enable"]]) && not_null(remove[["callback"]])) {
    remove_callback <- list(
      `__IS_FUNCTION__` = TRUE,
      source = unclass(remove[["callback"]])
    )
  } else {
    remove_callback <- NULL
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

dockview_payload <- function(layout, blocks = list(), extensions = list()) {

  layout <- as_dock_layout(layout)
  blocks <- as_blocks(blocks)
  ext_list <- as.list(as_dock_extensions(extensions))

  panel_ids <- grid_panel_ids(layout[["grid"]])

  blk_pids <- panel_ids[maybe_block_panel_id(panel_ids)]
  ext_pids <- panel_ids[maybe_ext_panel_id(panel_ids)]

  blk_ids <- sub("^block_panel-", "", blk_pids)
  ext_ids <- sub("^ext_panel-", "", ext_pids)

  blk_panels <- lapply(blk_ids, function(id) block_panel(blocks[id]))
  ext_panels <- lapply(ext_ids, function(id) ext_panel(ext_list[[id]]))

  panels <- lapply(c(blk_panels, ext_panels), create_layout_panel)
  names(panels) <- chr_xtr(panels, "id")

  out <- list(grid = layout[["grid"]], panels = panels)

  if (!is.null(layout[["activeGroup"]])) {
    out[["activeGroup"]] <- layout[["activeGroup"]]
  }

  out
}
