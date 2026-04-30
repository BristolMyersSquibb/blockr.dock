#' Dock layout
#'
#' A `dock_layout` object is the dockview-shaped (`grid` + `panels`) panel
#' arrangement of a single view. The low-level constructor
#' `new_dock_layout()` builds it from pre-resolved fields; `as_dock_layout()`
#' coerces a serialized payload back into one.
#'
#' For inline construction inside `new_dock_board(layouts = ...)`, use
#' [dock_grid()] — which builds an unresolved arrangement spec (a
#' `dock_grid`, with the optional `active` flag), to be panel-resolved
#' later when the board's blocks and extensions are known.
#' `default_grid()` produces the default arrangement for a given set
#' of blocks and extensions.
#'
#' A *view* is the conceptual page-level container; a *layout* is the
#' technical panel arrangement inside a view. The board holds an
#' internal `dock_layouts` collection (keyed by view name); at most
#' one entry may be marked as the active (initially-selected) one.
#'
#' @param grid,panels,active_group Layout components
#' @param active Logical; mark this layout/grid as the active
#'   (initially-selected) one within a `dock_layouts`.
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
#' default_grid(blks, exts)
#'
#' dock_grid("a", "b", active = TRUE)
#'
#' @return The low-level constructor `new_dock_layout()` and the coercion
#' function `as_dock_layout()` return a fully-resolved `dock_layout` object.
#' The inline constructor `dock_grid()` returns a `dock_grid` (an unresolved
#' arrangement spec). `default_grid()` returns a `dock_grid` describing the
#' default arrangement. The validator `validate_dock_layout()` returns its
#' input and throws errors as side-effect; inheritance can be checked using
#' `is_dock_layout()` and `is_dock_grid()`, which return booleans.
#'
#' @rdname layout
#' @export
new_dock_layout <- function(grid = NULL, panels = NULL, active_group = NULL,
                            active = FALSE) {

  if (!length(grid)) {
    grid <- draw_panel_tree(NULL)
  }

  if (!length(panels)) {
    panels <- set_names(list(), character(0L))
  }

  content <- list(grid = grid, panels = panels)

  if (length(active_group)) {
    content <- c(content, list(activeGroup = active_group))
  }

  set_active_view(
    validate_dock_layout(
      structure(content, class = "dock_layout")
    ),
    active
  )
}

#' @rdname layout
#' @export
dock_grid <- function(..., active = FALSE) {
  set_active_view(structure(list(...), class = "dock_grid"), active)
}

#' @rdname layout
#' @export
is_dock_grid <- function(x) {
  inherits(x, "dock_grid")
}

#' @rdname layout
#' @export
as_dock_grid <- function(x, ...) {
  UseMethod("as_dock_grid")
}

#' @export
as_dock_grid.dock_grid <- function(x, ...) x

#' @export
as_dock_grid.list <- function(x, ...) {
  active <- isTRUE(attr(x, "active"))
  set_active_view(structure(unname(x), class = "dock_grid"), active)
}

#' @param blocks,extensions Dock board components
#' @rdname layout
#' @export
default_grid <- function(blocks, extensions) {
  build_default_grid(
    blks = names(as_blocks(blocks)),
    exts = names(as_dock_extensions(extensions))
  )
}

build_default_grid <- function(blks, exts) {
  if (length(exts)) {
    structure(list(exts, blks), class = "dock_grid")
  } else if (!length(blks)) {
    structure(list(), class = "dock_grid")
  } else {
    structure(list(blks), class = "dock_grid")
  }
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

resolve_dock_layout <- function(blocks = list(), extensions = list(),
                                grid = default_grid(blocks, extensions)) {

  blocks <- as_blocks(blocks)
  ext_coll <- as_dock_extensions(extensions)

  blk_panels <- lapply(
    names(blocks),
    function(nme) {
      block_panel(blocks[nme])
    }
  )

  id_map <- set_names(
    c(as_ext_panel_id(ext_coll), as_block_panel_id(blocks)),
    c(names(ext_coll), names(blocks))
  )

  ids <- unlist(grid)

  if (all(ids %in% names(id_map)) && any(!ids %in% id_map)) {

    if (anyDuplicated(names(id_map)) > 0L) {

      blockr_warn(
        "Cannot use extension names that overlap with block names.",
        class = "extension_block_name_clash"
      )

      grid <- build_default_grid(
        blks = as_block_panel_id(blocks),
        exts = as_ext_panel_id(ext_coll)
      )

    } else {

      grid <- rapply(
        as.list(grid),
        function(x, map) chr_ply(x, function(i, x) x[[i]], map),
        "character",
        how = "replace",
        map = id_map
      )
    }
  }

  ext_panels <- lapply(ext_coll, ext_panel)

  panels <- lapply(c(blk_panels, ext_panels), create_layout_panel)
  names(panels) <- chr_xtr(panels, "id")

  if (!all(unlist(grid) %in% names(panels))) {
    blockr_abort(
      "Cannot match layout panel IDs to panels.",
      class = "invalid_panel_layout_specification"
    )
  }

  if (length(grid)) {

    grid <- draw_panel_tree(grid)
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

#' @param x Object
#' @rdname layout
#' @export
is_dock_layout <- function(x) {
  inherits(x, "dock_layout")
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

#' @param ... Generic consistency
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
  if ("activeGroup" %in% names(x)) {
    names(x)[names(x) == "activeGroup"] <- "active_group"
  }

  do.call(new_dock_layout, x)
}

layout_panel_ids <- function(x) {
  x <- as_dock_layout(x)
  chr_xtr(x[["panels"]], "id")
}
