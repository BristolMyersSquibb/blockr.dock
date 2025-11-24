#' Dock layout
#'
#' The arrangement of panels in a dock can be specified using a `dock_layout`
#' object. A default layout
#'
#' @param grid,panels,active_group Layout components
#'
#' @rdname layout
#' @export
new_dock_layout <- function(grid = NULL, panels = NULL, active_group = NULL) {

  if (!length(grid)) {
    grid <- draw_panel_tree(NULL)
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

#' @param blocks,extensions Dock board components
#' @rdname layout
#' @export
default_layout <- function(blocks, extensions) {
  list(
    as_ext_panel_id(extensions),
    as_block_panel_id(blocks)
  )
}

draw_panel_tree <- function(x) {

  group_id <- 0L

  new_leaf <- function(views, size = 1) {

    group_id <<- group_id + 1L

    res <- list(
      type = "leaf",
      data = list(
        views = as.list(views),
        activeView = views[1L],
        id = as.character(group_id)
      )
    )

    if (size != 1) {
      res <- c(res, list(size = size))
    }

    res
  }

  new_branch <- function(x, size = 1) {

    res <- list(type = "branch", data = filter_empty(x))

    if (size != 1) {
      res <- c(res, list(size = size))
    }

    res
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

#' @param arrangement Panel arrangement
#' @rdname layout
#' @export
create_dock_layout <- function(
  blocks = list(),
  extensions = list(),
  arrangement = default_layout(blocks, extensions)
) {

  blocks <- as_blocks(blocks)

  blk_panels <- lapply(
    names(blocks),
    function(nme) {
      block_panel(blocks[nme])
    }
  )

  if (is_dock_extension(extensions)) {
    ext_names <- as_ext_panel_id(extensions)
  } else {
    ext_names <- names(extensions)
  }

  extensions <- as_dock_extensions(extensions)

  id_map <- set_names(
    c(as_ext_panel_id(extensions), as_block_panel_id(blocks)),
    c(ext_names, names(blocks))
  )

  ids <- unlist(arrangement)

  if (all(ids %in% names(id_map)) && any(!ids %in% id_map)) {

    if (anyDuplicated(id_map) > 0L) {

      blockr_warn(
        "Cannot use extension names that overlap with block names.",
        class = "extension_block_name_clash"
      )

      arrangement <- default_layout(blocks, extensions)

    } else {

      arrangement <- rapply(
        as.list(arrangement),
        function(x, map) map[[x]],
        "character",
        how = "replace",
        map = id_map
      )
    }
  }

  ext_panels <- lapply(extensions, ext_panel)

  panels <- lapply(c(blk_panels, ext_panels), create_layout_panel)
  names(panels) <- chr_xtr(panels, "id")

  if (!all(unlist(arrangement) %in% names(panels))) {
    blockr_abort(
      "Cannot match layout panel IDs to panels.",
      class = "invalid_panel_layout_specification"
    )
  }

  if (length(arrangement)) {

    grid <- draw_panel_tree(arrangement)
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

is_empty_layout <- function(x) length(x[["panels"]]) == 0L

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
  dock_layout(x)
}

#' @export
as_dock_layout.list <- function(x, ...) {
  if ("activeGroup" %in% names(x)) {
    names(x)[names(x) == "activeGroup"] <- "active_group"
  }

  do.call(new_dock_layout, x)
}

#' @rdname layout
#' @export
layout_panel_ids <- function(x) {
  x <- as_dock_layout(x)
  chr_xtr(x[["panels"]], "id")
}
