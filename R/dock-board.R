#' Dock board
#'
#' Using the docking layout manager provided by dockViewR, a `dock_board`
#' extends [blockr.core::new_board()]. In addition to the attributes contained
#' in a core board, this also includes dock extensions (as `extensions`)
#' and the panel arrangement (as `layouts`). The `layouts` field is always
#' stored internally as a `dock_layouts` collection (multi-view); single-page
#' boards are a degenerate case with one auto-named "Page" view.
#'
#' For multi-view boards, pass a named list to `layouts =` — each name
#' becomes a view, each value is the panel arrangement (a [dock_layout()]
#' or a raw list of block / extension IDs). For a single-page board, pass
#' a `dock_layout` or raw list directly. Either way the input is normalised
#' to a `dock_layouts`, with leaf IDs resolved against the board's blocks
#' and extensions.
#'
#' @inheritParams blockr.core::new_board
#' @param extensions Dock extensions
#' @param layouts A named list of per-view arrangements (multi-view), a
#'   `dock_layout` / raw list (single-page), or an existing `dock_layouts`
#'   collection. All forms are normalised to `dock_layouts`.
#'
#' @examples
#' brd <- new_dock_board(c(a = blockr.core::new_dataset_block()))
#' str(active_layout(brd), max.level = 2)
#'
#' @return The constructor `new_dock_board()` returns a `board` object, as does
#' the coercion function `as_dock_board()`. Inheritance can be checked using
#' `is_dock_board()`, which returns a boolean. `board_layouts()` returns the
#' board's `dock_layouts`; `active_layout()` returns the active view's
#' `dock_layout` and `active_layout<-()` writes into the active view. The
#' `dock_extensions()` and `dock_extensions<-()` accessors return / set the
#' board's `dock_extension` objects. A character vector of IDs is returned by
#' `dock_ext_ids()` and `dock_board_options()` returns a `board_options`
#' object.
#'
#' @rdname dock
#' @export
new_dock_board <- function(blocks = list(), links = list(), stacks = list(),
                           ..., extensions = new_dock_extensions(),
                           layouts = default_layout(blocks, extensions),
                           options = dock_board_options(),
                           ctor = NULL, pkg = NULL, class = character()) {

  layouts <- initialise_layout(layouts, blocks, extensions)

  new_board(
    blocks = as_blocks(blocks),
    links = as_links(links),
    stacks = as_dock_stacks(stacks),
    ...,
    extensions = as_dock_extensions(extensions),
    layouts = layouts,
    options = as_board_options(options),
    ctor = forward_ctor(ctor),
    pkg = pkg,
    class = c(class, "dock_board")
  )
}

initialise_layout <- function(layouts, blocks, extensions) {

  c_blks <- as_blocks(blocks)
  c_exts <- as_dock_extensions(extensions)

  if (is_dock_layouts(layouts)) {
    return(validate_dock_layouts(layouts))
  }

  if (is_dock_layout(layouts)) {
    return(as_dock_layouts(resolve_dock_layout(c_blks, c_exts, layouts)))
  }

  if (is.list(layouts) && "grid" %in% names(layouts)) {
    return(
      as_dock_layouts(
        resolve_dock_layout(c_blks, c_exts, dockview_to_layout(layouts))
      )
    )
  }

  is_multi_view <- (
    is.list(layouts) && length(layouts) > 0L && !is.null(names(layouts))
  )

  if (!is_multi_view) {
    spec <- if (is.list(layouts)) {
      do.call(dock_layout, layouts)
    } else {
      do.call(dock_layout, as.list(layouts))
    }
    return(as_dock_layouts(resolve_dock_layout(c_blks, c_exts, spec)))
  }

  resolve_views(layouts, c_blks, c_exts)
}

resolve_views <- function(specs, c_blks, c_exts) {

  nms <- names(specs)

  if (is.null(nms) || any(!nzchar(nms))) {
    blockr_abort(
      "All views must be named.",
      class = "dock_layouts_names_missing"
    )
  }

  if (anyDuplicated(nms) > 0L) {
    blockr_abort(
      "View names must be unique.",
      class = "dock_layouts_names_duplicated"
    )
  }

  specs <- lapply(specs, coerce_view_spec)

  if (!any(vapply(specs, is_active_view, logical(1L)))) {
    specs[[1L]] <- set_active_view(specs[[1L]])
  }

  ext_list <- as.list(c_exts)

  for (view_name in names(specs)) {

    ly <- specs[[view_name]]
    was_active <- is_active_view(ly)

    v_obj <- panel_obj_ids(layout_panel_ids(ly))

    v_blks <- c_blks[intersect(v_obj, names(c_blks))]
    v_exts <- as_dock_extensions(
      ext_list[intersect(v_obj, names(ext_list))]
    )

    specs[[view_name]] <- resolve_dock_layout(v_blks, v_exts, ly)

    if (was_active) {
      specs[[view_name]] <- set_active_view(specs[[view_name]])
    }
  }

  validate_dock_layouts(structure(specs, class = "dock_layouts"))
}

#' @rdname layout-json
#' @export
panel_obj_ids <- function(ids) {
  is_blk <- maybe_block_panel_id(ids)
  is_ext <- maybe_ext_panel_id(ids)
  ids[is_blk] <- sub("^block_panel-", "", ids[is_blk])
  ids[is_ext] <- sub("^ext_panel-", "", ids[is_ext])
  ids
}

coerce_view_spec <- function(v) {
  if (is_dock_layout(v)) {
    return(v)
  }

  if (is.list(v) && "grid" %in% names(v)) {
    return(dockview_to_layout(v))
  }

  if (is.list(v)) {
    active <- isTRUE(attr(v, "active"))
    return(set_active_view(do.call(dock_layout, unname(v)), active))
  }

  blockr_abort(
    "Each layout slot must be a `dock_layout` or a list.",
    class = "dock_layouts_element_invalid"
  )
}

#' @export
validate_board.dock_board <- function(x) {

  x <- NextMethod()

  validate_dock_layouts(board_layouts(x))
  validate_extensions(dock_extensions(x))

  x
}

#' @param x Board object
#' @rdname dock
#' @export
is_dock_board <- function(x) {
  inherits(x, "dock_board")
}

#' @rdname dock
#' @export
as_dock_board <- function(x, ...) {
  UseMethod("as_dock_board")
}

#' @export
as_dock_board.dock_board <- function(x, ...) {
  x
}

#' @export
as_dock_board.board <- function(x, ...) {
  new_dock_board(
    board_blocks(x),
    board_links(x),
    board_stacks(x),
    ...
  )
}

#' @rdname dock
#' @export
active_layout <- function(x) {
  ly <- board_layouts(x)
  validate_dock_layout(ly[[active_view(ly)]], board_block_ids(x))
}

#' @param value Replacement value
#' @rdname dock
#' @export
`active_layout<-` <- function(x, value) {
  ly <- board_layouts(x)
  ly[[active_view(ly)]] <- set_active_view(
    validate_dock_layout(value, board_block_ids(x))
  )
  board_layouts(x) <- ly

  invisible(x)
}

#' @rdname dock
#' @export
dock_extensions <- function(x) {
  stopifnot(is_dock_board(x))
  validate_extensions(x[["extensions"]])
}

#' @param value Replacement value
#' @rdname dock
#' @export
`dock_extensions<-` <- function(x, value) {
  stopifnot(is_dock_board(x))
  x[["extensions"]] <- validate_extensions(value)
  invisible(x)
}

#' @rdname dock
#' @export
dock_ext_ids <- function(x) {
  chr_ply(dock_extensions(x), extension_id)
}

#' @rdname dock
#' @export
dock_board_options <- function() {
  new_board_options(
    new_board_name_option()
  )
}

#' @export
rm_blocks.dock_board <- function(x, rm, ...) {

  # for now, b/c dock$layout(), passed in ... is not updated in time, we can
  # only clear the layout to not run into validation issues
  active_layout(x) <- resolve_dock_layout(extensions = dock_extensions(x))

  NextMethod(object = x)
}
