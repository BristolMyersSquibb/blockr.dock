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
#' @param active Id of the initially active view (a key of `layouts`).
#'   Defaults to the first view. Which view is active is a property of the
#'   collection, not of an individual layout.
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
                           active = NULL, options = dock_board_options(),
                           ctor = NULL, pkg = NULL, class = character()) {

  layouts <- initialise_layout(layouts, blocks, extensions, active)

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

initialise_layout <- function(layouts, blocks, extensions, active = NULL) {

  # `as_dock_layouts()` homogenises every accepted input form (a
  # `dock_layouts`, a single `dock_layout`, a multi-view list, a bare
  # panel-id list / vector) into a resolved `dock_layouts`.
  res <- as_dock_layouts(layouts, blocks = blocks, extensions = extensions)

  # The active view is the container's to name (by id); default first.
  if (!is.null(active)) {
    active_view(res) <- active
  }

  res
}

resolve_views <- function(specs, c_blks, c_exts) {

  specs <- lapply(specs, coerce_view_spec)

  ext_list <- as.list(c_exts)

  # Iterate by position: a view's key is its id and may be absent (minted
  # later), so it cannot be used to index here.
  for (i in seq_along(specs)) {

    ly <- specs[[i]]

    v_obj <- panel_obj_ids(layout_panel_ids(ly))

    v_blks <- c_blks[intersect(v_obj, names(c_blks))]
    v_exts <- as_dock_extensions(
      ext_list[intersect(v_obj, names(ext_list))]
    )

    specs[[i]] <- resolve_dock_layout(v_blks, v_exts, ly)
  }

  reconstruct_dock_layouts(mint_view_ids(specs))
}

#' @rdname layout-json
#' @export
panel_obj_ids <- function(ids) {
  is_blk <- maybe_block_panel_id(ids)
  is_ext <- maybe_ext_panel_id(ids)
  ids[is_blk] <- as_obj_id(new_block_panel_id(ids[is_blk]))
  ids[is_ext] <- as_obj_id(new_ext_panel_id(ids[is_ext]))
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
    return(do.call(dock_layout, unname(v)))
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
  id <- active_view(ly)

  new <- validate_dock_layout(value, board_block_ids(x))

  nm <- view_name(ly[[id]])
  if (!is.null(nm)) {
    view_name(new) <- nm
  }

  ly[[id]] <- new
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

  if (is_blocks(rm)) {
    rm <- names(rm)
  }

  layouts <- board_layouts(x)
  rm_panels <- as.character(as_block_panel_id(rm))

  for (v in names(layouts)) {

    pids <- layout_panel_ids(layouts[[v]])

    if (any(pids %in% rm_panels)) {
      layouts[[v]] <- drop_panels_from_layout(layouts[[v]], rm_panels)
    }
  }

  board_layouts(x) <- layouts

  NextMethod(object = x)
}

#' @export
validate_board_update.dock_board <- function(payload, board, ...,
                                             session = get_session()) {

  if ("views" %in% names(payload) && !is.null(payload$views) &&
        !is.list(payload$views)) {
    blockr_abort(
      paste(
        "`views` must be a list with optional",
        "`add`/`mod`/`rm`/`active`/`rename`."
      ),
      class = "dock_views_delta_invalid"
    )
  }

  if ("extensions" %in% names(payload) && !is.null(payload$extensions) &&
        !is.list(payload$extensions)) {
    blockr_abort(
      "`extensions` must be a list with an optional `mod` component.",
      class = "dock_extensions_delta_invalid"
    )
  }

  NextMethod()
}

#' @export
augment_board_update.dock_board <- function(upd, board, ...,
                                            session = get_session()) {

  upd <- NextMethod()

  if (length(upd$views$add) || length(upd$views$mod)) {
    upd$views <- resolve_views_layouts(upd$views, board, upd)
  }

  # Mint ids for added views and resolve every name reference to an id, so
  # the rest of the pipeline (merge, validate, apply) is purely id-keyed.
  if (length(upd$views)) {
    upd$views <- normalize_views_delta(upd$views, board)
  }

  rm_block_ids <- upd$blocks$rm %||% character()
  skip_views <- upd$views$rm %||% character()

  if (length(rm_block_ids)) {

    merged <- merge_views_mod(
      upd$views$mod,
      board_layouts(board),
      rm_block_ids,
      skip_views = skip_views
    )

    if (length(merged)) {
      upd$views$mod <- merged
    }
  }

  if (length(upd$views)) {
    validate_views_delta(upd$views, board, upd)
  }

  if (length(upd$extensions)) {
    validate_extensions_delta(upd$extensions, board)
  }

  upd
}

#' @export
apply_board_update.dock_board <- function(board, upd, ...,
                                          session = get_session()) {

  dot_args <- list(...)
  dock_mgr <- dot_args$dock_mgr

  if (length(upd$extensions$mod) && !is.null(dock_mgr)) {
    apply_extensions_mod(upd$extensions$mod, dock_mgr$ext_res)
  }

  if (!length(upd$views)) {
    return(board)
  }

  if (length(upd$views$rm)) {
    board <- apply_views_rm(upd$views$rm, board, dock_mgr, session)
  }

  if (length(upd$views$add)) {
    board <- apply_views_add(upd$views$add, board, dock_mgr, session)
  }

  if (length(upd$views$mod)) {
    board <- apply_views_mod(upd$views$mod, board, dock_mgr)
  }

  if (length(upd$views$rename)) {
    board <- apply_views_rename(upd$views$rename, board, dock_mgr, session)
  }

  if (!is.null(upd$views$active)) {
    board <- apply_views_active(upd$views$active, board, dock_mgr, session)
  }

  board
}
