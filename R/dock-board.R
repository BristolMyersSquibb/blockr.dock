#' Dock board
#'
#' Using the docking layout manager provided by dockViewR, a `dock_board`
#' extends [blockr.core::new_board()]. In addition to the attributes contained
#' in a core board, this also includes dock extensions (as `extensions`) and
#' the per-view layout, stored as two independent slots -- view structure
#' ([board_views()]) and grid geometry ([board_grids()]). Single-page boards
#' are a degenerate case with one auto-named "Page" view.
#'
#' Multi-view boards pass `views` (and optionally `grids`); see
#' [dock_view()][dock_view] and [dock_grid()][layout] for the input forms and
#' [board_views()][dock_view] for how they combine. Bare block / extension
#' ids are resolved against the board's blocks and extensions. With
#' `views = NULL` the board gets a single default view.
#'
#' @inheritParams blockr.core::new_board
#' @param extensions Dock extensions
#' @param views Per-view membership: a named list keyed by view id (minted
#'   when absent), each value a [dock_view()], a bare character vector of
#'   member panel ids, or a list of panel ids. `NULL` yields a single default
#'   view over the board's blocks and extensions.
#' @param grids Per-view geometry: a named list keyed by view id whose values
#'   are [dock_grid()]s, or a `dock_grids`. Optional -- a view with no grid
#'   entry falls back to a default grid over its members.
#' @param active Id of the initially active view (a key of `views`). Defaults
#'   to the first view. Which view is active is a property of the collection,
#'   not of an individual view.
#'
#' @examples
#' brd <- new_dock_board(c(a = blockr.core::new_dataset_block()))
#' view_members(board_views(brd)[[1L]])
#'
#' @return The constructor `new_dock_board()` returns a `board` object, as does
#' the coercion function `as_dock_board()`. Inheritance can be checked using
#' `is_dock_board()`, which returns a boolean. The `dock_extensions()` and
#' `dock_extensions<-()` accessors return / set the board's `dock_extension`
#' objects. A character vector of IDs is returned by `dock_ext_ids()` and
#' `dock_board_options()` returns a `board_options` object.
#'
#' @rdname dock
#' @export
new_dock_board <- function(blocks = list(), links = list(), stacks = list(),
                           ..., extensions = new_dock_extensions(),
                           views = NULL, grids = NULL, active = NULL,
                           options = dock_board_options(),
                           ctor = NULL, pkg = NULL, class = character()) {

  blocks <- as_blocks(blocks)
  extensions <- as_dock_extensions(extensions)

  parts <- initialise_views(views, grids, blocks, extensions, active)

  new_board(
    blocks = blocks,
    links = as_links(links),
    stacks = as_dock_stacks(stacks),
    ...,
    extensions = extensions,
    views = parts[["views"]],
    grids = parts[["grids"]],
    options = as_board_options(options),
    ctor = forward_ctor(ctor),
    pkg = pkg,
    class = c(class, "dock_board")
  )
}

# Coerce the `views` / `grids` inputs into the board's two slots. A NULL (or
# empty) `views` yields the default single-view arrangement; otherwise each is
# resolved against the board's blocks and extensions via a shared id map.
# Finally the presentation is reconciled to the authoritative block /
# extension set: members with no backing panel are pruned from the views and
# each grid is restricted to its view's members, so construction (and restore)
# of a stale or inconsistent layout self-heals rather than aborting.
initialise_views <- function(views, grids, blocks, extensions, active = NULL) {

  if (!length(views) && !length(grids)) {

    parts <- default_layout(blocks, extensions)
    dock_vws <- parts[["views"]]
    dock_grds <- parts[["grids"]]

  } else {

    id_map <- panel_id_map(blocks, extensions)
    dock_grds <- coerce_dock_grids(grids, id_map)

    dock_vws <- if (length(views)) {
      coerce_dock_views(views, id_map)
    } else {
      reconstruct_dock_views(
        lapply(lapply(dock_grds, layout_panel_ids), new_dock_view)
      )
    }
  }

  ok_panels <- c(
    as.character(as_block_panel_id(blocks)),
    as.character(as_ext_panel_id(extensions))
  )

  dock_vws <- drop_unknown_members(dock_vws, ok_panels)
  dock_grds <- restrict_grids_to_views(dock_grds, dock_vws)

  if (!is.null(active)) {
    active_view(dock_vws) <- active
  }

  list(views = dock_vws, grids = dock_grds)
}

#' @rdname panel-ids
#' @export
panel_obj_ids <- function(ids) {
  is_blk <- maybe_block_panel_id(ids)
  is_ext <- maybe_ext_panel_id(ids)
  ids[is_blk] <- as_obj_id(new_block_panel_id(ids[is_blk]))
  ids[is_ext] <- as_obj_id(new_ext_panel_id(ids[is_ext]))
  ids
}

#' @export
validate_board.dock_board <- function(x) {

  x <- NextMethod()

  views <- board_views(x)

  ok_panels <- c(
    as.character(as_block_panel_id(board_block_ids(x))),
    as.character(as_ext_panel_id(dock_ext_ids(x)))
  )

  validate_dock_views(views)
  validate_view_membership(views, ok_panels)
  validate_dock_grids(board_grids(x), views)
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

#' @export
str_value.dock_board <- function(x, ...) {
  paste(
    NextMethod(),
    str_value(board_views(x)),
    str_value(dock_extensions(x)),
    sep = "\n"
  )
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

  rm_panels <- as.character(as_block_panel_id(rm))
  views <- board_views(x)

  for (v in names(views)) {

    members <- view_members(views[[v]])

    if (any(members %in% rm_panels)) {
      x <- set_view_membership(x, v, setdiff(members, rm_panels))
    }
  }

  NextMethod(object = x)
}

#' @export
validate_board_update.dock_board <- function(payload, board, ...,
                                             session = get_session()) {

  if ("views" %in% names(payload) && !is.null(payload$views)) {

    if (!is.list(payload$views)) {
      blockr_abort(
        paste(
          "`views` must be a list with optional",
          "`add`/`mod`/`rm`/`active`/`rename`."
        ),
        class = "dock_views_delta_invalid"
      )
    }

    reject_geometry_in_mod(payload$views$mod)
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

  # Mint ids for added views and resolve every name reference to an id, so
  # the rest of the pipeline (merge, validate, apply) is purely id-keyed.
  if (length(upd$views)) {
    upd$views <- normalize_views_delta(upd$views, board)
  }

  # Canonicalize the panel-op currency: typed refs (`blk()` / `ext()`) and bare
  # id sugar become the internal named-hint form the cascade, validation and
  # apply consume. Idempotent, so the re-augment loop converges.
  if (length(upd$views$mod)) {
    upd$views$mod <- resolve_views_mod(upd$views$mod, board, upd)
  }

  rm_block_ids <- upd$blocks$rm %||% character()
  skip_views <- upd$views$rm %||% character()

  if (length(rm_block_ids)) {

    merged <- merge_views_mod(
      upd$views$mod,
      board_views(board),
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

# Add newly-created blocks to the active view's membership, in the same update
# that adds the blocks -- a pure set write, no geometry. insert_block_ui()
# places the panels in the live dock and the settled-echo mirror stores their
# arrangement; keeping membership authoritative server-side here means
# reconcile never lags the dock (#196) and serialization never drops an added
# panel. Where the client places the panel is the client's to decide.
add_block_panels_to_view <- function(board, block_ids) {

  active <- active_view(board)

  if (is.null(active)) {
    return(board)
  }

  members <- view_members(board_views(board)[[active]])
  add_panels <- as.character(as_block_panel_id(block_ids))

  set_view_membership(board, active, union(members, add_panels))
}

#' @export
apply_board_update.dock_board <- function(board, upd, ...) {

  if (length(upd$blocks$add)) {
    board <- add_block_panels_to_view(board, names(upd$blocks$add))
  }

  if (!length(upd$views)) {
    return(board)
  }

  if (length(upd$views$rm)) {
    board <- apply_views_rm(upd$views$rm, board)
  }

  if (length(upd$views$add)) {
    board <- apply_views_add(upd$views$add, board)
  }

  if (length(upd$views$mod)) {
    board <- apply_views_mod(upd$views$mod, board)
  }

  if (length(upd$views$grid)) {
    board <- apply_views_grid(upd$views$grid, board)
  }

  if (length(upd$views$rename)) {
    board <- apply_views_rename(upd$views$rename, board)
  }

  if (!is.null(upd$views$active)) {
    board <- apply_views_active(upd$views$active, board)
  }

  board
}
