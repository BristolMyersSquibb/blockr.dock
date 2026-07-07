# Resolve bare IDs in a submitted `add` layout to canonical
# block_panel-/ext_panel- form (as `initialise_layout()` does at
# construction). Uses the pre-rm block set (current + blocks$add) so a
# layout referencing a to-be-removed block still resolves;
# `merge_views_mod()` drops it before the post-state checks. `mod` no longer
# carries geometry -- it is a membership set of already-canonical panel ids.
resolve_views_layouts <- function(views, board, upd) {

  current_blocks <- board_blocks(board)

  all_blocks <- c(
    as.list(current_blocks),
    upd$blocks$add %||% list()
  )

  if (length(all_blocks)) {
    all_blocks <- as_blocks(all_blocks)
  } else {
    all_blocks <- as_blocks(list())
  }

  all_exts <- dock_extensions(board)

  resolve_one <- function(ly) {
    if (!is_dock_layout(ly)) {
      return(ly)
    }
    tryCatch(
      resolve_dock_layout(all_blocks, all_exts, ly),
      error = function(e) ly
    )
  }

  if (length(views$add)) {
    views$add <- lapply(views$add, resolve_one)
  }

  views
}

# The update lifecycle owns membership, the settled-echo mirror owns geometry.
# A `views$mod` value carrying a layout would smuggle geometry into a
# membership write, so it is refused at the boundary with a pointer to the
# mirror.
reject_geometry_in_mod <- function(mod) {

  for (v in names(mod)) {

    if (is_dock_layout(mod[[v]])) {
      blockr_abort(
        paste0(
          "`views$mod$", v, "` must be a panel-id set, not a layout: view ",
          "geometry is owned by the settled-echo grid mirror, not the ",
          "update lifecycle."
        ),
        class = "dock_views_mod_geometry_rejected"
      )
    }
  }

  invisible(mod)
}

# Normalise an inbound `views` delta to be keyed purely by stable view id.
# Existing views are addressed by id only — `mod` / `rm` / `active` carry
# ids (the one unique, stable handle; the display name is just sugar) and
# pass through untouched. The exception is `add`: a new view has no id
# yet, so its key is the desired display name and a fresh id is minted.
# `active` may also forward-reference a view added in the same delta by its
# `add` key, which is resolved here to the minted id (see below).
normalize_views_delta <- function(views, board) {

  if (length(views$add)) {

    add_keys <- names(views$add)
    views$add <- mint_added_view_ids(views$add, names(board_views(board)))

    # "Add a view and make it active" in one delta: a new view has no id
    # to name in `active` until it is minted just above, so `active` may
    # carry its `add` key (the desired display name) instead. Resolve that
    # to the minted id here, before `validate_views_delta()` checks it
    # against the post-state ids. An `active` already naming an existing
    # view passes through; add keys resolve first, so the (vanishingly
    # unlikely) clash between an add key and an existing id settles
    # deterministically in favour of the freshly added view.
    if (is_string(views$active) && views$active %in% add_keys) {
      views$active <- names(views$add)[match(views$active, add_keys)]
    }
  }

  views
}

# Mint a stable id for each added view that still needs one — unique against
# the board's existing ids — carrying the key (the desired display name) onto
# the view as its name. A view that already has a name (and thus an id key,
# from an earlier pass) is left untouched, which keeps augment idempotent:
# the update lifecycle re-runs augment until it stops changing the payload, so
# re-minting on every pass would loop a view add forever.
mint_added_view_ids <- function(add, reserved) {
  needs <- lgl_ply(add, function(x) is.null(view_name(x)))
  if (!any(needs)) {
    return(add)
  }
  add[needs] <- map(`view_name<-`, add[needs], names(add)[needs])
  ids <- names(add)
  ids[needs] <- rand_names(c(reserved, ids[!needs]), n = sum(needs))
  set_names(add, ids)
}

# Structural + cross-reference checks for the `views` slice. Runs after
# in-core augmentation, so panel-ID xrefs resolve against the post-state
# block set derived from `upd`.
validate_views_delta <- function(views, board, upd) {

  if (!is.list(views)) {
    blockr_abort(
      paste(
        "`views` must be a list with optional",
        "`add`/`mod`/`rm`/`active`/`rename`."
      ),
      class = "dock_views_delta_invalid"
    )
  }

  unknown_keys <- setdiff(
    names(views), c("add", "mod", "rm", "active", "rename", "grid")
  )
  if (length(unknown_keys)) {
    blockr_abort(
      "Unknown `views` slice key{?s}: {unknown_keys}.",
      class = "dock_views_delta_invalid"
    )
  }

  add_ids <- names(views$add) %||% character()
  mod_ids <- names(views$mod) %||% character()
  rm_ids <- views$rm %||% character()
  rename_ids <- names(views$rename) %||% character()
  active <- views$active

  add_unnamed <- length(views$add) &&
    (is.null(names(views$add)) || any(!nzchar(add_ids)))

  if (add_unnamed) {
    blockr_abort(
      "All entries of `views$add` must carry a view id.",
      class = "dock_views_delta_unnamed"
    )
  }

  mod_unnamed <- length(views$mod) &&
    (is.null(names(views$mod)) || any(!nzchar(mod_ids)))

  if (mod_unnamed) {
    blockr_abort(
      "All entries of `views$mod` must carry a view id.",
      class = "dock_views_delta_unnamed"
    )
  }

  if (length(rm_ids) && !is.character(rm_ids)) {
    blockr_abort(
      "`views$rm` must be a character vector of view ids.",
      class = "dock_views_delta_invalid"
    )
  }

  rename_unnamed <- length(views$rename) &&
    (is.null(names(views$rename)) || any(!nzchar(rename_ids)) ||
       !all(lgl_ply(views$rename, is_string)))

  if (rename_unnamed) {
    blockr_abort(
      "`views$rename` must map view ids to single display names.",
      class = "dock_views_delta_invalid"
    )
  }

  clash <- intersect(add_ids, rm_ids)
  if (length(clash)) {
    blockr_abort(
      "View{?s} {clash} cannot appear in both `views$add` and `views$rm`.",
      class = "dock_views_delta_add_rm_clash"
    )
  }

  mod_in_rm <- intersect(mod_ids, rm_ids)
  if (length(mod_in_rm)) {
    blockr_abort(
      "View{?s} {mod_in_rm} cannot appear in both `views$mod` and `views$rm`.",
      class = "dock_views_delta_mod_rm_clash"
    )
  }

  mod_in_add <- intersect(mod_ids, add_ids)
  if (length(mod_in_add)) {
    blockr_abort(
      paste(
        "View{?s} {mod_in_add} cannot appear in both `views$mod` and",
        "`views$add`; fold the layout into `add`."
      ),
      class = "dock_views_delta_mod_add_clash"
    )
  }

  current_views <- names(board_views(board))
  unknown_mod <- setdiff(mod_ids, current_views)
  if (length(unknown_mod)) {
    blockr_abort(
      "View{?s} {unknown_mod} in `views$mod` do not exist on the board.",
      class = "dock_views_delta_mod_unknown"
    )
  }

  unknown_rm <- setdiff(rm_ids, current_views)
  if (length(unknown_rm)) {
    blockr_abort(
      "View{?s} {unknown_rm} in `views$rm` do not exist on the board.",
      class = "dock_views_delta_rm_unknown"
    )
  }

  unknown_rename <- setdiff(rename_ids, current_views)
  if (length(unknown_rename)) {
    blockr_abort(
      "View{?s} {unknown_rename} in `views$rename` do not exist on the board.",
      class = "dock_views_delta_rename_unknown"
    )
  }

  grid_ids <- names(views$grid) %||% character()
  unknown_grid <- setdiff(grid_ids, current_views)
  if (length(unknown_grid)) {
    blockr_abort(
      paste(
        "View{?s} {unknown_grid} in `views$grid` do not exist on",
        "the board."
      ),
      class = "dock_views_delta_arrange_unknown"
    )
  }

  add_clash <- intersect(add_ids, current_views)
  if (length(add_clash)) {
    blockr_abort(
      "View{?s} {add_clash} in `views$add` already exist on the board.",
      class = "dock_views_delta_add_existing"
    )
  }

  post_views <- setdiff(c(current_views, add_ids), rm_ids)

  if (!length(post_views)) {
    blockr_abort(
      "Cannot remove all views: at least one view must remain.",
      class = "dock_views_delta_remove_all"
    )
  }

  if (!is.null(active)) {

    if (!is_string(active)) {
      blockr_abort(
        "`views$active` must be a single view id.",
        class = "dock_views_delta_active_invalid"
      )
    }

    if (!active %in% post_views) {
      blockr_abort(
        "Active view {active} does not resolve to an existing view.",
        class = "dock_views_delta_active_invalid"
      )
    }
  }

  post_block_ids <- setdiff(
    c(board_block_ids(board), names(upd$blocks$add) %||% character()),
    upd$blocks$rm %||% character()
  )

  ok_panels <- c(
    as_block_panel_id(post_block_ids),
    as_ext_panel_id(dock_ext_ids(board))
  )

  for (v in add_ids) {

    if (!is_dock_layout(views$add[[v]])) {
      blockr_abort(
        "`views$add${v}` must be a `dock_layout`.",
        class = "dock_views_delta_layout_invalid"
      )
    }

    validate_layout_panel_refs(views$add[[v]], ok_panels, v)
  }

  for (v in mod_ids) {

    members <- views$mod[[v]]

    if (!is.character(members)) {
      blockr_abort(
        "`views$mod${v}` must be a character vector of panel ids.",
        class = "dock_views_delta_membership_invalid"
      )
    }

    bad <- setdiff(members, as.character(ok_panels))

    if (length(bad)) {
      blockr_abort(
        paste0(
          "Panel ID(s) in view `", v, "` do not resolve to current blocks ",
          "or extensions: ", paste(bad, collapse = ", "), "."
        ),
        class = "dock_views_delta_panel_ref_invalid"
      )
    }
  }

  invisible(views)
}

validate_layout_panel_refs <- function(layout, ok_panels, view_name) {

  validate_dock_layout(layout)

  pids <- layout_panel_ids(layout)
  bad <- pids[!pids %in% ok_panels]

  if (length(bad)) {

    msg <- paste0(
      "Panel ID(s) in view `", view_name, "` do not resolve to current ",
      "blocks or extensions: ", paste(bad, collapse = ", "), "."
    )

    blockr_abort(
      msg,
      class = "dock_views_delta_panel_ref_invalid"
    )
  }

  invisible(layout)
}

# Remove the given panel IDs from a layout's grid: empty leaves drop,
# branches with no surviving children collapse, each leaf's open tab is
# kept (falling back to the first survivor). Which view is active is a
# container concern, untouched by reshaping a single view's grid.
drop_panels_from_layout <- function(layout, panel_ids) {

  if (!length(panel_ids)) {
    return(layout)
  }

  drop_from_leaf <- function(leaf) {

    views <- unlst(leaf[["data"]][["views"]])
    kept <- setdiff(views, panel_ids)

    if (!length(kept)) {
      return(NULL)
    }

    leaf[["data"]][["views"]] <- as.list(kept)

    av <- leaf[["data"]][["activeView"]]
    if (is.null(av) || !av %in% kept) {
      leaf[["data"]][["activeView"]] <- kept[[1L]]
    }

    leaf
  }

  result <- layout
  result[["grid"]] <- grid_map_leaves(result[["grid"]], drop_from_leaf)

  if (!length(grid_panel_ids(result[["grid"]]))) {
    result[["activeGroup"]] <- NULL
  }

  result
}

# Fold the panel set the live dock now holds into a membership delta: the new
# membership as a plain id set, or `NULL` when it already matches (so the
# caller skips a no-op update). Geometry is not this path's concern -- the
# settled-echo mirror owns it -- so this is a pure set comparison. Catches the
# dock-only mutation paths (the add-panel modal, a closed tab, an extension
# show / hide) that never travel through block add / remove.
fold_live_membership <- function(members, live_ids) {

  if (setequal(members, live_ids)) {
    return(NULL)
  }

  as.character(live_ids)
}

# Merge a user-supplied `views$mod` (membership sets) with the block-removal
# cleanup: drop each removed block's panel from whatever set the update carries
# for a view, falling back to the view's current membership. Pure set algebra
# -- geometry never enters.
merge_views_mod <- function(user_mod, views, rm_block_ids,
                            skip_views = character()) {

  out <- user_mod %||% list()

  if (!length(rm_block_ids)) {
    return(out)
  }

  rm_panels <- as.character(as_block_panel_id(rm_block_ids))

  for (v in setdiff(names(views), skip_views)) {

    base <- out[[v]] %||% view_members(views[[v]])

    if (any(base %in% rm_panels)) {
      out[[v]] <- setdiff(base, rm_panels)
    }
  }

  out
}

# Write a view's membership as a set. Geometry is untouched: a shrink can leave
# an inert ghost in the arrangement and a grow an un-landed member, both legal
# under total semantics and reconciled only at the compose / restore boundary.
set_view_membership <- function(board, view_id, members) {

  views <- board_views(board)

  views[[view_id]] <- new_dock_view(
    as.character(members), view_name(views[[view_id]])
  )

  board_views(board) <- views

  board
}

apply_views_rm <- function(rm_ids, board) {

  views <- board_views(board)
  old_active <- active_view(views)
  surviving <- setdiff(names(views), rm_ids)

  for (v in rm_ids) {
    views[[v]] <- NULL
  }

  if (length(surviving) &&
        (is.null(old_active) || !old_active %in% surviving)) {
    active_view(views) <- surviving[[1L]]
  }

  arr <- board_grids(board)

  if (not_null(arr)) {
    for (v in rm_ids) {
      arr[[v]] <- NULL
    }
  }

  board_views(board) <- views
  board_grids(board) <- arr

  board
}

apply_views_add <- function(add_views, board) {

  views <- board_views(board)
  arr <- as.list(board_grids(board) %||% list())

  # Adding a view never changes which one is active: that travels on the
  # delta's `active` slot (resolved to the new view's id in
  # `normalize_views_delta()` when it names an add) and is applied by
  # `apply_views_active()` once the view exists. A new view's initial layout
  # is authored, so it splits into a membership record and its stored grid --
  # the only geometry the lifecycle ever writes.
  for (v in names(add_views)) {

    ly <- add_views[[v]]

    views[[v]] <- new_dock_view(layout_panel_ids(ly), view_name(ly))
    arr[[v]] <- project_grid(ly)
  }

  board_views(board) <- views
  board_grids(board) <- new_dock_grids(arr)

  board
}

apply_views_mod <- function(mod_views, board) {

  for (v in names(mod_views)) {
    board <- set_view_membership(board, v, mod_views[[v]])
  }

  board
}

# Apply the grid mirror's echo: store each view's settled layout verbatim in
# canonical form, eliding a plain default to NULL. No membership projection --
# containment is boundary hygiene, not a write-time concern. The mirror is the
# sole producer of this slice.
apply_views_grid <- function(grid, board) {

  arr <- as.list(board_grids(board) %||% list())
  views <- board_views(board)

  for (v in names(grid)) {

    if (!v %in% names(views)) {
      next
    }

    ly <- grid[[v]]

    if (is.null(ly)) {
      arr[[v]] <- NULL
      next
    }

    arr[[v]] <- project_grid(ly)
  }

  board_grids(board) <- new_dock_grids(arr)

  board
}

# Rename one or more views: a name-attribute write keyed by stable id.
# `rename` is a named list mapping view id to its new display name. The
# id is untouched, so the dock module, DOM element and registry key all
# survive -- no rebuild, no re-keying.
apply_views_rename <- function(rename, board) {

  views <- board_views(board)

  for (id in names(rename)) {
    if (id %in% names(views)) {
      view_name(views[[id]]) <- rename[[id]]
    }
  }

  board_views(board) <- views

  board
}

apply_views_active <- function(active, board) {

  views <- board_views(board)
  active_view(views) <- active
  board_views(board) <- views

  board
}

# Runtime switch to view `active` (by id): activate its dock in the DOM,
# move the block / ext UIs across, and point active_dock and the nav at
# it. The board's active marker is set by the caller; this drives the
# live session state.
switch_active_view <- function(active, docks, active_dock, client_active,
                               session) {

  old <- isolate(client_active())

  if (identical(old, active)) {
    return(invisible())
  }

  if (active %in% names(docks)) {

    # switch-view must fire before show_view_ui so the target dock carries
    # blockr-view-dock-active; otherwise move-element drops DOM moves into an
    # inactive dock.
    session$sendCustomMessage(
      "switch-view",
      list(id = session$ns(as_view_handle_id(active)))
    )

    hide_view_ui(old, docks)
    show_view_ui(active, docks)
    update_active_dock(active_dock, docks[[active]])
  }

  client_active(active)

  session$sendInputMessage(
    "view_nav",
    list(value = active)
  )

  invisible()
}
