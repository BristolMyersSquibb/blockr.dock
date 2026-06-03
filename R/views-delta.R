# Resolve bare IDs in submitted `add` / `mod` layouts to canonical
# block_panel-/ext_panel- form (as `initialise_layout()` does at
# construction). Uses the pre-rm block set (current + blocks$add) so a
# layout referencing a to-be-removed block still resolves;
# `merge_views_mod()` drops it before the post-state checks.
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
  if (length(views$mod)) {
    views$mod <- lapply(views$mod, resolve_one)
  }

  views
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
    views$add <- mint_added_view_ids(views$add, names(board_layouts(board)))

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
    names(views), c("add", "mod", "rm", "active", "rename")
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

  current_views <- names(board_layouts(board))
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

    if (!is_dock_layout(views$mod[[v]])) {
      blockr_abort(
        "`views$mod${v}` must be a `dock_layout`.",
        class = "dock_views_delta_layout_invalid"
      )
    }

    validate_layout_panel_refs(views$mod[[v]], ok_panels, v)
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

# Merge user-supplied `views$mod` with the block-removal cleanup: when
# both touch a view, the cleanup drops the removed block from whatever
# layout the user submitted; otherwise the present one wins.
merge_views_mod <- function(user_mod, layouts, rm_block_ids,
                            skip_views = character()) {

  out <- user_mod %||% list()

  if (!length(rm_block_ids)) {
    return(out)
  }

  rm_panels <- as.character(as_block_panel_id(rm_block_ids))

  for (v in setdiff(names(layouts), skip_views)) {

    base <- out[[v]] %||% layouts[[v]]
    pids <- layout_panel_ids(base)

    if (any(pids %in% rm_panels)) {
      out[[v]] <- drop_panels_from_layout(base, rm_panels)
    }
  }

  out
}

apply_views_rm <- function(rm_ids, board) {

  layouts <- board_layouts(board)
  old_active <- active_view(layouts)
  surviving <- setdiff(names(layouts), rm_ids)

  layouts <- structure(
    as.list(unclass(layouts))[surviving],
    class = "dock_layouts"
  )

  if (length(surviving)) {
    active_view(layouts) <- if (!is.null(old_active) &&
                                  old_active %in% surviving) {
      old_active
    } else {
      surviving[[1L]]
    }
  }

  board_layouts(board) <- layouts

  board
}

apply_views_add <- function(add_views, board) {

  layouts <- board_layouts(board)

  # Adding a view never changes which one is active: that travels on the
  # delta's `active` slot (resolved to the new view's id in
  # `normalize_views_delta()` when it names an add) and is applied by
  # `apply_views_active()` once the view exists. Here we only splice the
  # new views into the container.
  for (v in names(add_views)) {
    layouts[[v]] <- add_views[[v]]
  }

  board_layouts(board) <- layouts

  board
}

apply_views_mod <- function(mod_views, board) {

  layouts <- board_layouts(board)

  for (v in names(mod_views)) {

    new <- mod_views[[v]]

    # A mod replaces the arrangement, not the identity: keep the display
    # name (the incoming layout carries it only on the live-sync path).
    # Which view is active is a container field, unaffected by replacing a
    # view's layout under the same id.
    nm <- coal(view_name(mod_views[[v]]), view_name(layouts[[v]]),
               fail_all = FALSE)
    if (!is.null(nm)) {
      view_name(new) <- nm
    }

    layouts[[v]] <- new
  }

  board_layouts(board) <- layouts

  board
}

# Rename one or more views: a name-attribute write keyed by stable id.
# `rename` is a named list mapping view id to its new display name. The
# id is untouched, so the dock module, DOM element and registry key all
# survive — no layout rebuild, no re-keying.
apply_views_rename <- function(rename, board) {

  layouts <- board_layouts(board)

  for (id in names(rename)) {
    if (id %in% names(layouts)) {
      view_name(layouts[[id]]) <- rename[[id]]
    }
  }

  board_layouts(board) <- layouts

  board
}

# Whether two layouts describe the same panel arrangement, used to
# short-circuit `restore_layout` when the live dockview state already
# matches the target. Compares normalised wire specs (drops volatile
# pixel sizes / regenerated group IDs) and additionally drops `focus`
# so a tab click doesn't force a rebuild. `a` is the live wire shape
# (`get_dock()`); `b` is the target `dock_layout`.
layouts_match <- function(a, b) {

  to_spec <- function(x) {

    if (is.null(x)) {
      return(NULL)
    }

    ly <- if (is_dock_layout(x)) {
      x
    } else {
      tryCatch(dockview_to_layout(x), error = function(e) NULL)
    }

    if (is.null(ly)) {
      return(NULL)
    }

    spec <- tryCatch(layout_to_spec(ly), error = function(e) NULL)

    if (is.null(spec)) {
      return(NULL)
    }

    spec[["focus"]] <- NULL

    spec
  }

  spec_a <- to_spec(a)
  spec_b <- to_spec(b)

  if (is.null(spec_a) || is.null(spec_b)) {
    return(FALSE)
  }

  identical(spec_a, spec_b)
}

apply_views_active <- function(active, board) {

  layouts <- board_layouts(board)
  active_view(layouts) <- active
  board_layouts(board) <- layouts

  board
}

# Runtime switch to view `active` (by id): activate its dock in the DOM,
# move the block / ext UIs across, and point active_dock and the nav at
# it. The board's active marker is set by the caller; this drives the
# live session state.
switch_active_view <- function(active, docks, active_dock, current_active,
                               session) {

  old <- isolate(current_active())

  if (identical(old, active)) {
    return(invisible())
  }

  if (exists(active, envir = docks, inherits = FALSE)) {

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

  current_active(active)

  session$sendInputMessage(
    "view_nav",
    list(value = active)
  )

  invisible()
}

# Apply one view's layout change. v1 unconditionally restores the target
# layout; surgical diff paths (noop / active-tab / reorder) are a
# deferred follow-up that can dispatch here without touching callers.
apply_layout_diff <- function(view, target, proxy, blocks, extensions) {

  log_debug("applying layout diff for view {view}")

  # Block / extension UIs live in board-level wrapper elements (the
  # offcanvas at init time, a dockview panel container while attached)
  # and are moved between them via `move_dom_element`. `restore_dock`
  # tears the panel containers down, taking the attached UIs with them,
  # so we have to (a) park the UIs back in the offcanvas first and
  # (b) reattach them to the freshly-rendered panels afterwards. Iterate
  # object ids (as `hide_view_ui()` does): `for` over the classed id
  # vector would drop the class and double-prefix the move target.
  for (oid in as_obj_id(block_panel_ids(proxy))) {
    hide_block_panel(oid, rm_panel = FALSE, proxy = proxy)
  }
  for (oid in as_obj_id(ext_panel_ids(proxy))) {
    hide_ext_panel(oid, rm_panel = FALSE, proxy = proxy)
  }

  restore_layout(target, proxy, blocks = blocks, extensions = extensions)

  for (pid in as_dock_panel_id(target)) {
    if (is_block_panel_id(pid)) {
      show_block_panel(pid, add_panel = FALSE, proxy = proxy)
    } else if (is_ext_panel_id(pid)) {
      show_ext_panel(pid, add_panel = FALSE, proxy = proxy)
    } else {
      blockr_abort(
        "Unknown panel type {.cls {class(pid)}}.",
        class = "dock_panel_invalid"
      )
    }
  }

  invisible(NULL)
}
