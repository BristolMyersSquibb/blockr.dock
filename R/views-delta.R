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
# The dock's own live-sync emits id-keyed deltas already; external
# producers (e.g. the assistant) address views by display name. Here every
# `add` gets a freshly minted id (its former key becomes the new view's
# display name), and every `mod` / `rm` / `active` reference — an id or a
# name — resolves to an id. Downstream of this, a name is never a key.
normalize_views_delta <- function(views, board) {

  layouts <- board_layouts(board)

  if (length(views$add)) {
    views$add <- mint_view_ids(views$add, names(layouts))
  }

  if (length(views$mod)) {
    names(views$mod) <- chr_ply(
      names(views$mod), resolve_view_ref, layouts, views$add
    )
  }

  if (length(views$rm)) {
    views$rm <- chr_ply(views$rm, resolve_view_ref, layouts, views$add)
  }

  if (!is.null(views$active)) {
    views$active <- resolve_view_ref(views$active, layouts, views$add)
  }

  views
}

# Map a single view reference (an id already, or a display name) to a view
# id, considering existing board views and views added in the same delta.
resolve_view_ref <- function(ref, layouts, added = list()) {

  if (ref %in% names(layouts) || ref %in% names(added)) {
    return(ref)
  }

  by_name <- view_id_by_name(layouts, ref)
  if (!is.na(by_name)) {
    return(by_name)
  }

  added_hit <- match(ref, chr_ply(added, view_name))
  if (!is.na(added_hit)) {
    return(names(added)[added_hit])
  }

  ref
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
# branches with no surviving children collapse, the active-view marker
# is preserved.
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

  was_active <- is_active_view(layout)

  result <- layout
  result[["grid"]] <- grid_map_leaves(result[["grid"]], drop_from_leaf)

  if (!length(grid_panel_ids(result[["grid"]]))) {
    result[["activeGroup"]] <- NULL
  }

  set_active_view(result, was_active)
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

apply_views_rm <- function(rm_ids, board, dock_mgr = NULL, session = NULL) {

  layouts <- board_layouts(board)
  surviving <- setdiff(names(layouts), rm_ids)
  layouts <- structure(
    as.list(unclass(layouts))[surviving],
    class = "dock_layouts"
  )

  if (length(layouts) && !any_active_view(layouts)) {
    layouts[[1L]] <- set_active_view(layouts[[1L]])
  }

  board_layouts(board) <- layouts

  if (is.null(dock_mgr) || is.null(session)) {
    return(board)
  }

  ns <- session$ns
  state <- isolate(dock_mgr$vs$state)
  was_active_removed <- active_view(state) %in% rm_ids

  for (v in rm_ids) {

    if (exists(v, envir = dock_mgr$docks, inherits = FALSE)) {

      rm_dock <- dock_mgr$docks[[v]]

      # Park the view's block / ext UIs back in the offcanvas before the
      # dock is destroyed — otherwise a block that survives in another
      # view loses its (board-level, single-instance) UI along with the
      # torn-down panel container. No-op for views already parked.
      hide_view_ui(v, dock_mgr$docks)

      destroy_module(rm_dock$dock_id, session = session)
      removeUI(
        selector = paste0("#", ns(as_view_handle_id(rm_dock$dock_id))),
        immediate = TRUE,
        session = session
      )
      rm(list = v, envir = dock_mgr$docks)
    }

    state[[v]] <- NULL
  }

  if (length(state) && !any_active_view(state)) {
    active_view(state) <- names(state)[[1L]]
  }

  dock_mgr$vs$state <- state

  if (was_active_removed && length(state)) {

    new_active <- active_view(state)

    if (exists(new_active, envir = dock_mgr$docks, inherits = FALSE)) {

      # switch-view must fire before show_view_ui so the target dock carries
      # blockr-view-dock-active; otherwise move-element drops DOM moves into
      # an inactive dock.
      session$sendCustomMessage(
        "switch-view",
        list(
          id = ns(as_view_handle_id(dock_mgr$docks[[new_active]]$dock_id))
        )
      )

      show_view_ui(new_active, dock_mgr$docks)
      update_active_dock(dock_mgr$active_dock, dock_mgr$docks[[new_active]])
    }

    dock_mgr$current_active(new_active)
  }

  for (v in rm_ids) {
    session$sendInputMessage("view_nav", list(remove = v))
  }

  if (length(state)) {
    session$sendInputMessage("view_nav", list(value = active_view(state)))
  }

  board
}

apply_views_add <- function(add_views, board, dock_mgr = NULL, session = NULL) {

  layouts <- board_layouts(board)

  for (v in names(add_views)) {

    layouts[[v]] <- set_active_view(add_views[[v]], active = FALSE)
  }

  board_layouts(board) <- layouts

  if (is.null(dock_mgr) || is.null(session)) {
    return(board)
  }

  ns <- session$ns

  for (v in names(add_views)) {

    # The view id (minted in augment_board_update.dock_board) is the dock
    # module id and the stem of the DOM ids — no separate runtime mint.
    if (exists(v, envir = dock_mgr$docks, inherits = FALSE)) {
      next
    }

    dock_output_id <- ns(NS(v, dock_id()))

    insertUI(
      selector = paste0("#", ns("view_container")),
      where = "beforeEnd",
      ui = div(
        id = ns(as_view_handle_id(v)),
        class = "blockr-view-dock",
        dockViewR::dock_view_output(
          dock_output_id,
          width = "100%",
          height = "100%"
        ),
        uiOutput(NS(ns(v), "empty_prompt"))
      ),
      immediate = TRUE,
      session = session
    )

    dock_res <- manage_dock(
      v,
      dock_mgr$board_rv,
      dock_mgr$update,
      dock_mgr$triggers,
      layout = add_views[[v]],
      blocks = board_blocks(board),
      extensions = dock_extensions(board)
    )
    dock_res$dock_id <- v
    dock_mgr$docks[[v]] <- dock_res

    session$sendInputMessage(
      "view_nav",
      list(add = list(id = v, name = view_name(add_views[[v]])))
    )
  }

  state <- isolate(dock_mgr$vs$state)

  for (v in names(add_views)) {

    state[[v]] <- bare_view(add_views[[v]])
  }

  dock_mgr$vs$state <- state

  board
}

apply_views_mod <- function(mod_views, board, dock_mgr = NULL) {

  layouts <- board_layouts(board)

  for (v in names(mod_views)) {

    was_active <- is_active_view(layouts[[v]])
    new <- set_active_view(mod_views[[v]], was_active)

    # A mod replaces the arrangement, not the identity: keep the display
    # name (the incoming layout carries it only on the live-sync path).
    nm <- coal(view_name(mod_views[[v]]), view_name(layouts[[v]]),
               fail_all = FALSE)
    if (!is.null(nm)) {
      view_name(new) <- nm
    }

    layouts[[v]] <- new
  }

  board_layouts(board) <- layouts

  if (is.null(dock_mgr)) {
    return(board)
  }

  for (v in names(mod_views)) {

    if (!exists(v, envir = dock_mgr$docks, inherits = FALSE)) {
      next
    }

    live <- tryCatch(
      isolate(dock_mgr$docks[[v]]$layout()),
      error = function(e) NULL
    )

    if (!is.null(live) && layouts_match(live, mod_views[[v]])) {
      next
    }

    apply_layout_diff(
      view = v,
      target = mod_views[[v]],
      proxy = dock_mgr$docks[[v]]$proxy,
      blocks = board_blocks(board),
      extensions = dock_extensions(board)
    )
  }

  board
}

# Rename one or more views: a name-attribute write keyed by stable id.
# `rename` is a named list mapping view id to its new display name. The
# id is untouched, so the dock module, DOM element and registry key all
# survive — no layout rebuild, no re-keying.
apply_views_rename <- function(rename, board, dock_mgr = NULL,
                               session = NULL) {

  layouts <- board_layouts(board)

  for (id in names(rename)) {
    if (id %in% names(layouts)) {
      view_name(layouts[[id]]) <- rename[[id]]
    }
  }

  board_layouts(board) <- layouts

  if (is.null(dock_mgr) || is.null(session)) {
    return(board)
  }

  state <- isolate(dock_mgr$vs$state)

  for (id in names(rename)) {
    if (id %in% names(state)) {
      view_name(state[[id]]) <- rename[[id]]
    }
  }

  dock_mgr$vs$state <- state

  for (id in names(rename)) {
    session$sendInputMessage(
      "view_nav",
      list(rename = list(id = id, to = rename[[id]]))
    )
  }

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

apply_views_active <- function(active, board, dock_mgr = NULL,
                               session = NULL) {

  layouts <- board_layouts(board)
  active_view(layouts) <- active
  board_layouts(board) <- layouts

  if (is.null(dock_mgr) || is.null(session)) {
    return(board)
  }

  ns <- session$ns
  state <- isolate(dock_mgr$vs$state)
  old_active <- active_view(state)

  if (identical(old_active, active)) {
    return(board)
  }

  session$sendCustomMessage(
    "switch-view",
    list(
      id = ns(as_view_handle_id(dock_mgr$docks[[active]]$dock_id))
    )
  )

  hide_view_ui(old_active, dock_mgr$docks)
  show_view_ui(active, dock_mgr$docks)

  active_view(state) <- active
  dock_mgr$vs$state <- state

  update_active_dock(dock_mgr$active_dock, dock_mgr$docks[[active]])
  dock_mgr$current_active(active)

  session$sendInputMessage(
    "view_nav",
    list(value = active)
  )

  board
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
