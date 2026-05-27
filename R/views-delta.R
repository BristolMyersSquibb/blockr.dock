#' Resolve raw IDs in `views$add` / `views$mod` layouts to the
#' canonical `block_panel-...` / `ext_panel-...` form.
#'
#' Callers may submit layouts built with bare IDs (e.g.
#' `dock_layout("blk_a")`); resolving here normalises them before
#' validation and apply. Mirrors what `initialise_layout()` does at
#' board construction time. Resolution uses the *pre-rm* block set
#' (current ∪ `blocks$add`) so that user-submitted layouts referencing
#' a block being removed still resolve — `merge_views_mod()` then
#' drops those references on the canonical form before
#' `validate_views_delta()` checks post-state.
#'
#' @param views The `views` payload slice.
#' @param board The current `dock_board`.
#' @param upd The full augmented update payload (provides
#'   `blocks$add` for the resolution block set).
#'
#' @return The `views` slice with `add` / `mod` layouts rewritten.
#'
#' @noRd
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

#' Validate a `views` payload slice.
#'
#' Structural and cross-reference checks for the `views` slot of a
#' `dock_board` update payload. Run from
#' `augment_board_update.dock_board()` after in-core augmentation, so
#' panel-ID xrefs resolve against the post-augment block set.
#'
#' @param views The `views` payload slice
#'   (`list(add, mod, rm, active)`).
#' @param board The current `dock_board`.
#' @param upd The full augmented update payload (used to derive the
#'   post-state block set for panel-ID xref).
#'
#' @return `views` invisibly. Throws classed errors on invalid input.
#'
#' @noRd
validate_views_delta <- function(views, board, upd) {

  if (!is.list(views)) {
    blockr_abort(
      "`views` must be a list with optional `add`/`mod`/`rm`/`active`.",
      class = "dock_views_delta_invalid"
    )
  }

  unknown_keys <- setdiff(names(views), c("add", "mod", "rm", "active"))
  if (length(unknown_keys)) {
    blockr_abort(
      "Unknown `views` slice key{?s}: {unknown_keys}.",
      class = "dock_views_delta_invalid"
    )
  }

  add_names <- names(views$add) %||% character()
  mod_names <- names(views$mod) %||% character()
  rm_names <- views$rm %||% character()
  active <- views$active

  add_unnamed <- length(views$add) &&
    (is.null(names(views$add)) || any(!nzchar(add_names)))

  if (add_unnamed) {
    blockr_abort(
      "All entries of `views$add` must be named.",
      class = "dock_views_delta_unnamed"
    )
  }

  mod_unnamed <- length(views$mod) &&
    (is.null(names(views$mod)) || any(!nzchar(mod_names)))

  if (mod_unnamed) {
    blockr_abort(
      "All entries of `views$mod` must be named.",
      class = "dock_views_delta_unnamed"
    )
  }

  if (length(rm_names) && !is.character(rm_names)) {
    blockr_abort(
      "`views$rm` must be a character vector of view names.",
      class = "dock_views_delta_invalid"
    )
  }

  clash <- intersect(add_names, rm_names)
  if (length(clash)) {
    blockr_abort(
      "View{?s} {clash} cannot appear in both `views$add` and `views$rm`.",
      class = "dock_views_delta_add_rm_clash"
    )
  }

  mod_in_rm <- intersect(mod_names, rm_names)
  if (length(mod_in_rm)) {
    blockr_abort(
      "View{?s} {mod_in_rm} cannot appear in both `views$mod` and `views$rm`.",
      class = "dock_views_delta_mod_rm_clash"
    )
  }

  mod_in_add <- intersect(mod_names, add_names)
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
  unknown_mod <- setdiff(mod_names, current_views)
  if (length(unknown_mod)) {
    blockr_abort(
      "View{?s} {unknown_mod} in `views$mod` do not exist on the board.",
      class = "dock_views_delta_mod_unknown"
    )
  }

  unknown_rm <- setdiff(rm_names, current_views)
  if (length(unknown_rm)) {
    blockr_abort(
      "View{?s} {unknown_rm} in `views$rm` do not exist on the board.",
      class = "dock_views_delta_rm_unknown"
    )
  }

  add_clash <- intersect(add_names, current_views)
  if (length(add_clash)) {
    blockr_abort(
      "View{?s} {add_clash} in `views$add` already exist on the board.",
      class = "dock_views_delta_add_existing"
    )
  }

  post_views <- setdiff(c(current_views, add_names), rm_names)

  if (!length(post_views)) {
    blockr_abort(
      "Cannot remove all views: at least one view must remain.",
      class = "dock_views_delta_remove_all"
    )
  }

  if (!is.null(active)) {

    if (!is_string(active)) {
      blockr_abort(
        "`views$active` must be a single view name.",
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

  for (v in add_names) {

    if (!is_dock_layout(views$add[[v]])) {
      blockr_abort(
        "`views$add${v}` must be a `dock_layout`.",
        class = "dock_views_delta_layout_invalid"
      )
    }

    validate_layout_panel_refs(views$add[[v]], ok_panels, v)
  }

  for (v in mod_names) {

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

#' Drop panel IDs from a single `dock_layout`.
#'
#' Walks the layout's grid tree and removes any leaf views matching the
#' supplied panel IDs. Leaves that become empty are dropped; their
#' parent branches collapse if all children vanish. The active-view
#' marker on the layout is preserved.
#'
#' @param layout A `dock_layout`.
#' @param panel_ids Character vector of panel IDs to drop (the full
#'   `block_panel-...` / `ext_panel-...` form).
#'
#' @return The modified `dock_layout`.
#'
#' @noRd
drop_panels_from_layout <- function(layout, panel_ids) {

  if (!length(panel_ids)) {
    return(layout)
  }

  walk <- function(node) {

    if (is.null(node) || !length(node)) {
      return(NULL)
    }

    if (identical(node[["type"]], "leaf")) {

      views <- unlist(node[["data"]][["views"]], use.names = FALSE)
      kept <- setdiff(views, panel_ids)

      if (length(kept) == 0L) {
        return(NULL)
      }

      node[["data"]][["views"]] <- as.list(kept)

      av <- node[["data"]][["activeView"]]
      if (is.null(av) || !av %in% kept) {
        node[["data"]][["activeView"]] <- kept[[1L]]
      }

      return(node)
    }

    children <- lapply(node[["data"]], walk)
    children <- Filter(Negate(is.null), children)

    if (length(children) == 0L) {
      return(NULL)
    }

    node[["data"]] <- children
    node
  }

  was_active <- is_active_view(layout)

  result <- layout
  result[["grid"]][["root"]] <- walk(layout[["grid"]][["root"]])

  if (!length(grid_panel_ids(result[["grid"]]))) {
    result[["activeGroup"]] <- NULL
  }

  set_active_view(result, was_active)
}

#' Compute the per-view auto-augment for `blocks$rm`.
#'
#' For every current view layout that contains an affected panel,
#' returns a cleaned `dock_layout`. Pass the result through
#' [merge_views_mod()] to combine with any user-submitted `views$mod`.
#'
#' @param layouts The current `dock_layouts` on the board.
#' @param rm_block_ids Character vector of block IDs being removed.
#' @param skip_views Character vector of view names to skip (e.g.
#'   `views$rm` — no point cleaning a view that will be deleted).
#'
#' @return A named list (possibly empty) of `view_name -> dock_layout`.
#'
#' @noRd
drop_panels_from_layouts <- function(layouts, rm_block_ids,
                                     skip_views = character()) {

  if (!length(rm_block_ids)) {
    return(list())
  }

  rm_panels <- as.character(as_block_panel_id(rm_block_ids))

  out <- list()

  for (v in setdiff(names(layouts), skip_views)) {

    layout <- layouts[[v]]
    pids <- layout_panel_ids(layout)

    if (any(pids %in% rm_panels)) {
      out[[v]] <- drop_panels_from_layout(layout, rm_panels)
    }
  }

  out
}

#' Merge user-supplied `views$mod` with the block-removal augment.
#'
#' When both target the same view, the augment runs on top of the
#' user-supplied layout (drop the same panel IDs). When only one is
#' present, that one wins.
#'
#' @param user_mod User-supplied `views$mod` (may be `NULL`).
#' @param layouts Current `dock_layouts` (used for `skip_views`
#'   semantics).
#' @param rm_block_ids Character vector of block IDs being removed.
#' @param skip_views Views to skip entirely (e.g. `views$rm`).
#'
#' @return A named list of `view_name -> dock_layout` representing the
#'   merged `views$mod`.
#'
#' @noRd
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

apply_views_rm <- function(rm_names, board, dock_mgr = NULL, session = NULL) {

  layouts <- board_layouts(board)
  surviving <- setdiff(names(layouts), rm_names)
  layouts <- structure(
    as.list(unclass(layouts))[surviving],
    class = "dock_layouts"
  )

  if (length(layouts) && is.null(active_view(layouts))) {
    layouts[[1L]] <- set_active_view(layouts[[1L]])
  }

  board_layouts(board) <- layouts

  if (is.null(dock_mgr) || is.null(session)) {
    return(board)
  }

  ns <- session$ns
  state <- isolate(dock_mgr$vs$state)
  was_active_removed <- active_view(state) %in% rm_names

  for (v in rm_names) {

    if (exists(v, envir = dock_mgr$docks, inherits = FALSE)) {

      rm_dock <- dock_mgr$docks[[v]]

      destroy_module(rm_dock$dock_id, session = session)
      removeUI(
        selector = paste0("#", ns(paste0("view_wrap_", rm_dock$dock_id))),
        immediate = TRUE,
        session = session
      )
      rm(list = v, envir = dock_mgr$docks)
    }

    state[[v]] <- NULL
  }

  if (length(state) && is.null(active_view(state))) {
    active_view(state) <- names(state)[[1L]]
  }

  dock_mgr$vs$state <- state

  if (was_active_removed && length(state)) {

    new_active <- active_view(state)
    update_active_dock(dock_mgr$active_dock, dock_mgr$docks[[new_active]])
    dock_mgr$current_active(new_active)
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

    v_id <- dock_mgr$next_id()
    dock_output_id <- ns(NS(v_id, dock_id()))

    insertUI(
      selector = paste0("#", ns("view_container")),
      where = "beforeEnd",
      ui = div(
        id = ns(paste0("view_wrap_", v_id)),
        class = "blockr-view-dock",
        dockViewR::dock_view_output(
          dock_output_id,
          width = "100%",
          height = "100%"
        ),
        uiOutput(NS(ns(v_id), "empty_prompt"))
      ),
      immediate = TRUE,
      session = session
    )

    dock_res <- manage_dock(
      v_id,
      dock_mgr$board_rv,
      dock_mgr$update,
      dock_mgr$triggers,
      layout = add_views[[v]],
      blocks = board_blocks(board),
      extensions = dock_extensions(board)
    )
    dock_res$dock_id <- v_id
    dock_mgr$docks[[v]] <- dock_res

    session$sendInputMessage("view_nav", list(add = v))
  }

  state <- isolate(dock_mgr$vs$state)

  for (v in names(add_views)) {

    state[[v]] <- new_dock_layout()
  }

  dock_mgr$vs$state <- state

  board
}

apply_views_mod <- function(mod_views, board, dock_mgr = NULL) {

  layouts <- board_layouts(board)

  for (v in names(mod_views)) {

    was_active <- is_active_view(layouts[[v]])
    layouts[[v]] <- set_active_view(mod_views[[v]], was_active)
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

#' Test whether two layouts describe the same panel arrangement.
#'
#' Used by [apply_views_mod()] to short-circuit `restore_layout` when
#' the live dockView state already matches the target — avoids the
#' visual rebuild on UI-driven sync. Compares the normalised wire spec
#' (`layout_to_spec()`), which already drops volatile pixel sizes and
#' regenerated group IDs; we additionally drop `focus` so a
#' focus-only change (a tab click) doesn't force a rebuild. `a` is the
#' live dockview wire shape (`get_dock()` output); `b` is the target
#' `dock_layout`.
#'
#' @noRd
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
      id = ns(paste0("view_wrap_", dock_mgr$docks[[active]]$dock_id))
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

#' Apply a single view-layout diff.
#'
#' v1 implementation: unconditionally calls [restore_layout()] with the
#' target layout. Surgical diff paths (identical-layout noop, active-tab
#' only, panel reorder, etc.) are a deferred follow-up; the dispatch
#' point lives here so callers stay stable across that change.
#'
#' @param view Name of the view being modified (for logging / future
#'   diff dispatch).
#' @param target The desired `dock_layout`.
#' @param proxy The dockView proxy for this view.
#' @param blocks,extensions Current board components (for layout
#'   payload construction).
#'
#' @noRd
apply_layout_diff <- function(view, target, proxy, blocks, extensions) {
  log_debug("applying layout diff for view {view}")
  restore_layout(target, proxy, blocks = blocks, extensions = extensions)
}
