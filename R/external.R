# External-mutation hooks
#
# blockr.dock's view/layout state is normally driven by UI events
# (clicking a view tab, dragging a panel). These hooks expose the same
# mutations to programmatic callers — primarily blockr.mcp's MCP tool
# bodies, but available to any code in the same Shiny session.
#
# All hooks read state stashed on `session$userData$blockr_dock_handle`,
# which `board_server_callback()` populates on session start.

# Session-start hook registry. External packages (e.g. blockr.mcp)
# register a function with `dock_on_session_start()`; dock invokes
# every registered function from `board_server_callback()` once a
# Shiny session opens. Supported extension point for packages that
# need to know when a dock session is live and want access to the
# live board / board_update reactives — without going through the
# `callbacks` arg of `serve()` (which dock's app-server consumes for
# its own bookkeeping).
.dock_session_hooks <- new.env(parent = emptyenv())
.dock_session_hooks$cbs <- list()

#' Register a callback that runs when a dock Shiny session starts
#'
#' Receives the same arguments dock's own session callback receives:
#' the read-only board reactive, the `board_update` reactiveVal, and
#' the Shiny session. Use this to capture the live session in your
#' own package state.
#'
#' Multiple hooks can register under different names; calling with the
#' same name replaces the existing one.
#'
#' @param name A stable identifier for the hook (used for replacement).
#' @param fn A function `function(board, update, session)`.
#' @return Invisibly, NULL.
#' @export
dock_on_session_start <- function(name, fn) {
  stopifnot(
    is.character(name), length(name) == 1L, nzchar(name),
    is.function(fn)
  )
  .dock_session_hooks$cbs[[name]] <- fn
  invisible()
}

# Run all registered session-start hooks. Called from
# `board_server_callback()`. Errors in individual hooks are caught
# and surfaced as warnings so one bad hook can't take down the dock.
run_session_start_hooks <- function(board, update, session) {
  msg <- sprintf(
    "[blockr.dock build:2026-04-30-debug-4] run_session_start_hooks in PID %d, %d hook(s) registered: [%s]\n",
    Sys.getpid(),
    length(.dock_session_hooks$cbs),
    paste(names(.dock_session_hooks$cbs), collapse = ", ")
  )
  cat(msg, file = stderr())
  cat(msg, file = stdout())
  cat(msg, file = "/tmp/blockr-mcp-debug.log", append = TRUE)
  for (nm in names(.dock_session_hooks$cbs)) {
    tryCatch(
      .dock_session_hooks$cbs[[nm]](board, update, session),
      error = function(e) {
        warning("dock session-start hook '", nm, "' failed: ",
                conditionMessage(e), call. = FALSE)
      }
    )
  }
}

#' Stash the dock state handle on the Shiny session
#'
#' Called from `board_server_callback()`. Makes view-switching and
#' layout-setting from outside UI events possible by exposing `vs` and
#' `dock_mgr` via `session$userData`.
#'
#' @param session The Shiny session.
#' @param vs The view-state reactive container created by
#'   `init_view_docks()`.
#' @param dock_mgr The dock manager from `new_dock_manager()`.
#' @param board The read-only board reactive.
#' @param update The `board_update` reactiveVal.
#' @param triggers The action triggers from `action_triggers()`.
#' @return Invisibly, NULL.
#' @noRd
stash_dock_handle <- function(session, vs, dock_mgr,
                              board = NULL, update = NULL,
                              triggers = NULL) {
  session$userData$blockr_dock_handle <- list(
    vs       = vs,
    dock_mgr = dock_mgr,
    board    = board,
    update   = update,
    triggers = triggers
  )
  invisible()
}

#' Retrieve the dock state handle from the current Shiny session
#'
#' Returns the dock handle stashed by `board_server_callback()` on
#' session start. Used by external mutation helpers and by any caller
#' that needs read access to view membership / active view / live
#' layouts.
#'
#' @param session The Shiny session. Defaults to the current one.
#' @return A list with `vs` (view state) and `dock_mgr` (dock manager),
#'   or `NULL` if not stashed.
#' @export
dock_get_handle <- function(session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session)) return(NULL)
  session$userData$blockr_dock_handle
}

#' Switch the active view programmatically
#'
#' Drives view switching from outside the dock UI events. Same effect
#' as the user clicking a view tab.
#'
#' @param view_name Name of the view to activate.
#' @param session The Shiny session. Defaults to the current one.
#' @return Invisibly, the new active view name.
#' @export
dock_set_active_view <- function(view_name,
                                 session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session)) {
    stop("dock_set_active_view() must be called inside a Shiny session.",
         call. = FALSE)
  }
  h <- dock_get_handle(session)
  if (is.null(h)) {
    stop("No dock handle on this session — is the dock app running?",
         call. = FALSE)
  }
  if (!view_name %in% names(h$dock_mgr$docks)) {
    stop(sprintf("View '%s' does not exist. Available: %s",
                 view_name,
                 paste(names(h$dock_mgr$docks), collapse = ", ")),
         call. = FALSE)
  }

  do_switch_view(view_name, h$vs, session, h$dock_mgr)

  invisible(view_name)
}

# Lifted from `switch_view_observer()` so both the UI observer and the
# programmatic API run the same code path.
do_switch_view <- function(new_v, vs, session, dock_mgr) {
  state <- vs$state
  old_v <- active_view(state)

  session$sendCustomMessage(
    "switch-view",
    list(
      id = session$ns(
        paste0("view_wrap_", dock_mgr$docks[[new_v]]$dock_id)
      )
    )
  )

  if (!identical(old_v, new_v)) {
    hide_view_ui(old_v, dock_mgr$docks)
    show_view_ui(new_v, dock_mgr$docks)

    active_view(state) <- new_v
    vs$state <- state
    update_active_dock(dock_mgr$active_dock, dock_mgr$docks[[new_v]])
    dock_mgr$current_active(new_v)
  }
  invisible()
}

#' Read the current dock state
#'
#' Returns the current view membership, active view, and live layouts
#' (reflecting any drag-and-drop the user has done). Useful for the MCP
#' inspection tools that need to surface the live dock state to the LLM.
#'
#' @param session The Shiny session. Defaults to the current one.
#' @return A list with `views` (character vector of view names),
#'   `active_view` (character), and `layouts` (a `dock_layouts` object),
#'   or `NULL` if the dock isn't initialised on this session.
#' @export
dock_state <- function(session = shiny::getDefaultReactiveDomain()) {
  h <- dock_get_handle(session)
  if (is.null(h)) return(NULL)
  state <- h$vs$state
  list(
    views       = names(state),
    active_view = as.character(active_view(state)),
    layouts     = shiny::isolate(
      live_view_data(h$vs, h$dock_mgr)()
    )
  )
}

# ---------------------------------------------------------------------------
# Programmatic layout / view mutation (in-session redraw)
#
# These rebuild a view's dock module inside the LIVE Shiny session
# rather than re-serving the app. They reuse the same create/destroy
# choreography as add_view_observer()/remove_view_observer(); a layout
# or view change flickers and resets in-block interactive state for the
# rebuilt view, which is acceptable for agent-initiated (not drag)
# layout changes. See blockr.mcp's layout tools.
# ---------------------------------------------------------------------------

# Tear down a view's dock module and rebuild it with `new_layout`,
# inside the current session. Mirrors the confirm_view_add body.
do_rebuild_view <- function(view_name, new_layout, h, session) {
  # manage_dock() / moduleServer() / insertUI() resolve the Shiny
  # session via the default reactive domain. The MCP tool body runs
  # outside it (shiny::isolate is not enough), so establish the
  # captured session as the domain for the whole rebuild.
  shiny::withReactiveDomain(session, {
  vs <- h$vs
  dock_mgr <- h$dock_mgr
  ns <- session$ns

  was_active <- identical(as.character(active_view(vs$state)), view_name)

  if (exists(view_name, envir = dock_mgr$docks, inherits = FALSE)) {
    old <- dock_mgr$docks[[view_name]]
    if (was_active) hide_view_ui(view_name, dock_mgr$docks)
    destroy_module(old$dock_id, session = session)
    shiny::removeUI(
      selector = paste0("#", ns(paste0("view_wrap_", old$dock_id))),
      immediate = TRUE,
      session = session
    )
    rm(list = view_name, envir = dock_mgr$docks)
  }

  v_id <- dock_mgr$next_id()
  dock_output_id <- ns(shiny::NS(v_id, dock_id()))

  shiny::insertUI(
    selector = paste0("#", ns("view_container")),
    where = "beforeEnd",
    ui = shiny::div(
      id = ns(paste0("view_wrap_", v_id)),
      class = "blockr-view-dock",
      dockViewR::dock_view_output(
        dock_output_id,
        width = "100%",
        height = "100%"
      ),
      shiny::uiOutput(shiny::NS(ns(v_id), "empty_prompt"))
    )
  )

  dock_res <- manage_dock(v_id, h$board, h$update, h$triggers,
                          layout = new_layout)
  dock_res$dock_id <- v_id
  dock_mgr$docks[[view_name]] <- dock_res

  if (was_active) {
    session$sendCustomMessage(
      "switch-view",
      list(id = ns(paste0("view_wrap_", v_id)))
    )
    show_view_ui(view_name, dock_mgr$docks)
    update_active_dock(dock_mgr$active_dock, dock_mgr$docks[[view_name]])
    dock_mgr$current_active(view_name)
  }
  invisible()
  })
}

# Split a flat vector of ids referenced by a grid into board block ids
# and dock extension ids, against the live board.
.split_layout_ids <- function(ids, brd) {
  ids <- unique(ids[nzchar(ids)])
  list(
    blocks = intersect(ids, board_block_ids(brd)),
    exts   = intersect(ids, dock_ext_ids(brd))
  )
}

#' Read the active view's layout
#'
#' Returns the active view name, all view names, and the active view's
#' live layout (reflecting drag-and-drop), for the MCP `get_layout`
#' tool.
#'
#' @param session The Shiny session. Defaults to the current one.
#' @return A list with `active_view`, `views`, and `layout`
#'   (a `dock_layout`), or `NULL` if the dock isn't initialised.
#' @export
dock_get_layout <- function(session = shiny::getDefaultReactiveDomain()) {
  h <- dock_get_handle(session)
  if (is.null(h)) return(NULL)
  state <- h$vs$state
  av <- as.character(active_view(state))
  raw <- shiny::isolate(
    tryCatch(h$dock_mgr$docks[[av]]$layout(), error = function(e) NULL)
  )
  # Right after a programmatic rebuild the view's layout() reactive
  # is NULL until the browser re-renders. Degrade gracefully instead
  # of erroring in as_dock_layout(NULL).
  if (is.null(raw)) {
    return(list(
      active_view = av,
      views       = names(state),
      panels      = character(0L),
      layout      = NULL
    ))
  }
  ly <- shiny::isolate(as_dock_layout(raw))
  list(
    active_view = av,
    views       = names(state),
    panels      = as_dock_panel_id(ly),
    layout      = ly
  )
}

#' Set the active view's layout from a grid
#'
#' Rebuilds the active view's dock in-session with a layout built from
#' `grid` (the nested-list grid format `create_dock_layout()` accepts:
#' a string is a panel, an array of strings tabs them, nesting splits).
#' Block/extension membership is derived from the ids the grid
#' references.
#'
#' @param grid Nested list of block/extension ids.
#' @param session The Shiny session. Defaults to the current one.
#' @return Invisibly, the active view name.
#' @export
dock_set_layout <- function(grid,
                            session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session)) {
    stop("dock_set_layout() must be called inside a Shiny session.",
         call. = FALSE)
  }
  h <- dock_get_handle(session)
  if (is.null(h) || is.null(h$board)) {
    stop("No dock handle on this session — is the dock app running?",
         call. = FALSE)
  }
  brd <- h$board$board
  av <- as.character(active_view(h$vs$state))

  ids <- unlist(grid, use.names = FALSE)
  sel <- .split_layout_ids(ids, brd)
  missing <- setdiff(ids, c(sel$blocks, sel$exts))
  if (length(missing)) {
    stop(sprintf("Unknown id(s) in grid: %s",
                 paste(missing, collapse = ", ")), call. = FALSE)
  }

  new_layout <- create_dock_layout(
    blocks = board_blocks(brd)[sel$blocks],
    extensions = as_dock_extensions(
      as.list(dock_extensions(brd))[sel$exts]
    ),
    grid = grid
  )
  do_rebuild_view(av, new_layout, h, session)
  invisible(av)
}

#' Add a view programmatically
#'
#' Creates a new view containing the given blocks/extensions (default:
#' all board blocks and extensions) and switches to it. Same effect as
#' the "New view" dialog.
#'
#' @param name New view name.
#' @param blocks Block ids to include. `NULL` = all board blocks.
#' @param exts Extension ids to include. `NULL` = all extensions.
#' @param session The Shiny session. Defaults to the current one.
#' @return Invisibly, the new view name.
#' @export
dock_add_view <- function(name, blocks = NULL, exts = NULL,
                          session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session)) {
    stop("dock_add_view() must be called inside a Shiny session.",
         call. = FALSE)
  }
  h <- dock_get_handle(session)
  if (is.null(h) || is.null(h$board)) {
    stop("No dock handle on this session — is the dock app running?",
         call. = FALSE)
  }
  state <- h$vs$state
  name <- trimws(name)
  msg <- validate_view_name(name, names(state))
  if (!is.null(msg)) stop(msg, call. = FALSE)

  brd <- h$board$board
  blk_ids <- if (is.null(blocks)) board_block_ids(brd) else
    intersect(blocks, board_block_ids(brd))
  ext_ids <- if (is.null(exts)) dock_ext_ids(brd) else
    intersect(exts, dock_ext_ids(brd))

  v_ly <- create_dock_layout(
    blocks = board_blocks(brd)[blk_ids],
    extensions = as_dock_extensions(
      as.list(dock_extensions(brd))[ext_ids]
    )
  )

  shiny::withReactiveDomain(session, {
  state[[name]] <- list()
  h$vs$state <- state

  ns <- session$ns
  dock_mgr <- h$dock_mgr
  v_id <- dock_mgr$next_id()
  dock_output_id <- ns(shiny::NS(v_id, dock_id()))

  shiny::insertUI(
    selector = paste0("#", ns("view_container")),
    where = "beforeEnd",
    ui = shiny::div(
      id = ns(paste0("view_wrap_", v_id)),
      class = "blockr-view-dock",
      dockViewR::dock_view_output(
        dock_output_id,
        width = "100%",
        height = "100%"
      ),
      shiny::uiOutput(shiny::NS(ns(v_id), "empty_prompt"))
    )
  )

  dock_res <- manage_dock(v_id, h$board, h$update, h$triggers,
                          layout = v_ly)
  dock_res$dock_id <- v_id
  dock_mgr$docks[[name]] <- dock_res

  old_v <- as.character(active_view(state))

  session$sendInputMessage("view_nav", list(add = name))
  session$sendCustomMessage(
    "switch-view",
    list(id = ns(paste0("view_wrap_", v_id)))
  )

  # Switch active view SERVER-SIDE too. The UI add-view observer
  # relies on a client round-trip (view_nav input -> switch_view
  # observer) to advance active_view; the external/MCP path has no
  # round-trip before the next tool call, so without this a following
  # dock_set_layout() would target the previously-active view.
  if (!identical(old_v, name)) {
    hide_view_ui(old_v, dock_mgr$docks)
    show_view_ui(name, dock_mgr$docks)
    active_view(state) <- name
    h$vs$state <- state
    update_active_dock(dock_mgr$active_dock, dock_mgr$docks[[name]])
    dock_mgr$current_active(name)
  }
  })
  invisible(name)
}

#' Remove a view programmatically
#'
#' Destroys the view's dock module and switches away if it was active.
#' Refuses to remove the last remaining view.
#'
#' @param name View name to remove.
#' @param session The Shiny session. Defaults to the current one.
#' @return Invisibly, the new active view name.
#' @export
dock_remove_view <- function(name,
                             session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session)) {
    stop("dock_remove_view() must be called inside a Shiny session.",
         call. = FALSE)
  }
  h <- dock_get_handle(session)
  if (is.null(h)) {
    stop("No dock handle on this session — is the dock app running?",
         call. = FALSE)
  }
  vs <- h$vs
  dock_mgr <- h$dock_mgr
  ns <- session$ns
  state <- vs$state

  if (!name %in% names(state)) {
    stop(sprintf("View '%s' does not exist. Available: %s",
                 name, paste(names(state), collapse = ", ")),
         call. = FALSE)
  }
  if (length(state) <= 1L) {
    stop("Cannot remove the last view.", call. = FALSE)
  }

  was_active <- identical(as.character(active_view(state)), name)

  if (exists(name, envir = dock_mgr$docks, inherits = FALSE)) {
    rm_dock <- dock_mgr$docks[[name]]
    if (was_active) hide_view_ui(name, dock_mgr$docks)
    destroy_module(rm_dock$dock_id, session = session)
    shiny::removeUI(
      selector = paste0("#", ns(paste0("view_wrap_", rm_dock$dock_id))),
      immediate = TRUE,
      session = session
    )
    rm(list = name, envir = dock_mgr$docks)
  }

  state[[name]] <- NULL
  if (was_active) active_view(state) <- names(state)[1L]
  vs$state <- state
  active_name <- as.character(active_view(state))

  session$sendCustomMessage(
    "switch-view",
    list(
      id = ns(paste0("view_wrap_", dock_mgr$docks[[active_name]]$dock_id))
    )
  )
  if (was_active) {
    show_view_ui(active_name, dock_mgr$docks)
    update_active_dock(dock_mgr$active_dock, dock_mgr$docks[[active_name]])
  }
  session$sendInputMessage(
    "view_nav",
    list(remove = name, value = active_name)
  )
  invisible(active_name)
}

#' Rename a view programmatically
#'
#' Changes a view's name (the label in the view selector). Mirrors the
#' dock UI rename. The dock module keeps its `dock_id`, so DOM lookups
#' are unaffected.
#'
#' @param old Current view name.
#' @param new New view name.
#' @param session The Shiny session. Defaults to the current one.
#' @return Invisibly, the new view name.
#' @export
dock_rename_view <- function(old, new,
                             session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session)) {
    stop("dock_rename_view() must be called inside a Shiny session.",
         call. = FALSE)
  }
  h <- dock_get_handle(session)
  if (is.null(h)) {
    stop("No dock handle on this session — is the dock app running?",
         call. = FALSE)
  }
  vs <- h$vs
  dock_mgr <- h$dock_mgr
  state <- vs$state

  if (!old %in% names(state)) {
    stop(sprintf("View '%s' does not exist. Available: %s",
                 old, paste(names(state), collapse = ", ")),
         call. = FALSE)
  }
  new <- trimws(new)
  if (identical(old, new)) {
    return(invisible(new))
  }
  msg <- validate_view_name(new, setdiff(names(state), old))
  if (!is.null(msg)) stop(msg, call. = FALSE)

  shiny::withReactiveDomain(session, {
    nms <- names(state)
    nms[match(old, nms)] <- new
    names(state) <- nms
    if (identical(as.character(active_view(state)), old)) {
      active_view(state) <- new
    }
    vs$state <- state

    if (exists(old, envir = dock_mgr$docks, inherits = FALSE)) {
      dock_mgr$docks[[new]] <- dock_mgr$docks[[old]]
      rm(list = old, envir = dock_mgr$docks)
    }

    # Refresh the view selector widget client-side (the observer path
    # relies on the reactive cycle; the external path is outside it).
    session$sendInputMessage(
      "view_nav",
      list(rename = list(from = old, to = new))
    )
  })
  invisible(new)
}
