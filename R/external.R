# External-mutation hooks
#
# blockr.dock's view/layout state is normally driven by UI events
# (clicking a view tab, dragging a panel). These hooks expose the same
# mutations to programmatic callers — primarily blockr.mcp's MCP tool
# bodies, but available to any code in the same Shiny session.
#
# All hooks read state stashed on `session$userData$blockr_dock_handle`,
# which `board_server_callback()` populates on session start.

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
#' @return Invisibly, NULL.
#' @noRd
stash_dock_handle <- function(session, vs, dock_mgr) {
  session$userData$blockr_dock_handle <- list(
    vs       = vs,
    dock_mgr = dock_mgr
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
