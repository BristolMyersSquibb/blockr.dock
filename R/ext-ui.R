show_ext_panel <- function(ext, add_panel = TRUE, dock, ...) {

  if (isTRUE(add_panel)) {
    add_ext_panel(ext, dock = dock)
  } else if (isFALSE(add_panel)) {
    select_ext_panel(ext, dock$proxy)
  } else {
    add_ext_panel(ext, position = add_panel, dock = dock)
  }

  show_ext_ui(ext, dock$proxy$session,
              board_ns = dock_board_ns(dock), ...)

  invisible(NULL)
}

hide_ext_panel <- function(id, rm_panel = TRUE, dock, ...) {

  hide_ext_ui(id, dock$proxy$session,
              board_ns = dock_board_ns(dock), ...)

  if (isTRUE(rm_panel)) {
    remove_ext_panel(id, dock)
  }

  invisible(NULL)
}

hide_ext_ui <- function(id, session, board_ns = session$ns) {

  eid <- board_ns(as_ext_handle_id(id))
  oid <- paste0(board_ns("exts_offcanvas"), " .offcanvas-body")

  log_debug("hiding extension {eid} in {oid}")

  move_dom_element(paste0("#", eid), paste0("#", oid), session)
}

show_ext_ui <- function(id, session, board_ns = session$ns) {

  # board_ns: board-level namespace for DOM element IDs (handles, offcanvas).
  # session$ns: dock-module namespace for dock panel IDs.
  # These differ when called from a nested dock module (views).
  eid <- board_ns(as_ext_handle_id(id))
  pid <- paste(dock_id(session$ns), as_ext_panel_id(id), sep = "-")

  log_debug("showing extension {eid} in panel {pid}")

  move_dom_element(paste0("#", eid), paste0("#", pid), session)
}
