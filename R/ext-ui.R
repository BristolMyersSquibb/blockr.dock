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

hide_ext_ui <- function(ids, session, board_ns = session$ns) {

  hid <- as_ext_handle_id(ids)

  if (!length(hid)) {
    return(invisible(NULL))
  }

  eid <- board_ns(hid)
  oid <- paste0(board_ns("exts_offcanvas"), " .offcanvas-body")

  log_debug("hiding {cli::qty(length(eid))}extension{?s} {eid} in {oid}")

  move_dom_elements(paste0("#", eid), paste0("#", oid), session)
}

show_ext_ui <- function(ids, session, board_ns = session$ns) {

  hid <- as_ext_handle_id(ids)

  if (!length(hid)) {
    return(invisible(NULL))
  }

  # board_ns: board-level namespace for DOM element IDs (handles, offcanvas).
  # session$ns: dock-module namespace for dock panel IDs.
  # These differ when called from a nested dock module (views).
  eid <- board_ns(hid)
  pid <- paste(dock_id(session$ns), as_ext_panel_id(ids), sep = "-")

  log_debug(
    "showing {cli::qty(length(eid))}extension{?s} {eid} in ",
    "{cli::qty(length(pid))}panel{?s} {pid}"
  )

  move_dom_elements(paste0("#", eid), paste0("#", pid), session)
}
