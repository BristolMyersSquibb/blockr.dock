show_ext_panel <- function(ext, add_panel = TRUE, session = get_session()) {

  stopifnot(is_dock_extension(ext), is_bool(add_panel))

  if (add_panel) {
    add_ext_panel(ext, session)
  }

  show_ext_ui(ext, session)

  invisible(NULL)
}

hide_ext_panel <- function(id, rm_panel = TRUE, session = get_session()) {

  stopifnot(is_string(id), is_bool(rm_panel))

  hide_ext_ui(id, session)

  if (rm_panel) {
    remove_ext_panel(id, session)
  }

  invisible(NULL)
}

hide_ext_ui <- function(id, session) {

  ns <- session$ns
  id <- as_ext_handle_id(id)

  log_debug("hiding extension {ns(id)}")

  from <- paste0("#", paste(dock_id(ns), id, sep = "-"), " .card")
  to <- paste0("#", ns("offcanvas"), " .offcanvas-body")

  move_dom_element(from, to, session)
}

show_ext_ui <- function(id, session) {

  ns <- session$ns

  eid <- ns(as_ext_handle_id(id))
  pid <- paste(dock_id(ns), as_ext_panel_id(id), sep = "-")

  log_debug("showing extension {eid} in panel {pid}")

  move_dom_element(paste0("#", eid), paste0("#", pid), session)
}
