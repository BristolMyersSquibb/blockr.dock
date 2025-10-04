show_block_panel <- function(id, add_panel = TRUE, session = get_session()) {

  stopifnot(is_string(id), is_bool(add_panel))

  ns <- session$ns

  if (add_panel) {
    add_block_panel(id, session)
  }

  bid <- ns(id)
  pid <- block_panel_id(id, dock_id(ns))

  log_debug("showing block {bid} in panel {pid}")

  session$sendCustomMessage(
    "show-block",
    list(
      block_id = paste0("#", bid),
      panel_id = paste0("#", pid)
    )
  )

  invisible(NULL)
}

hide_block_panel <- function(id, rm_panel = TRUE, session = get_session()) {

  stopifnot(is_string(id), is_bool(rm_panel))

  ns <- session$ns

  session$sendCustomMessage(
    "hide-block",
    list(
      offcanvas = paste0("#", ns("offcanvas")),
      block_id = paste0("#", block_panel_id(id, dock_id(ns)))
    )
  )

  if (rm_panel) {
    remove_block_panel(id, session)
  }

  invisible(NULL)
}

remove_block_panel <- function(id, session = get_session()) {
  did <- dock_id()
  pid <- block_panel_id(id)
  log_debug("removing block panel {pid} from dock {did}")
  dockViewR::remove_panel(did, pid, session = session)
}

add_block_panel <- function(id, session = get_session()) {

  did <- dock_id()
  pan <- block_panel(id)

  log_debug("adding block {id} to dock {did}")

  dockViewR::add_panel(did, panel = pan, session = session)

  invisible(NULL)
}

block_panel <- function(id) {

  pid <- block_panel_id(id)

  log_debug("creating block panel {pid}")

  dockViewR::panel(
    id = pid,
    title = paste("Block:", id),
    content = tagList(),
    style = list(
      overflow = "auto",
      height = "100%"
    )
  )
}

list_block_panels <- function(session = get_session()) {
  res <- dockViewR::get_panels_ids(dock_id(), session)
  res[is_block_panel_id(res)]
}
