dock_panel_ids <- function(session = get_session()) {
  as_dock_panel_id(
    dockViewR::get_panels_ids(dock_id(), session)
  )
}

block_panel_ids <- function(session = get_session()) {

  res <- dock_panel_ids(session)

  as_block_panel_id(
    res[lgl_ply(res, is_block_panel_id)]
  )
}

remove_block_panel <- function(id, session = get_session()) {

  did <- dock_id()
  pid <- as_block_panel_id(id)

  log_debug("removing block panel {pid} from dock {did}")

  dockViewR::remove_panel(did, pid, session = session)

  invisible(NULL)
}

add_block_panel <- function(id, session = get_session()) {

  did <- dock_id()
  pan <- block_panel(id)

  log_debug("adding block {id} to dock {did}")

  dockViewR::add_panel(did, panel = pan, session = session)

  invisible(NULL)
}

block_panel <- function(id) {

  pid <- as_block_panel_id(id)

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

remove_ext_panel <- function(id, session = get_session()) {

  did <- dock_id()
  pid <- as_ext_panel_id(id)

  log_debug("removing extension panel {pid} from dock {did}")

  dockViewR::remove_panel(did, pid, session = session)

  invisible(NULL)
}

add_ext_panel <- function(ext, session = get_session()) {

  stopifnot(is_dock_extension(ext))

  did <- dock_id()
  pan <- ext_panel(ext)

  log_debug("adding block {extension_id(ext)} to dock {did}")

  dockViewR::add_panel(did, panel = pan, session = session)

  invisible(NULL)
}

ext_panel <- function(ext) {

  eid <- as_ext_panel_id(ext)

  log_debug("creating block panel {eid}")

  dockViewR::panel(
    id = eid,
    title = extension_name(ext),
    content = tagList(),
    style = list(
      overflow = "auto",
      height = "100%"
    )
  )
}

ext_panel_ids <- function(session = get_session()) {

  res <- dock_panel_ids(session)

  as_ext_panel_id(
    res[lgl_ply(res, is_ext_panel_id)]
  )
}

restore_dock <- function(layout, session = get_session()) {
  log_debug("restoring dockview layout")
  dockViewR::restore_dock(dock_id(), unclass(layout), session = session)
}

get_dock <- function(session = get_session()) {
  dockViewR::get_dock(dock_id(), session)
}
