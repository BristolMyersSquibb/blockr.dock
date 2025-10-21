dock_panel_ids <- function(proxy) {
  as_dock_panel_id(
    dockViewR::get_panels_ids(proxy)
  )
}

block_panel_ids <- function(proxy) {
  res <- dock_panel_ids(proxy)

  as_block_panel_id(
    res[lgl_ply(res, is_block_panel_id)]
  )
}

remove_block_panel <- function(proxy, id) {
  pid <- as_block_panel_id(id)

  log_debug("removing block panel {pid} from dock {did}")

  dockViewR::remove_panel(proxy, pid)

  invisible(NULL)
}

add_block_panel <- function(proxy, id) {
  pan <- block_panel(id)

  log_debug("adding block {id} to dock {did}")

  dockViewR::add_panel(proxy, panel = pan)

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

remove_ext_panel <- function(proxy, id) {
  pid <- as_ext_panel_id(id)

  log_debug("removing extension panel {pid} from dock {did}")

  dockViewR::remove_panel(proxy, pid)

  invisible(NULL)
}

add_ext_panel <- function(proxy, ext) {
  stopifnot(is_dock_extension(ext))

  pan <- ext_panel(ext)

  log_debug("adding block {extension_id(ext)} to dock {did}")

  dockViewR::add_panel(proxy, panel = pan)

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

ext_panel_ids <- function(proxy) {
  res <- dock_panel_ids(proxy)

  as_ext_panel_id(
    res[lgl_ply(res, is_ext_panel_id)]
  )
}

restore_dock <- function(proxy, layout) {
  log_debug("restoring dockview layout")
  dockViewR::restore_dock(proxy, unclass(layout))
}
