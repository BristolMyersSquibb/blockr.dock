dock_panel_ids <- function(proxy) {
  as_dock_panel_id(
    dockViewR::get_panels_ids(proxy)
  )
}

dock_panel_named_ids <- function(proxy) {
  panels <- dockViewR::get_panels(proxy)
  set_names(chr_xtr(panels, "id"), chr_xtr(panels, "title"))
}

block_panel_ids <- function(proxy) {

  res <- dock_panel_ids(proxy)

  as_block_panel_id(
    res[lgl_ply(res, is_block_panel_id)]
  )
}

remove_block_panel <- function(proxy, id) {

  pid <- as_block_panel_id(id)

  log_debug("removing block panel {pid}")

  dockViewR::remove_panel(proxy, pid)

  invisible(NULL)
}

add_block_panel <- function(proxy, id) {

  log_debug("adding block panel {as_block_panel_id(id)}")

  dockViewR::add_panel(proxy, panel = block_panel(id))

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

  log_debug("removing extension panel {pid}")

  dockViewR::remove_panel(proxy, pid)

  invisible(NULL)
}

add_ext_panel <- function(proxy, ext) {

  stopifnot(is_dock_extension(ext))

  log_debug("adding block {as_ext_panel_id(ext)}")

  dockViewR::add_panel(proxy, panel = ext_panel(ext))

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
