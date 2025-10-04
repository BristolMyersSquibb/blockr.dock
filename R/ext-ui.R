extension_panel <- function(ext, id) {

  eid <- extension_panel_id(ext)

  log_debug("creating extension panel {eid}")

  dockViewR::panel(
    id = eid,
    title = extension_name(ext),
    content = extension_ui(id),
    style = list(
      overflow = "auto",
      height = "100%"
    )
  )
}

extension_panel_id <- function(ext_id, dock_id = NULL) {

  if (is_dock_extenstion(ext_id)) {
    ext_id <- list(ext_id)
  }

  if (is.list(ext_id)) {
    ext_id <- chr_ply(ext_id, extension_id)
  }

  stopifnot(is.character(ext_id), has_length(ext_id))

  res <- paste0("ext-", ext_id)

  if (is.null(dock_id)) {
    return(res)
  }

  stopifnot(is_string(dock_id))

  paste0(dock_id, "-", res)
}
