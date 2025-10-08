extension_panel <- function(ext, id, ...) {

  eid <- extension_panel_id(ext)

  log_debug("creating extension panel {eid}")

  dockViewR::panel(
    id = eid,
    title = extension_name(ext),
    content = extension_ui(ext, id, ...),
    style = list(
      overflow = "auto",
      height = "100%"
    )
  )
}
