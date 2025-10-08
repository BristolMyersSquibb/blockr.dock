add_ext_panel <- function(ext, session = get_session()) {

  stopifnot(is_dock_extension(ext))

  did <- dock_id()
  pan <- ext_panel(ext)

  log_debug("adding block {id} to dock {did}")

  dockViewR::add_panel(did, panel = pan, session = session)

  invisible(NULL)
}

ext_panel <- function(ext) {

  eid <- extension_panel_id(ext)

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

show_ext_panel <- function(ext, add_panel = TRUE, session = get_session()) {

  stopifnot(is_dock_extension(ext), is_bool(add_panel))

  invisible(NULL)
}
