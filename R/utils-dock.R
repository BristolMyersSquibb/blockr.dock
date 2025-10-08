dock_panel_ids <- function(session = get_session()) {
	dockViewR::get_panels_ids(dock_id(), session)
}

restore_dock <- function(layout, session = get_session()) {
  log_debug("restoring dockview layout")
  dockViewR::restore_dock(dock_id(), unclass(layout), session = session)
}
