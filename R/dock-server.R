manage_dock <- function(board, session = get_session()) {

  exts <- isolate(
    dock_extensions(board$board)
  )

  session$output[[dock_id()]] <- dockViewR::render_dock_view(
    {
      log_debug("initializing empty dock {dock_id(session$ns)}")
      dockViewR::dock_view(
        panels = lapply(exts, extension_panel, session$ns(NULL)),
        defaultRenderer = "always"
      )
    }
  )

  observeEvent(
    dockViewR::get_dock(dock_id(), session),
    {
      layout <- dock_layout(board$board)
      if (is_empty_layout(layout)) {
        lapply(
          board_block_ids(board$board),
          add_block_panel,
          session = session
        )
      } else {
        restore_dock(layout, session)
      }
    },
    once = TRUE
  )

  initial_block_panels <- reactive(
    {
      layout <- dock_layout(board$board)

      if (is_empty_layout(layout)) {
        board_block_ids(board$board)
      } else {
        layout_panel_block_ids(layout)
      }
    }
  )

  observeEvent(
    initial_panels_avail(initial_block_panels()),
    lapply(
      initial_block_panels(),
      show_block_panel,
      add_panel = FALSE,
      session = session
    ),
    once = TRUE
  )

  reactive(
    dockViewR::get_dock(dock_id(), session)
  )
}

initial_panels_avail <- function(initial_panels, session = get_session()) {
  req(
    setequal(
      block_panel_id(initial_panels),
      dockViewR::get_panels_ids(dock_id(), session)
    )
  )
}

restore_dock <- function(layout, session = get_session()) {
  log_debug("restoring dockview layout")
  dockViewR::restore_dock(dock_id(), unclass(layout), session = session)
}
