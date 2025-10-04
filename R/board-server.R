board_server_callback <- function(board, update, ..., session = get_session()) {

  layout <- manage_dock(board, session)

  exts <- isolate(
    dock_extensions(board$board)
  )

  intercom <- set_names(
    replicate(length(exts), reactiveVal()),
    exts
  )

  ext_state <- set_names(
    vector("list", length(exts)),
    names(exts)
  )

  for (i in names(exts)) {
    ext_state[[i]] <- extension_server(
      exts[[i]],
      list(board = board, update = update, layout = layout),
      intercom,
      list(...)
    )
  }

  c(
    list(layout = layout),
    ext_state
  )
}

manage_dock <- function(board, session = get_session()) {

  initial_board <- isolate(board$board)

  session$output[[dock_id()]] <- dockViewR::render_dock_view(
    {
      panels <- lapply(
        dock_extensions(initial_board),
        extension_panel,
        id = session$ns(NULL),
        board = initial_board
      )
      log_debug(
        "initializing dock {dock_id(session$ns)} with {length(panels)} ",
        "panel{?s}"
      )
      dockViewR::dock_view(
        panels = unname(panels),
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
    all(
      block_panel_id(initial_panels) %in%
        dockViewR::get_panels_ids(dock_id(), session)
    )
  )
}

restore_dock <- function(layout, session = get_session()) {
  log_debug("restoring dockview layout")
  dockViewR::restore_dock(dock_id(), unclass(layout), session = session)
}
