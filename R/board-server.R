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

  session$output[[dock_id()]] <- dockViewR::render_dock_view(
    {
      log_debug("initializing empty dock {dock_id(session$ns)}")
      dockViewR::dock_view(
        panels = list(),
        defaultRenderer = "always"
      )
    }
  )

  observeEvent(
    dockViewR::get_dock(dock_id(), session),
    {
      layout <- dock_layout(board$board)

      if (is_empty_layout(layout)) {
        for (id in board_block_ids(board$board)) {
          add_block_panel(id, session)
        }
        for (ext in dock_extensions(board$board)) {
          add_ext_panel(ext, session)
        }
      } else {
        restore_dock(layout, session)
      }
    },
    once = TRUE
  )

  initial_panels <- reactive(
    {
      layout <- dock_layout(board$board)

      if (is_empty_layout(layout)) {
        c(
          block_panel_id(board$board),
          extension_panel_id(board$board)
        )
      } else {
        layout_panel_ids(layout)
      }
    }
  )

  observeEvent(
    req(all(initial_panels() %in% dock_panel_ids(session))),
    {
      exts <- dock_extensions(board$board)

      for (id in initial_panels()) {

        if (is_block_panel_id(id)) {

          show_block_panel(id, add_panel = FALSE, session = session)

        } else if (is_ext_panel_id(id)) {

          ext <- lgl_ply(exts, extension_id()) == ext_panel_id_to_ext_id(id)
          ext <- exts[[ext]]

          show_ext_panel(ext, add_panel = FALSE, session = session)

        }  else {

          blockr_abort(
            "Malformed layout panel ID {id}.",
            class = "dock_layout_invalid"
          )
        }
      }
    },
    once = TRUE
  )

  reactive(
    dockViewR::get_dock(dock_id(), session)
  )
}
