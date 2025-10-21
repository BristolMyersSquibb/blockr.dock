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
  dock_proxy <- dockViewR::dock_view_proxy(
    id = dock_id(),
    session = session
  )

  session$output[[dock_id()]] <- dockViewR::render_dock_view(
    {
      log_debug("initializing empty dock {dock_id(session$ns)}")
      dockViewR::dock_view(defaultRenderer = "always")
    }
  )

  observeEvent(
    req(session$input[[sprintf("%s_initialized", dock_id())]]),
    {
      layout <- dock_layout(board$board)

      if (is_empty_layout(layout)) {
        for (id in board_block_ids(board$board)) {
          add_block_panel(dock_proxy, id)
        }
        for (ext in dock_extensions(board$board)) {
          add_ext_panel(dock_proxy, ext)
        }
      } else {
        restore_dock(dock_proxy, layout)
      }
    },
    once = TRUE
  )

  initial_panels <- reactive(
    {
      layout <- dock_layout(board$board)

      if (is_empty_layout(layout)) {
        as_dock_panel_id(board$board)
      } else {
        as_dock_panel_id(layout)
      }
    }
  )

  observeEvent(
    req(all(initial_panels() %in% dock_panel_ids(dock_proxy))),
    {
      exts <- dock_extensions(board$board)

      for (id in initial_panels()) {
        if (is_block_panel_id(id)) {
          show_block_panel(dock_proxy, id, add_panel = FALSE)
        } else if (is_ext_panel_id(id)) {
          ext <- which(chr_ply(exts, extension_id) == as_obj_id(id))
          ext <- exts[[ext]]

          show_ext_panel(dock_proxy, ext, add_panel = FALSE)
        } else {
          blockr_abort(
            "Malformed layout panel ID {id}.",
            class = "dock_layout_invalid"
          )
        }
      }
    },
    once = TRUE
  )

  reactive(dockViewR::get_dock(dock_proxy))
}
