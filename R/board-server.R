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

  dock <- dock_proxy(session)

  session$output[[dock_id()]] <- dockViewR::render_dock_view(
    {
      log_debug("initializing empty dock {dock_id(session$ns)}")
      dockViewR::dock_view(defaultRenderer = "always")
    }
  )

  observeEvent(
    req(session$input[[paste0(dock_id(), "_initialized")]]),
    {
      layout <- dock_layout(board$board)

      if (is_empty_layout(layout)) {

        for (id in board_block_ids(board$board)) {
          show_block_panel(id, add_panel = TRUE, proxy = dock)
        }

        for (ext in dock_extensions(board$board)) {
          show_ext_panel(ext, add_panel = TRUE, proxy = dock)
        }

      } else {

        restore_dock(layout, dock)

        for (id in as_dock_panel_id(layout)) {

          if (is_block_panel_id(id)) {

            show_block_panel(id, add_panel = FALSE, proxy = dock)

          } else if (is_ext_panel_id(id)) {

            show_ext_panel(id, add_panel = FALSE, proxy = dock)

          } else {

            blockr_abort(
              "Unknown panel type {class(id)}.",
              class = "dock_panel_invalid"
            )
          }
        }
      }
    },
    once = TRUE
  )

  reactive(dockViewR::get_dock(dock))
}
