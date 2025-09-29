manage_dock <- function(board, ..., session = get_session()) {

  session$output[[dock_id()]] <- dockViewR::render_dock_view(
    {
      log_debug("initializing empty dock {dock_id(session$ns)}")
      dockViewR::dock_view(
        panels = lapply(
          isolate(board_block_ids(board$board)),
          block_panel
        )
      )
    }
  )

  observeEvent(
    dockViewR::get_dock(dock_id(), session),
    {
      for (block in board_block_ids(board$board)) {
        show_block_panel(block, add_panel = FALSE, session = session)
      }
    },
    once = TRUE
  )
}
