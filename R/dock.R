manage_dock <- function(board, ..., session = get_session()) {

  initial_board <- isolate(board$board)
  board_id <- isolate(board$board_id)

	session$output$dock <- dockViewR::render_dock_view(
    {
      blocks <- board_blocks(initial_board)
      dockViewR::dock_view(
        panels = map(
          dockViewR::panel,
          id = names(blocks),
          title = chr_ply(blocks, block_name),
          content = block_ui(board_id, initial_board, blocks)
        )
      )
    }
  )
}
