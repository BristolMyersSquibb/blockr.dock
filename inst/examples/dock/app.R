library(blockr.core)
library(blockr.dock)

serve(
  new_dock_board(
    extensions = new_edit_board_extension()
  ),
  "my_board"
)
