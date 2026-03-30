library(blockr.core)
library(blockr.dock)

board <- new_dock_board(
  blocks = c(
    dataset_1 = new_dataset_block(),
    head_1 = new_head_block()
  ),
  links = new_link("dataset_1", "head_1"),
  extensions = new_edit_board_extension(),
  layout = dock_workspaces(
    Analysis = dock_workspace(
      layout = list("dataset_1", "head_1", "edit_board_extension")
    ),
    Overview = dock_workspace(
      layout = list("edit_board_extension")
    )
  )
)

serve(board, "my_board")
