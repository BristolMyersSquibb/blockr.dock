library(blockr.core)
library(blockr.dock)

serve(
  new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_dataset_block()),
    layouts = list(First = dock_layout("a"), Second = dock_layout("b")),
    active = "First"
  ),
  "my_board"
)
