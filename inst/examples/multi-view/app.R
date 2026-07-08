library(blockr.core)
library(blockr.dock)

serve(
  new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_dataset_block()),
    views = list(First = "a", Second = "b"),
    active = "First"
  ),
  "my_board"
)
