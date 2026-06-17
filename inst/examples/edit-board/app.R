library(blockr.core)
library(blockr.dock)

# A populated board paired with the "Edit board" extension (the empty-board
# counterpart lives in inst/examples/empty). It ships with a link and a stack
# already in place so the extension's edit and removal flows can be driven
# without first building them up. The stack groups block "a" only, leaving "b"
# free to be deleted (cascading its link) without touching the stack.
serve(
  new_dock_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_head_block()
    ),
    links = links(ab = new_link(from = "a", to = "b", input = "data")),
    stacks = stacks(grp = new_dock_stack(blocks = "a", name = "Group A")),
    extensions = new_edit_board_extension()
  ),
  "my_board"
)
