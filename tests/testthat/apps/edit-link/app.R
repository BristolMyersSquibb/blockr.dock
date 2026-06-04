library(blockr.core)
library(blockr.dock)

# Fixture for the edit-extension links e2e test: a board pre-seeded with a
# source and a transform block so the test drives only link operations
# (adding blocks mutates the board, which re-renders the extension panel and
# races shinytest2 clicks). Mirrors apps/edit-stacks.
serve(
  new_dock_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_head_block()
    ),
    extensions = new_edit_board_extension()
  ),
  "my_board"
)
