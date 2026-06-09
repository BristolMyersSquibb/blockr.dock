library(blockr.core)
library(blockr.dock)

# Fixture for the edit-extension stacks e2e test: a board pre-seeded with two
# blocks so the test drives only stack operations (adding blocks mutates the
# board, which re-renders the extension panel and races shinytest2 clicks).
serve(
  new_dock_board(
    blocks = c(
      data = new_dataset_block("iris"),
      more = new_dataset_block("mtcars")
    ),
    extensions = new_edit_board_extension()
  ),
  "my_board"
)
