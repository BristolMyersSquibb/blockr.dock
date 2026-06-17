library(blockr.core)
library(blockr.dock)

# A board pre-seeded with a source and a transform block plus the edit-board
# extension, but no links or stacks. The edit-extension add-link and add-stack
# e2e tests drive only those add operations against it -- adding blocks would
# re-render the extension panel and race shinytest2 clicks.
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
