library(blockr.core)
library(blockr.dock)

# Fixture for the append-block e2e test: a board pre-seeded with a single
# source block, so the test drives only the append flow from that block's
# context menu (the block-card dropdown's "Append block" item). Mirrors
# apps/edit-link and apps/edit-stacks.
serve(
  new_dock_board(
    blocks = c(a = new_dataset_block("iris"))
  ),
  "my_board"
)
