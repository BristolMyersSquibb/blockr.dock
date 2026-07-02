library(blockr.core)
library(blockr.dock)

# A single view laid out as two side-by-side groups, so a focus change can be
# driven by activating the second group's panel. Used by the focus-persistence
# e2e test: a pure focus change must still turn the dock's `_state` over.
serve(
  new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_dataset_block()),
    layouts = list(Page = dock_layout("a", "b"))
  ),
  "my_board"
)
