library(blockr.core)
library(blockr.dock)

# A board authored with a NON-default grid (explicit sizes): the round-trip
# stability sentinel compares the stored authored grid against the live grid the
# client echoes, and a plain default (a member the grid omits) would render the
# same both sides and pass without exercising the sizes. The sentinel reads the
# `roundtrip_stable` test export (gated by Shiny's test mode) off this board.
serve(
  new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_dataset_block()),
    grids = list(Main = dock_grid("a", "b", sizes = c(0.3, 0.7)))
  ),
  "my_board"
)
