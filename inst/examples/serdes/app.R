library(blockr.core)
library(blockr.dock)

# Fixture for the serialization round-trip e2e: a board carrying every kind of
# dock-owned state -- two views with distinct layouts (a rich multi-panel view
# with custom sizes, an active panel and the extension panel; a single-panel
# view), a non-default active view, plus blocks, a link and a stack. The test
# exports this through the live Export/Import plugin and asserts the board
# rebuilds and re-renders identically after the restore reload.
serve(
  new_dock_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_head_block(),
      c = new_head_block()
    ),
    links = links(ab = new_link("a", "b", "data")),
    stacks = stacks(grp = c("a", "b")),
    extensions = new_edit_board_extension(),
    layouts = list(
      overview = dock_layout("c", name = "Overview"),
      analysis = dock_layout(
        "edit_board_extension",
        panels("a", "b", active = "b"),
        sizes = c(0.3, 0.7),
        name = "Analysis"
      )
    ),
    active = "analysis"
  ),
  "my_board"
)
