library(blockr.core)
library(blockr.dock)

# Repro: rename observer crashes when the active view does not contain a
# panel for the renamed block.
#
# This mirrors the layout pattern used in real dashboards (e.g. CDEx POC):
# blocks are split across multiple `dock_view()`s, so any block that is not
# in the active view lives in the offcanvas. Renaming such a block from the
# offcanvas (pen-to-square icon next to its title) triggers the crash.
#
# Steps:
#   1. Open the app — `Setup` is the active view, showing only `dataset_1`.
#      The other three blocks (`head_1`, `subset_1`, `scatter_1`) live in
#      the offcanvas; open the side panel to see them.
#   2. From the offcanvas, rename `subset_1` to anything (e.g. "renamed").
#   3. Without the fix, the Shiny session crashes with
#      "argument is of length zero". Cause: in the active `Setup` dock,
#      `get_dock_panel(...)` returns NULL for `subset_1`, so `NULL$title` is
#      NULL, and `NULL == "renamed"` evaluates to logical(0) — which
#      `if (...)` rejects.
board <- new_dock_board(
  blocks = c(
    dataset_1 = new_dataset_block("iris"),
    head_1    = new_head_block(),
    subset_1  = new_subset_block(),
    scatter_1 = new_scatter_block(x = "Sepal.Length", y = "Sepal.Width")
  ),
  links = c(
    new_link("dataset_1", "head_1"),
    new_link("head_1", "subset_1"),
    new_link("subset_1", "scatter_1")
  ),
  layout = dock_layouts(
    Setup    = dock_view("dataset_1", active = TRUE),
    Analysis = dock_view("head_1", "subset_1", "scatter_1")
  )
)

serve(board, "repro_rename_offcanvas")
