library(blockr.core)
library(blockr.dock)

# The DAG extension is given an explicit id ("analysis") via its list name;
# left unnamed it would default to "dag" (its class, stripped of the
# `_extension` suffix). Panels are named with the `blk()` / `ext()` typed
# references throughout, so a view or grid reads as blocks-and-extensions
# rather than wire-prefixed panel ids.
board <- new_dock_board(
  extensions = list(analysis = blockr.dag::new_dag_extension()),
  blocks = c(
    dataset_1 = new_dataset_block(),
    head_1 = new_head_block()
  ),
  links = new_link("dataset_1", "head_1"),
  views = list(
    Analysis = list(ext("analysis"), blk("dataset_1"), blk("head_1")),
    Overview = ext("analysis"),
    Empty = list()
  ),
  grids = list(
    Analysis = dock_grid(
      ext("analysis"),
      panels(blk("dataset_1"), blk("head_1"), active = blk("head_1")),
      sizes = c(0.3, 0.7)
    )
  ),
  active = "Overview"
)

serve(board, "my_board")
