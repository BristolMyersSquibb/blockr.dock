library(blockr.core)
library(blockr.dock)

board <- new_dock_board(
  extensions = blockr.dag::new_dag_extension(),
  blocks = c(
    dataset_1 = new_dataset_block(),
    head_1 = new_head_block()
  ),
  links = new_link("dataset_1", "head_1"),
  views = list(
    Analysis = c("dag_extension", "dataset_1", "head_1"),
    Overview = "dag_extension",
    Empty = list()
  ),
  grids = list(
    Analysis = dock_grid(
      "dag_extension",
      panels("dataset_1", "head_1", active = "head_1"),
      sizes = c(0.3, 0.7)
    )
  ),
  active = "Overview"
)

serve(board, "my_board")
