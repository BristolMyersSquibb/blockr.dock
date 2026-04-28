library(blockr.core)
library(blockr.dock)

board <- new_dock_board(
  extensions = blockr.dag::new_dag_extension(),
  blocks = c(
    dataset_1 = new_dataset_block(),
    head_1 = new_head_block()
  ),
  links = new_link("dataset_1", "head_1"),
  layout = list(
    Analysis = list("dataset_1", "head_1", "dag_extension"),
    Overview = dock_view("dag_extension", active = TRUE),
    Empty = list()
  )
)

serve(board, "my_board")
