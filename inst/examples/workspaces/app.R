library(blockr.core)
library(blockr.dag)

blocks <- c(
  dataset_1 = new_dataset_block("iris"),
  head_1 = new_head_block(),
  dataset_2 = new_dataset_block("mtcars"),
  scatter_1 = new_scatter_block("mpg", "hp")
)

links <- list(
  new_link("dataset_1", "head_1"),
  new_link("dataset_2", "scatter_1")
)

serve(
  new_dock_board(
    blocks = blocks,
    links = links,
    extensions = new_dag_extension(),
    workspaces = list(
      Analysis = list(
        children = list(
          Data = list(
            block_ids = c("dataset_1", "head_1")
          ),
          Plots = list(
            block_ids = c("dataset_2", "scatter_1")
          )
        )
      ),
      Summary = list(
        block_ids = c("dataset_1")
      )
    )
  ),
  "my_board"
)
