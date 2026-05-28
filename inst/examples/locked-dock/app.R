library(blockr.dock)
library(blockr.core)

options(blockr.dock_is_locked = TRUE)

serve(
  new_dock_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_head_block(n = 20L),
      c = new_scatter_block(x = "Sepal.Length", y = "Sepal.Width"),
      d = new_subset_block()
    ),
    links = c(
      new_link("a", "b", input = "data"),
      new_link("b", "c", input = "data"),
      new_link("b", "d", input = "data")
    ),
    layouts = list(list("a", "b"), list("c", "d"))
  )
)
