library(blockr.dock)
library(blockr.core)

# The blockr option `locked` turns on the board lock.
# Under it, the dock offers no view or panel to add, rename, move or close,
# keeps every panel's size and place, and the board lock fixes the state of
# each block.
options(blockr.locked = TRUE)

serve(
  new_board(
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
    plugins = list(
      new_dock_plugin(views = list(dock_view(c("a", "b", "c", "d"))))
    ),
    id = "my_board"
  )
)
