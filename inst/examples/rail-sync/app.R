library(blockr.core)
library(blockr.dock)

# Three views, each with a block on its left rail, every rail open.
# While the board option `sync_rails` is on, as it is by default, collapsing
# the rail of one view collapses the left rail of every view.
serve(
  new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_dataset_block("mtcars"),
      c = new_dataset_block("iris"),
      d = new_dataset_block("mtcars"),
      e = new_dataset_block("iris"),
      f = new_dataset_block("mtcars")
    ),
    plugins = list(
      new_dock_plugin(
        views = list(
          First = dock_view(dock_grid("a", rail(blk("b")))),
          Second = dock_view(dock_grid("c", rail(blk("d")))),
          Third = dock_view(dock_grid("e", rail(blk("f"))))
        ),
        active = "First"
      )
    ),
    id = "my_board"
  )
)
