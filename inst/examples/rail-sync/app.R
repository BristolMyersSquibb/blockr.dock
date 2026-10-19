library(blockr.core)
library(blockr.dock)

# Three pages, each with a block on its left rail, all stored expanded. With
# the rails kept in step (the default), collapsing one collapses the others,
# whether their docks are built yet or not -- a test can watch both happen.
serve(
  new_dock_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_dataset_block("mtcars"),
      c = new_dataset_block("iris"),
      d = new_dataset_block("mtcars"),
      e = new_dataset_block("iris"),
      f = new_dataset_block("mtcars")
    ),
    views = list(
      First = c("a", "b"),
      Second = c("c", "d"),
      Third = c("e", "f")
    ),
    grids = list(
      First = dock_grid("a", rail(blk("b"))),
      Second = dock_grid("c", rail(blk("d"))),
      Third = dock_grid("e", rail(blk("f")))
    ),
    active = "First"
  ),
  "my_board"
)
