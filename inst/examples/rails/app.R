library(blockr.core)
library(blockr.dock)

# Three views, each with a panel on its left rail, every rail open.
# The first view also has a panel on its right rail, so that a rail shows on
# either side at once, with the seam where each rail meets the tab groups.
# Its left rail holds the panel of core's Edit board plugin, which the script
# names `edit`, and its right rail the panel of block `b`.
# While the board option `sync_rails` is on, as it is by default, collapsing
# the left rail of one view collapses the left rail of every view.
serve(
  new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_head_block(),
      c = new_head_block(n = 3L),
      d = new_dataset_block("mtcars"),
      e = new_dataset_block("iris"),
      f = new_dataset_block("airquality"),
      g = new_dataset_block("mtcars")
    ),
    links = links(
      ab = new_link(from = "a", to = "b", input = "data"),
      ac = new_link(from = "a", to = "c", input = "data")
    ),
    plugins = list(
      new_dock_plugin(
        views = list(
          First = dock_view(
            dock_grid(
              "a",
              "c",
              rail(plg("edit"), position = "left"),
              rail(blk("b"), position = "right")
            )
          ),
          Second = dock_view(
            dock_grid("d", rail(blk("e"), position = "left"))
          ),
          Third = dock_view(
            dock_grid("f", rail(blk("g"), position = "left"))
          )
        ),
        active = "First"
      ),
      edit = new_edit_board_plugin()
    ),
    id = "my_board"
  )
)
