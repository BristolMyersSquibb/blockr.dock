library(blockr.core)
library(blockr.dock)

# A view with a panel on each rail, so that a rail shows on either side at
# once, with the seam where each rail meets the tab groups.
# The left rail holds the panel of core's Edit board plugin, which the script
# names `edit`, and the right rail the panel of block `b`.
serve(
  new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_head_block(),
      c = new_head_block(n = 3L)
    ),
    links = links(
      ab = new_link(from = "a", to = "b", input = "data"),
      ac = new_link(from = "a", to = "c", input = "data")
    ),
    plugins = list(
      new_dock_plugin(
        views = list(
          main = dock_view(
            dock_grid(
              "a",
              "c",
              rail(plg("edit"), position = "left"),
              rail(blk("b"), position = "right")
            )
          )
        )
      ),
      edit = new_edit_board_plugin()
    ),
    id = "my_board"
  )
)
