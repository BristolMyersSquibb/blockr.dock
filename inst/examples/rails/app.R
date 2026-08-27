library(blockr.core)
library(blockr.dock)

# Every board offers both edges; populating one is what makes it visible. This
# fixture fills each, so a test can see a rail on either side at once, and the
# seam where a vertically-tabbed strip meets its content.
serve(
  new_dock_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_head_block(),
      c = new_head_block(n = 3L)
    ),
    links = links(
      ab = new_link(from = "a", to = "b", input = "data"),
      ac = new_link(from = "a", to = "c", input = "data")
    ),
    extensions = new_edit_board_extension(),
    views = list(main = c("a", "b", "c", "edit_board")),
    grids = list(
      main = dock_grid(
        "a",
        rail(ext("edit_board"), position = "left"),
        rail(blk("b"), position = "right")
      )
    )
  ),
  "my_board"
)
