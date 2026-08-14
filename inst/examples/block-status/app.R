library(blockr.core)
library(blockr.dock)

# Two blocks in separate groups so both cards render: `a` evaluates and carries
# no status affordance, while `b` has no data input and settles on `waiting` --
# the one status that draws both a status dot and a status note. Tabbed panels
# would leave the background one `dormant`, which draws neither, so the grid
# splits them. The second view holds a third waiting block, whose card is built
# only on first visit -- the deferred case a card's status has to survive.
serve(
  new_dock_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_head_block(),
      c = new_head_block()
    ),
    views = list(
      main = dock_view(c("a", "b"), name = "Main"),
      later = dock_view("c", name = "Later")
    ),
    grids = list(
      main = dock_grid("block_panel-a", "block_panel-b", sizes = c(0.5, 0.5))
    ),
    active = "main"
  ),
  "my_board"
)
