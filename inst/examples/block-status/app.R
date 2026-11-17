library(blockr.core)
library(blockr.dock)

# Two blocks in tab groups of their own, so that both cards show.
# Block `a` evaluates, and block `b` has no data input, so it waits.
# A waiting block's card shows its status dot and a note that says what the
# block lacks.
# The second view holds a third waiting block, whose card the dock draws only
# once the user switches to the view.
serve(
  new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_head_block(),
      c = new_head_block()
    ),
    plugins = list(
      new_dock_plugin(
        views = list(
          main = dock_view(
            dock_grid("a", "b", sizes = c(0.5, 0.5)),
            name = "Main"
          ),
          later = dock_view("c", name = "Later")
        ),
        active = "main"
      )
    ),
    id = "my_board"
  )
)
