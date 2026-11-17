library(blockr.core)
library(blockr.dock)

# A board that holds every part of the dock's state, for saving and opening
# it again with the Export and Import of core's storage plugin.
# It has two views with grids of their own: one of three panels, with its
# sizes, a tab group that shows its second panel and the panel of core's Edit
# board plugin, and one of a single panel.
# The board opens on the first of them, which the views menu lists second.
# It holds blocks, a link and a stack as well.
serve(
  new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_head_block(),
      c = new_head_block()
    ),
    links = links(ab = new_link("a", "b", "data")),
    stacks = stacks(grp = c("a", "b")),
    plugins = list(
      new_dock_plugin(
        views = list(
          overview = dock_view("c", name = "Overview"),
          analysis = dock_view(
            dock_grid(
              plg("edit"),
              panels("a", "b", active = "b"),
              sizes = c(0.3, 0.7)
            ),
            name = "Analysis"
          )
        ),
        active = "analysis"
      ),
      edit = new_edit_board_plugin()
    ),
    id = "my_board"
  )
)
