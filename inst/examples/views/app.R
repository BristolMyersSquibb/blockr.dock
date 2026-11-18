library(blockr.core)
library(blockr.dock)

# The script names core's Edit board plugin `analysis` in the board's
# `plugins`, and its views place the plugin's panel by that name with plg().
# Each view takes its grid, or the panels that its default grid places, and
# the view `Empty` shows no panel.
# The board opens on the view `Overview`.
board <- new_board(
  blocks = c(
    dataset_1 = new_dataset_block(),
    head_1 = new_head_block()
  ),
  links = new_link("dataset_1", "head_1"),
  plugins = list(
    new_dock_plugin(
      views = list(
        Analysis = dock_view(
          dock_grid(
            plg("analysis"),
            panels(blk("dataset_1"), blk("head_1"), active = blk("head_1")),
            sizes = c(0.3, 0.7)
          )
        ),
        Overview = dock_view(plg("analysis")),
        Empty = dock_view(list())
      ),
      active = "Overview"
    ),
    analysis = new_edit_board_plugin()
  ),
  id = "my_board"
)

serve(board)
