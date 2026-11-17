library(blockr.dock)
library(blockr.core)

# One view, which the script leaves without an id, so that R draws one and
# the view is named after it.
# The view takes its panels in place of a grid, so it shows them in the
# default grid: the panel of core's Edit board plugin in one tab group,
# beside the blocks' panels in another.
serve(
  new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_scatter_block(x = "Sepal.Length", y = "Sepal.Width")
    ),
    links = list(from = "a", to = "b", input = "data"),
    plugins = list(
      new_dock_plugin(views = list(dock_view(list(plg("edit"), "a", "b")))),
      edit = new_edit_board_plugin()
    ),
    id = "my_board"
  )
)
