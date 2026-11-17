library(blockr.core)
library(blockr.dock)

# Three views, shown as tabs under the navbar.
# The blockr option `view_tabs` gives the board option "Views as tabs" its
# default, so the line of tabs shows from the start.
options(blockr.view_tabs = TRUE)

serve(
  new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_dataset_block("mtcars"),
      c = new_dataset_block("airquality")
    ),
    plugins = list(
      new_dock_plugin(
        views = list(
          First = dock_view("a"),
          Second = dock_view("b"),
          Third = dock_view("c")
        ),
        active = "First"
      )
    ),
    id = "my_board"
  )
)
