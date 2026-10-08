library(blockr.core)
library(blockr.dock)

# Three views shown as tabs under the navbar. With the `view_tabs` blockr
# option set, the board's "Views as tabs" option starts on, as on a board saved
# with it, so the tab line is there from the first paint.
options(blockr.view_tabs = TRUE)

serve(
  new_dock_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_dataset_block("mtcars"),
      c = new_dataset_block("airquality")
    ),
    views = list(First = "a", Second = "b", Third = "c"),
    active = "First"
  ),
  "my_board"
)
