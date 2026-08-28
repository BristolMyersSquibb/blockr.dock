library(blockr.core)
library(blockr.dock)

# A board whose authored grid carries a tabbed group, so collapsing it to a
# stack leaves a background tab. Every background tab's render overlay is
# parked below the grid by dockView, a full container tall, so this is the
# board that shows whether the stacked container still clips them -- a
# single-panel-per-group board never grows an overlay and would pass either
# way. The authored 40/60 split also keeps the geometry write-back assertions
# meaningful, as a plain default grid would not.
serve(
  new_dock_board(
    blocks = c(
      a = new_dataset_block(),
      b = new_dataset_block("mtcars"),
      c = new_dataset_block("airquality")
    ),
    grids = list(
      Main = dock_grid("a", panels("b", "c", active = "b"), sizes = c(0.4, 0.6))
    )
  ),
  "my_board"
)
