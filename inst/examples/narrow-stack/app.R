library(blockr.core)
library(blockr.dock)

# A board whose grid has a tab group of two panels, so that the narrow layout
# has a panel behind a tab.
# Its split of 0.4 and 0.6, where a default grid gives equal sizes, shows
# whether a gesture writes back the sizes that dockview shows, and whether the
# stored sizes outlast the narrow layout.

# The dock draws the narrow layout only in a window narrower than the blockr
# option `narrow_breakpoint`, so the example sets it.
# A board that sets no breakpoint shows its grid at every width.
options(blockr.narrow_breakpoint = 900)

serve(
  new_board(
    blocks = c(
      a = new_dataset_block(),
      b = new_dataset_block("mtcars"),
      c = new_dataset_block("airquality")
    ),
    plugins = list(
      new_dock_plugin(
        views = list(
          Main = dock_view(
            dock_grid("a", panels("b", "c", active = "b"), sizes = c(0.4, 0.6))
          )
        )
      )
    ),
    id = "my_board"
  )
)
