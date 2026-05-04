# blockr.dock (development version)

## Internal changes

* Label observers for OTEL support.

# blockr.dock 0.1.1

* Added prepend block action.
* Define multi-view boards with `dock_layouts()`:

  ```r
  layout = dock_layouts(
    Analysis = list("block_1", "block_2"),
    Overview = list("dag_extension")
  )
  ```

  Mark a view as initially active by tagging its spec via `dock_view()`:

  ```r
  layout = dock_layouts(
    Analysis = list("block_1", "block_2"),
    Overview = dock_view("dag_extension", active = TRUE)
  )
  ```

  If no view is tagged, the first one is used.

  In `new_dock_board`, the layout now defaults to `dock_layouts(Page = default_view_grid(blocks, extensions))`, so it create a single page dashboard with a default grid when nothing is specified by the user.

# blockr.dock 0.1.0

* Initial CRAN submission
