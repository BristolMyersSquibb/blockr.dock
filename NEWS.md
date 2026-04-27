# blockr.dock 0.1.1

* Added prepend block action.
* Define multi-view boards with `dock_layouts()` or a plain named list:

  ```r
  layout = list(
    Analysis = list("block_1", "block_2"),
    Overview = list("dag_extension")
  )
  ```

  Mark a view as initially active by tagging its spec via `dock_view()`:

  ```r
  layout = list(
    Analysis = list("block_1", "block_2"),
    Overview = dock_view("dag_extension", active = TRUE)
  )
  ```

  If no view is tagged, the first one is used. Single-view boards still
  work.

# blockr.dock 0.1.0

* Initial CRAN submission
