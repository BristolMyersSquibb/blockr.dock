# blockr.dock 0.1.1

* Added prepend block action.
* Define multi-view boards with `dock_layouts()` or a plain named list:

  ```r
  layout = list(
    Analysis = list("block_1", "block_2"),
    Overview = list("dag_extension")
  )
  ```

  Set the initially active view with `active`:
  `dock_layouts(..., active = "Overview")`.
  Previously saved boards are restored automatically. Single view boards
  still work.

# blockr.dock 0.1.0

* Initial CRAN submission
