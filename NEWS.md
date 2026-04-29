# blockr.dock 0.1.1

* Added prepend block action.
* Define multi-view boards with `dock_layouts()`:

  ```r
  layouts = dock_layouts(
    Analysis = list("block_1", "block_2"),
    Overview = list("dag_extension")
  )
  ```

  Mark a layout as initially active with `dock_layout(..., active = TRUE)`:

  ```r
  layouts = dock_layouts(
    Analysis = list("block_1", "block_2"),
    Overview = dock_layout("dag_extension", active = TRUE)
  )
  ```

  If none is marked, the first one is used.

  In `new_dock_board()`, the `layouts` argument now defaults to `dock_layouts(Page = default_layout(blocks, extensions))`, so it creates a single-page dashboard with a default panel arrangement when nothing is specified by the user.

* **Breaking changes** to the layout API:
  - Renamed `new_dock_board()`'s `layout` parameter to `layouts` (and the corresponding board field), since boards now hold a `dock_layouts` collection.
  - Renamed the board accessor `dock_layout(brd)` (and setter `dock_layout(brd) <-`) to `active_layout(brd)` / `active_layout(brd) <-`. This frees up `dock_layout()` as the inline constructor.
  - Renamed `board_views(brd)` to `board_layouts(brd)`, and the corresponding setter `dock_layouts(brd) <- value` to `board_layouts(brd) <- value`.
  - Removed `dock_view()`; use `dock_layout(..., active = FALSE)` instead — same semantics, and the name fits the vocabulary (a view's content is a layout).
  - Unexported the high-level resolver `create_dock_layout()` (renamed internally to `resolve_dock_layout()`); the public surface is now `dock_layout()` (inline spec), `new_dock_layout()` (low-level constructor), and `default_layout()` (default arrangement).
  - The previously-exported `default_grid()` (panel-ID form) is no longer exported; use `default_layout()` for the names-form default that `dock_layouts()` consumes.

# blockr.dock 0.1.0

* Initial CRAN submission
