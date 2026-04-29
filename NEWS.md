# blockr.dock 0.1.1

* Added prepend block action.
* Define multi-view boards with `dock_layouts()`:

  ```r
  layouts = dock_layouts(
    Analysis = list("block_1", "block_2"),
    Overview = list("dag_extension")
  )
  ```

  Mark an arrangement as initially active with `dock_grid(..., active = TRUE)`:

  ```r
  layouts = dock_layouts(
    Analysis = list("block_1", "block_2"),
    Overview = dock_grid("dag_extension", active = TRUE)
  )
  ```

  If none is marked, the first one is used.

  In `new_dock_board()`, the `layouts` argument now defaults to `default_grid(blocks, extensions)`, which is coerced to a single-page `dock_layouts(Page = ...)` so that nothing-is-specified gives you a single-page dashboard with a sensible panel arrangement.

* **Breaking changes** to the layout API:
  - Renamed `new_dock_board()`'s `layout` parameter to `layouts` (and the corresponding board field), since boards now hold a `dock_layouts` collection.
  - Renamed the board accessor `dock_layout(brd)` (and setter `dock_layout(brd) <-`) to `active_layout(brd)` / `active_layout(brd) <-`.
  - Renamed `board_views(brd)` to `board_layouts(brd)`, and the corresponding setter `dock_layouts(brd) <- value` to `board_layouts(brd) <- value`.
  - Introduced a new `dock_grid` type with constructor `dock_grid(..., active = FALSE)` and predicate `is_dock_grid()`. A `dock_grid` is the unresolved arrangement spec (a list of IDs) that goes inside `dock_layouts(...)`; it is panel-resolved later when blocks/extensions are known. `dock_layout` is reserved for fully-resolved (dockview-shaped: `grid + panels`) layouts.
  - Removed `dock_view()`; use `dock_grid(..., active = FALSE)` instead.
  - Renamed `default_layout()` to `default_grid()` to reflect the return type (a `dock_grid`, not a `dock_layout`).
  - Unexported the high-level resolver `create_dock_layout()` (renamed internally to `resolve_dock_layout()`).
  - The previously-exported `default_grid()` (panel-ID form, unrelated to the new `default_grid()`) was already unexported; the name is now reused for the names-form default that `dock_layouts()` consumes.

# blockr.dock 0.1.0

* Initial CRAN submission
