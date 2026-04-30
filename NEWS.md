# blockr.dock 0.1.1

* Added prepend block action.
* Define multi-view boards by passing a named list to
  `new_dock_board(layouts = ...)`:

  ```r
  layouts = list(
    Analysis = list("block_1", "block_2"),
    Overview = list("dag_extension")
  )
  ```

  Mark an arrangement as initially active with `dock_grid(..., active = TRUE)`:

  ```r
  layouts = list(
    Analysis = list("block_1", "block_2"),
    Overview = dock_grid("dag_extension", active = TRUE)
  )
  ```

  If none is marked, the first one is used. The board's `initialise_layout`
  resolves each slot's grid spec into a `dock_layout` using the board's
  blocks and extensions, and stores the result as a `dock_layouts`
  collection.

* **Breaking changes** to the layout API:
  - Renamed `new_dock_board()`'s `layout` parameter to `layouts` (and the corresponding board field), since boards now hold a `dock_layouts` collection.
  - Renamed the board accessor `dock_layout(brd)` (and setter `dock_layout(brd) <-`) to `active_layout(brd)` / `active_layout(brd) <-`.
  - Renamed `board_views(brd)` to `board_layouts(brd)`, and the corresponding setter `dock_layouts(brd) <- value` to `board_layouts(brd) <- value`.
  - Introduced a new `dock_grid` type with constructor `dock_grid(..., active = FALSE)` and predicate `is_dock_grid()`. A `dock_grid` is the unresolved arrangement spec (a list of IDs); it is panel-resolved later when blocks/extensions are known. `dock_layout` is reserved for fully-resolved (dockview-shaped: `grid + panels`) layouts.
  - Removed `dock_view()`; use `dock_grid(..., active = FALSE)` instead.
  - Renamed `default_layout()` to `default_grid()` to reflect the return type (a `dock_grid`, not a `dock_layout`).
  - Unexported the high-level resolver `create_dock_layout()` (renamed internally to `resolve_dock_layout()`).
  - Unexported `dock_layouts()` and `validate_dock_layouts()`. The user-facing input shape for `new_dock_board(layouts = ...)` is now a plain named list — the `dock_layouts` type is the resolved collection that the board holds internally. `is_dock_layouts()` (predicate) and `as_dock_layouts()` (with `dock_layouts` and `dock_layout` methods) remain exported. The `as_dock_layouts.list` method was dropped.
  - **Silent semantic change to `default_grid()`**: in 0.1.0, the exported `default_grid(blocks, extensions)` returned a list of *panel IDs* (`block_panel-a`, `ext_panel-foo`); in 0.1.1 the same call returns a list of plain *block/extension names* (`a`, `foo`), wrapped in a `dock_grid`. Existing call sites compile but produce different values. Audit any direct uses of `default_grid()` and switch to the panel-ID accessors (`as_block_panel_id()`, `as_ext_panel_id()`) if you depended on the old shape.

# blockr.dock 0.1.0

* Initial CRAN submission
