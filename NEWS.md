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

  Mark an arrangement as initially active with `dock_layout(..., active = TRUE)`:

  ```r
  layouts = list(
    Analysis = list("block_1", "block_2"),
    Overview = dock_layout("dag_extension", active = TRUE)
  )
  ```

  If none is marked, the first one is used. The board's `initialise_layout`
  normalises each slot to a `dock_layout` (storing the arrangement only),
  and stores the result as a `dock_layouts` collection.

* **Breaking changes** to the layout API:
  - Renamed `new_dock_board()`'s `layout` parameter to `layouts` (and the corresponding board field), since boards now hold a `dock_layouts` collection.
  - Renamed the board accessor `dock_layout(brd)` (and setter `dock_layout(brd) <-`) to `active_layout(brd)` / `active_layout(brd) <-`.
  - Renamed `board_views(brd)` to `board_layouts(brd)`, and the corresponding setter `dock_layouts(brd) <- value` to `board_layouts(brd) <- value`.
  - `dock_layout` is now the per-view arrangement type. `dock_layout(...)` constructs one from a nested list of block / extension IDs and accepts `orientation`, `sizes`, and `active` arguments. The previously-exported "fully-resolved" `dock_layout` (grid + panels wire shape) is gone — panel content is derived from the board's blocks and extensions on demand at the dockview boundary, so per-view storage shrinks to just the arrangement and panel definitions no longer duplicate across views.
  - Added `panels(..., active = NULL)` for tabbed leaves with an explicit open tab, and `group(..., sizes = NULL)` for nested branches with explicit ratios. `dock_layout()` itself also accepts `sizes =` for root-level ratios and `orientation =` for the top-level split direction.
  - Removed `dock_view()`, `dock_grid()`, `is_dock_grid()`, and `as_dock_grid()`. Use `dock_layout(...)` (or the new `panels()` / `group()`) for the per-view spec.
  - Renamed `default_layout()` → and kept the name `default_layout()`. It now returns a `dock_layout` (arrangement only) — the previously-exported `default_grid()` (panel-ID form) is gone.
  - Unexported the high-level resolver `create_dock_layout()` (renamed internally to `resolve_dock_layout()`).
  - Unexported the `dock_layouts()` constructor. The user-facing input shape for `new_dock_board(layouts = ...)` is a plain named list — the `dock_layouts` type is the resolved collection that the board holds internally. `is_dock_layouts()`, `as_dock_layouts()`, and `validate_dock_layouts()` remain exported.
  - Unexported `new_dock_layout()`; use `dock_layout()` instead.
  - Unexported `view_ids()` and `view_can_crud()`. Both were internal helpers exposed by accident; renamed to `layout_ids()` and `views_can_crud()` respectively to align with what they operate on.
  - Wire format for serialised `dock_layout` decoupled from dockview's internal tree. The persisted shape is a flattened recursive spec: the top object carries `orientation`, `children`, optional `sizes`, and optional `focus` (the panel with current focus); a child is a bare string (single-panel leaf), a `{panels, active?}` object (tabbed leaf), or a `{children, sizes?}` object (nested branch). Sizes are ratios (auto-normalised from dockview's pixel sizes); even splits omit `sizes`; the default open tab omits `active`; focus on the first leaf omits `focus`. `focus` round-trips the focused group (dockview's `activeGroup`) by naming a stable panel rather than the regenerated group id. Legacy payloads (with the dockview-shape `grid` field) load via a shape-discriminated reader. Producer-version routing is tracked in #153 (depends on blockr.core forwarding `...` in `blockr_deser.list`).
  - Layout conversion API consolidated around `as_dock_layout()` (in) and `layout_to_json()` (out). `as_dock_layout()` now coerces a JSON string or a parsed spec list (in addition to a `dock_layout` and a `board`); `blocks` / `extensions` fold bare-ID resolution and validation into the call. `as.list()` of a `dock_layout` returns the spec list (the inverse of `as_dock_layout(<list>)`). `layout_to_json()` writes the JSON; `layout_panel_ids()` / `panel_obj_ids()` inspect the panel / object IDs a layout references. `layout_from_json()` was folded into `as_dock_layout()` and is gone. The dockview wire format and its converters are not part of the public API — only the `dock_layout` object, our JSON, and the spec list are.

# blockr.dock 0.1.0

* Initial CRAN submission
