# blockr.dock (development version)

* `dock_layout` objects gain `format()` / `print()` methods that render
  the arrangement as an indented tree: orientation, nested groups with
  their sizes, tabbed leaves with the active tab, and the focused panel.
  Panel IDs print without their `block_panel-` / `ext_panel-` prefixes
  by default; pass `bare = FALSE` for the canonical IDs (#161).

* The add / append / prepend block action handlers now mount the
  `blockr.ui` block-browser module: a card-list block picker with
  search, per-category icons, and a per-card expand for tweaking the
  id / title / link / port before adding. Repeated single clicks on a
  card produce distinct blocks (suggested ids are seeded against the
  board so they never collide), replacing the old single-select
  selectize form. The per-field Shiny inputs (`<mode>_block_selection`
  / `<mode>_block_id` / `<mode>_block_name` / `<mode>_link_id` /
  `<mode>_block_input` / `<mode>_block_confirm`) are gone in favour of
  a single committed-block reactive returned by
  `blockr.ui::block_browser_server()`. `block_sidebar_body()` is
  removed (nothing in-tree calls it; link / stack flows use
  `link_sidebar_body()` / `stack_sidebar_body()` unchanged). Requires
  `blockr.ui` with the block-browser module.

# blockr.dock 0.1.2

* The `views` slot in the `board_update` payload is now a structured
  delta (`add` / `mod` / `rm` / `active`) instead of a wholesale
  `dock_layouts` replacement. Mentioned views are touched, omitted
  views keep their current state, and the four sub-slots compose
  atomically with `blocks` / `links` / `stacks` in the same lifecycle
  tick. See `?dock_board_update_lifecycle` for the contract (#150).

* Removing a block no longer clears the active view's layout. Instead,
  every view containing the removed block has the block's panel
  dropped surgically from its layout, preserving the rest of the grid.
  `augment_board_update.dock_board()` performs the cleanup for the
  `update()` lifecycle path; `rm_blocks.dock_board()` does the same
  surgically (rather than nuking the active layout) for direct
  callers like `clear_board()` (#150).

* `board_layouts(rv$board)` now stays in sync with UI-driven layout
  changes (panel close/add, drag-resize/rearrange, view CRUD).
  UI-driven mutations are routed through `update(list(views = ...))`
  and applied via `validate_board_update.dock_board()` and
  `apply_board_update.dock_board()`. Writes are debounced (250 ms) so
  drag-resize doesn't thrash. Requires `blockr.core (>= 0.1.3)` for
  the update lifecycle generics.

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
  - Layout conversion API split by boundary. The R object ↔ R list boundary uses coercion: `as_dock_layout()` coerces a `dock_layout` (identity), a `board` (its active layout), or a spec list to a `dock_layout`; `as.list()` of a `dock_layout` returns that spec list. The R object ↔ JSON string boundary uses explicit verbs: `layout_to_json()` / `layout_from_json()`. Both `as_dock_layout(<list>)` and `layout_from_json()` take optional `blocks` / `extensions` to resolve bare IDs and validate. `layout_panel_ids()` / `panel_obj_ids()` inspect the panel / object IDs a layout references. The dockview wire format and its converters are not part of the public API — only the `dock_layout` object, our JSON, and the spec list are; `as_dock_layout()` rejects a dockview grid-shaped list.

# blockr.dock 0.1.0

* Initial CRAN submission
