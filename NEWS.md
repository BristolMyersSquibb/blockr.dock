# blockr.dock 0.1.2

* Panel operations are now first-class verbs in the `views$mod` update payload
  (`add` / `rm` / `move` / `select`), with panels named by the new typed
  references `blk()` / `ext()` instead of wire-id prefixes. **Breaking:** the
  old set-replace membership form is retired.
* **Breaking:** an extension's id is now owned by its container (mirroring
  blocks), serving as its single identity everywhere -- the wire panel id, DOM
  handle, module namespace and `ext()` target.
* **Breaking (extension authors):** an extension's live result now reaches
  actions, callbacks and peer extensions as an `extensions` bundle keyed by
  extension id; the new exported `extension_ids()` resolves a class to the
  runtime id(s) the container assigned.
* A dock extension opens a block's panel through the `views` grammar (compose
  `active` + `select`) rather than the retired live `dock` handle; the exported
  `show_panel()` is removed.
* The block status badge is now one exported helper, `block_status_badge()`,
  shared with blockr.dag so the dock card icon and the DAG node badge render
  identically; the dock dot now also reflects render-phase errors.
* Renaming a block no longer crashes a board where that block is absent from
  some view -- placed in only one view, or parked in the offcanvas with no
  panel.
* A board's per-view layout splits into two independent slots: a
  server-authoritative `dock_views` structure collection (read with
  `board_views()`) and a separate `NULL`-valid `dock_grids` geometry slot
  (`board_grids()`), each grid validated as a subset of its view's membership.
* Block eval status is now a first-class panel affordance: a `waiting` or
  `unset` block shows a dedicated placeholder instead of a generic warning, and
  a colour-coded dot in the card header marks `waiting`, `unset` and `failed`
  blocks at a glance.
* Per-block condition UI (warnings, messages, errors) is updated surgically by
  the stable condition id, so a persistent warning no longer flashes on every
  re-evaluation.
* The busy pulse no longer flashes on a plain panel switch: a CSS rule gates it
  on a genuinely recomputing output, so startup and block evaluation still
  pulse but bare navigation does not.
* The "Edit board" extension no longer churns on a board re-emit -- it re-syncs
  its staged working copy only when links or stacks actually change, stops
  flickering the manage-links cell inputs, and overlays half-finished staged
  edits instead of clobbering them.
* Dock extensions now receive `view_data`, the live all-views layout reactive
  that serialization also reads (`NULL` until every view has reported once, so
  `req()` it); the active-view `dock` handle is retired from the extension
  surface.
* Multi-view boards no longer emit a burst of redundant board updates at
  startup; the fold that mirrored every dockview focus tick back into
  `board_layouts` is removed, and the live layout is read on demand instead.
* The dock no longer loops or tears its panels down on a slow client: a view's
  arrangement is now client-owned and flows dock -> board only, removing the
  reconcile push whose echo could restore an impoverished layout.
* Live panel rearrangements are no longer lost on save -- `view_data()`, the
  live layout serialization reads, no longer stays stuck at `NULL` for the
  whole session.
* Adding a block before the dock view has finished initialising no longer
  throws `argument is of length zero`.
* The block, link and stack action handlers now mount the corresponding
  `blockr.ui` menu modules (card-list pickers with search and inline editing),
  replacing every per-field Shiny input; the link menu adds bidirectional
  source / target picking and the stack menu an inline colour picker. The block
  browsers are pre-rendered and toggled rather than rebuilt on each open, and
  the dock-side `*_sidebar_body()` bodies and spec helpers are removed.
* Layout deserialization now routes on the producing blockr.dock version (read
  off `constructor$version`) rather than sniffing the payload shape, keeping
  shape discrimination only as the fallback for version-less payloads.
* The dock "manager" object is gone: `apply_board_update.dock_board()` is a
  pure reducer and all live view surgery runs in one reconcile pass driven by
  the committed board. `augment_board_update.dock_board()` is now idempotent,
  fixing a view-add loop.
* Views now carry a stable, immutable id decoupled from their editable display
  name (mirroring blocks): `dock_layouts` is keyed by id, the name is read and
  written via `view_name()` / `view_name<-()`, and the `views` delta gains a
  `rename` slot. In `new_dock_board(layouts = list(...))` the list name is the
  view's id, so producers that addressed views by name (e.g. blockr.assistant)
  must switch to ids.
* A `dock_layout` now has `format()` / `print()` methods that render its
  arrangement as an indented tree; panel ids print without their wire prefixes
  unless `bare = FALSE`.
* The `views` slot of the `board_update` payload is a structured delta (`add` /
  `mod` / `rm` / `active`) instead of a wholesale `dock_layouts` replacement,
  composing atomically with `blocks` / `links` / `stacks`. UI-driven layout
  changes (panel close / add, drag-resize, view CRUD) route through this
  lifecycle and are debounced (250 ms), and removing a block drops its panel
  surgically rather than clearing the active view. Requires
  `blockr.core (>= 0.1.3)`.

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
