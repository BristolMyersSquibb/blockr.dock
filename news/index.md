# Changelog

## blockr.dock (development version)

- Live panel rearrangements are no longer lost when a board is saved
  ([\#243](https://github.com/BristolMyersSquibb/blockr.dock/issues/243)).
  `view_data()`, the live dock layout that serialization reads, was
  stuck at `NULL` for the whole session, so Export fell back to the
  board’s default layout. The live dock registry is now a
  `reactiveValues` rather than a plain environment, so `view_data()`
  takes a dependency on each view’s entry and re-evaluates when
  reconcile creates it – whatever the init flush order – instead of
  relying on a reconcile observer priority to win that race.

- The add and append block browsers are each pre-rendered once into a
  dedicated sidebar (`add_block_sidebar` / `append_block_sidebar`) and
  merely toggled open, instead of being rebuilt on every open (the
  append rebuild was ~500 ms with a large registry, dominated by
  instantiating every block to compute the linkable filter). The add /
  append / prepend handlers are thin adapters over
  [`blockr.ui::block_browser_server()`](https://rdrr.io/pkg/blockr.ui/man/block-browser.html),
  which now returns ready-to-apply `blocks` / `links` objects (target
  port resolved menu-side); the dock-side `build_block_from_spec()`,
  `valid_block_id()` and `valid_link_id()` helpers are removed. Requires
  the matching blockr.ui (`block_browser_server()` ready-objects
  contract).

- Adding a block before the dock view has finished initialising no
  longer throws `argument is of length zero`. While the dock is
  uninitialised its layout is `NULL`; `determine_active_views()` now
  treats that as an empty dock, so the block’s panel is placed freely
  instead of being stranded in the offcanvas.

- `add_link_action()` now mounts the `blockr.ui` link-menu module and is
  bidirectional: right-clicking a downstream block now lets you pick an
  upstream source, not just a target. The handler passes the board and
  anchor as reactives, so the menu owns link-id validation and keeps a
  pinned menu in sync with the board itself - removing a link frees a
  target whose card reappears live, and removing a block drops its card,
  both without a re-render. The per-field link inputs (`create_link` /
  `add_link_input` / `add_link_id` / `add_link_confirm`) and the
  dock-side `valid_link_id` validator are gone in favour of a single
  committed-spec reactive from
  [`blockr.ui::link_menu_server()`](https://rdrr.io/pkg/blockr.ui/man/link-menu.html).
  `link_sidebar_body()` is removed (no in-tree callers remain;
  out-of-tree consumers migrate to
  [`blockr.ui::link_menu_ui()`](https://rdrr.io/pkg/blockr.ui/man/link-menu.html)
  / `link_menu_server()`).

- The add / edit stack action handlers now mount the `blockr.ui`
  stack-menu module: a multi-select card-list block picker with search,
  per-category icons, an inline hue / lightness colour picker, and a
  panel-level form for the stack name / colour / id. The per-field Shiny
  inputs (`stack_id` / `stack_name` / `stack_color` /
  `stack_block_selection` / `stack_confirm` and the `edit_stack_*`
  equivalents) are gone in favour of a single committed-stack reactive
  returned by
  [`blockr.ui::stack_menu_server()`](https://rdrr.io/pkg/blockr.ui/man/stack-menu.html).
  `stack_sidebar_body()` is removed (no in-tree callers remain;
  out-of-tree consumers migrate to
  [`blockr.ui::stack_menu_ui()`](https://rdrr.io/pkg/blockr.ui/man/stack-menu.html)).
  Drops the
  [`shinyWidgets::colorPickr`](https://dreamrs.github.io/shinyWidgets/reference/colorPickr.html)
  floating popover; the new colour picker renders inline in the sidebar.
  The handlers now pass the board as a reactive, so a pinned stack menu
  stays in sync with board changes (removing a block drops its card
  live); spec validation moved into `blockr.ui` (the dock-side
  `valid_stack_*` validators are gone). The menu builds `dock_stack`
  objects itself via
  [`blockr.dock::new_dock_stack()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/stack.md)
  (gated behind `pkg_avail("blockr.dock")`, a Suggests back-edge), so
  the handlers apply the committed `stacks` object as-is.

- Layout deserialization now routes on the producing blockr.dock version
  rather than sniffing the payload shape. `blockr_deser.dock_board()`
  reads the producer version off the saved `constructor$version` and
  threads it down (through `blockr.core`’s `...`-forwarding
  `blockr_deser.list()`) to `blockr_deser.dock_layout()`, which picks
  the wire-format reader from a version-keyed registry. Shape
  discrimination (legacy dockview `grid` vs. flattened spec) stays as
  the fallback for version-less payloads — very old saves or
  hand-crafted JSON
  ([\#153](https://github.com/BristolMyersSquibb/blockr.dock/issues/153)).

- The dock “manager” object is gone. `apply_board_update.dock_board()`
  is now a pure reducer over
  [`board_layouts()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md);
  all live view surgery (instantiate / tear down / restore / rename /
  switch) runs in a single closure-resident reconcile pass driven by the
  committed board, replacing the duplicated delta-driven and UI-driven
  view CRUD. View init is just the empty-registry case of that pass
  (create every view, show the active one), so there is no separate init
  path. The per-session dock state is ordinary closure-private state
  passed explicitly, not a handle threaded back from the board callback
  through `dot_args`. Also makes `augment_board_update.dock_board()`
  idempotent — a view id is minted once rather than re-minted on every
  augment pass — fixing a view-add loop
  ([\#164](https://github.com/BristolMyersSquibb/blockr.dock/issues/164)).

- Views now carry a stable, immutable **id** decoupled from their
  editable display **name**, mirroring the id / name split used for
  blocks. `dock_layouts` (and the runtime `dock_mgr$docks` registry) are
  keyed by id; the name is an attribute read / written via
  [`view_name()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
  / `view_name<-()` (with
  [`view_names()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
  for a whole collection), and
  [`active_view()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
  now returns the active view’s id. Renaming a view is a pure
  name-attribute write — the id, dock module and DOM element are
  untouched, so no structure is ever re-keyed (and the live-sync rename
  no longer leaks as remove-then-add). The dock module / DOM ids derive
  deterministically from the view id (no random per-render minting), and
  the `views` delta gains a `rename` slot. Naming constraints relax to
  display concerns (non-empty, unique label). Serialization round-trips
  ids. In `new_dock_board(layouts = list(...))` the **list name is the
  view’s id** (the container’s key, like a block id — minted when
  absent); the display name is set on the view via
  `dock_layout(name = )` and falls back to a label derived from the id
  when unset. The `views` delta addresses existing views by **id** (the
  only stable handle) — `mod` / `rm` / `active` carry ids; `add`
  supplies a display name and mints the id. Producers that addressed
  views by name (e.g. `blockr.assistant`) must switch to ids
  ([\#166](https://github.com/BristolMyersSquibb/blockr.dock/issues/166)).

- `dock_layout` objects gain
  [`format()`](https://rdrr.io/r/base/format.html) /
  [`print()`](https://rdrr.io/r/base/print.html) methods that render the
  arrangement as an indented tree: orientation, nested groups with their
  sizes, tabbed leaves with the active tab, and the focused panel. Panel
  IDs print without their `block_panel-` / `ext_panel-` prefixes by
  default; pass `bare = FALSE` for the canonical IDs
  ([\#161](https://github.com/BristolMyersSquibb/blockr.dock/issues/161)).

- The add / append / prepend block action handlers now mount the
  `blockr.ui` block-browser module: a card-list block picker with
  search, per-category icons, and a per-card expand for tweaking the id
  / title / link / port before adding. Repeated single clicks on a card
  produce distinct blocks (suggested ids are seeded against the board so
  they never collide), replacing the old single-select selectize form.
  The per-field Shiny inputs (`<mode>_block_selection` /
  `<mode>_block_id` / `<mode>_block_name` / `<mode>_link_id` /
  `<mode>_block_input` / `<mode>_block_confirm`) are gone in favour of a
  single committed-block reactive returned by
  [`blockr.ui::block_browser_server()`](https://rdrr.io/pkg/blockr.ui/man/block-browser.html).
  `block_sidebar_body()` is removed (nothing in-tree calls it; link /
  stack flows use `link_sidebar_body()` / `stack_sidebar_body()`
  unchanged). Requires `blockr.ui` with the block-browser module.

## blockr.dock 0.1.2

- The `views` slot in the `board_update` payload is now a structured
  delta (`add` / `mod` / `rm` / `active`) instead of a wholesale
  `dock_layouts` replacement. Mentioned views are touched, omitted views
  keep their current state, and the four sub-slots compose atomically
  with `blocks` / `links` / `stacks` in the same lifecycle tick. See
  `?dock_board_update_lifecycle` for the contract
  ([\#150](https://github.com/BristolMyersSquibb/blockr.dock/issues/150)).

- Removing a block no longer clears the active view’s layout. Instead,
  every view containing the removed block has the block’s panel dropped
  surgically from its layout, preserving the rest of the grid.
  `augment_board_update.dock_board()` performs the cleanup for the
  [`update()`](https://rdrr.io/r/stats/update.html) lifecycle path;
  `rm_blocks.dock_board()` does the same surgically (rather than nuking
  the active layout) for direct callers like
  [`clear_board()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_blocks.html)
  ([\#150](https://github.com/BristolMyersSquibb/blockr.dock/issues/150)).

- `board_layouts(rv$board)` now stays in sync with UI-driven layout
  changes (panel close/add, drag-resize/rearrange, view CRUD). UI-driven
  mutations are routed through `update(list(views = ...))` and applied
  via `validate_board_update.dock_board()` and
  `apply_board_update.dock_board()`. Writes are debounced (250 ms) so
  drag-resize doesn’t thrash. Requires `blockr.core (>= 0.1.3)` for the
  update lifecycle generics.

## blockr.dock 0.1.1

CRAN release: 2026-04-29

- Added prepend block action.

- Define multi-view boards by passing a named list to
  `new_dock_board(layouts = ...)`:

  ``` r

  layouts = list(
    Analysis = list("block_1", "block_2"),
    Overview = list("dag_extension")
  )
  ```

  Mark an arrangement as initially active with
  `dock_layout(..., active = TRUE)`:

  ``` r

  layouts = list(
    Analysis = list("block_1", "block_2"),
    Overview = dock_layout("dag_extension", active = TRUE)
  )
  ```

  If none is marked, the first one is used. The board’s
  `initialise_layout` normalises each slot to a `dock_layout` (storing
  the arrangement only), and stores the result as a `dock_layouts`
  collection.

- **Breaking changes** to the layout API:

  - Renamed
    [`new_dock_board()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock.md)’s
    `layout` parameter to `layouts` (and the corresponding board field),
    since boards now hold a `dock_layouts` collection.
  - Renamed the board accessor `dock_layout(brd)` (and setter
    `dock_layout(brd) <-`) to `active_layout(brd)` /
    `active_layout(brd) <-`.
  - Renamed `board_views(brd)` to `board_layouts(brd)`, and the
    corresponding setter `dock_layouts(brd) <- value` to
    `board_layouts(brd) <- value`.
  - `dock_layout` is now the per-view arrangement type.
    `dock_layout(...)` constructs one from a nested list of block /
    extension IDs and accepts `orientation`, `sizes`, and `active`
    arguments. The previously-exported “fully-resolved” `dock_layout`
    (grid + panels wire shape) is gone — panel content is derived from
    the board’s blocks and extensions on demand at the dockview
    boundary, so per-view storage shrinks to just the arrangement and
    panel definitions no longer duplicate across views.
  - Added `panels(..., active = NULL)` for tabbed leaves with an
    explicit open tab, and `group(..., sizes = NULL)` for nested
    branches with explicit ratios.
    [`dock_layout()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout.md)
    itself also accepts `sizes =` for root-level ratios and
    `orientation =` for the top-level split direction.
  - Removed `dock_view()`, `dock_grid()`, `is_dock_grid()`, and
    `as_dock_grid()`. Use `dock_layout(...)` (or the new
    [`panels()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout.md)
    /
    [`group()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout.md))
    for the per-view spec.
  - Renamed
    [`default_layout()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout.md)
    → and kept the name
    [`default_layout()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout.md).
    It now returns a `dock_layout` (arrangement only) — the
    previously-exported `default_grid()` (panel-ID form) is gone.
  - Unexported the high-level resolver `create_dock_layout()` (renamed
    internally to `resolve_dock_layout()`).
  - Unexported the `dock_layouts()` constructor. The user-facing input
    shape for `new_dock_board(layouts = ...)` is a plain named list —
    the `dock_layouts` type is the resolved collection that the board
    holds internally.
    [`is_dock_layouts()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md),
    [`as_dock_layouts()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md),
    and
    [`validate_dock_layouts()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
    remain exported.
  - Unexported `new_dock_layout()`; use
    [`dock_layout()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout.md)
    instead.
  - Unexported `view_ids()` and `view_can_crud()`. Both were internal
    helpers exposed by accident; renamed to `layout_ids()` and
    `views_can_crud()` respectively to align with what they operate on.
  - Wire format for serialised `dock_layout` decoupled from dockview’s
    internal tree. The persisted shape is a flattened recursive spec:
    the top object carries `orientation`, `children`, optional `sizes`,
    and optional `focus` (the panel with current focus); a child is a
    bare string (single-panel leaf), a `{panels, active?}` object
    (tabbed leaf), or a `{children, sizes?}` object (nested branch).
    Sizes are ratios (auto-normalised from dockview’s pixel sizes); even
    splits omit `sizes`; the default open tab omits `active`; focus on
    the first leaf omits `focus`. `focus` round-trips the focused group
    (dockview’s `activeGroup`) by naming a stable panel rather than the
    regenerated group id. Legacy payloads (with the dockview-shape
    `grid` field) load via a shape-discriminated reader.
    Producer-version routing is tracked in
    [\#153](https://github.com/BristolMyersSquibb/blockr.dock/issues/153)
    (depends on blockr.core forwarding `...` in `blockr_deser.list`).
  - Layout conversion API split by boundary. The R object ↔︎ R list
    boundary uses coercion:
    [`as_dock_layout()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout.md)
    coerces a `dock_layout` (identity), a `board` (its active layout),
    or a spec list to a `dock_layout`;
    [`as.list()`](https://rdrr.io/r/base/list.html) of a `dock_layout`
    returns that spec list. The R object ↔︎ JSON string boundary uses
    explicit verbs:
    [`layout_to_json()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout-json.md)
    /
    [`layout_from_json()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout-json.md).
    Both `as_dock_layout(<list>)` and
    [`layout_from_json()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout-json.md)
    take optional `blocks` / `extensions` to resolve bare IDs and
    validate.
    [`layout_panel_ids()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout-json.md)
    /
    [`panel_obj_ids()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout-json.md)
    inspect the panel / object IDs a layout references. The dockview
    wire format and its converters are not part of the public API — only
    the `dock_layout` object, our JSON, and the spec list are;
    [`as_dock_layout()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout.md)
    rejects a dockview grid-shaped list.

## blockr.dock 0.1.0

CRAN release: 2025-12-11

- Initial CRAN submission
