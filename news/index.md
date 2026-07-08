# Changelog

## blockr.dock (development version)

- The block status badge is now derived in one exported helper,
  [`block_status_badge()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/meta.md),
  and reused by blockr.dag, so the dock card icon and the DAG node badge
  always render the same status, colour and geometry (including the
  white ring both previously hardcoded) instead of each front-end
  deriving and styling it independently. As a consequence the dock’s own
  status dot now reflects render-phase errors – a block that evaluates
  cleanly but errors while rendering its output, which the eval status
  alone reports as `ready`; previously only the DAG badge flagged those
  ([\#314](https://github.com/BristolMyersSquibb/blockr.dock/issues/314)).

- Renaming a block no longer crashes a board where that block is absent
  from some view – a block placed in only one of several views, or
  parked in the offcanvas with no panel at all. The per-view rename
  observer fires for every view; it now skips the ones whose dock does
  not hold the renamed block instead of pushing the new title to a panel
  that view lacks, which reached dockView with an unknown id and threw
  client-side
  ([\#116](https://github.com/BristolMyersSquibb/blockr.dock/issues/116)).

- A board’s per-view layout is now split into two independent slots: a
  `dock_views` collection of structure objects (each view’s ordered
  panel-id set, name and id, plus the active view), read with
  [`board_views()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md),
  and a separate `NULL`-valid `dock_grids` slot of grid geometry, read
  with
  [`board_grids()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
  ([\#273](https://github.com/BristolMyersSquibb/blockr.dock/issues/273)).
  Structure is server-authoritative and always current; a view’s grid
  may be absent. A stored grid must reference only panels in its view’s
  membership (`grid ⊆ membership`, checked in
  [`validate_board()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_board.html)),
  and a board is valid with no grids at all. The DSL is unchanged –
  `new_dock_board(layouts = ...)` still takes fused `dock_layout()`s and
  splits them at construction – and `board_layouts()` composes the two
  slots back into the grid-bearing handle the update lifecycle reads.
  The serialized form carries both fields.

- Block eval status is now a first-class panel affordance instead of
  leaking through as an incidental warning
  ([\#290](https://github.com/BristolMyersSquibb/blockr.dock/issues/290)).
  Consuming the `board$eval[[id]]` status enum that blockr.core exposes,
  a `waiting` block (a required data input is unconnected) or an `unset`
  block (a required user input is not yet provided) shows a dedicated
  placeholder in its output region – “Waiting for a data input” / “Set
  this block’s inputs” – rather than core’s status explanation being
  painted as a generic warning alert. That `status`-phase condition is
  now separated from genuine warnings so the two are no longer
  conflated. A small colour-coded dot in the block card header marks
  `waiting`, `unset` and `failed` blocks, so a board shows at a glance
  which blocks are not yet producing output.

- Per-block condition UI (warnings, messages and errors) is now updated
  surgically using the stable condition id blockr.core assigns each
  condition, instead of tearing down and rebuilding the whole region on
  every change
  ([\#36](https://github.com/BristolMyersSquibb/blockr.dock/issues/36)).
  An unchanged condition stays in place while a newly raised one is
  inserted next to it and a resolved one is removed on its own, so a
  persistent warning no longer flashes on each re-evaluation.

- The busy pulse no longer flashes on a plain panel switch, only on real
  computation
  ([\#285](https://github.com/BristolMyersSquibb/blockr.dock/issues/285)).
  [`serve()`](https://bristolmyerssquibb.github.io/blockr.core/reference/serve.html)
  enables shiny’s page pulse (`useBusyIndicators(pulse = TRUE)`), which
  shows on any server-busy flush – and a panel switch round-trips to the
  server (the on-screen visibility report and the layout fold) without
  recomputing a visible output, so the pulse fired for what is only
  layout bookkeeping. A CSS rule now gates the pulse on a genuinely
  recalculating output inside the visible view container: startup and
  block evaluation still pulse, a bare panel switch does not. A block
  still pending evaluation in the (hidden) offcanvas pool sits outside
  the container, so it never forces the pulse either.

- The “Edit board” extension now re-syncs its staged working copy only
  when the board’s links or stacks actually change, not on every board
  re-emit
  ([\#281](https://github.com/BristolMyersSquibb/blockr.dock/issues/281)).
  The two observers keying `upd$curr` / `stk$curr` off
  [`board_links()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_blocks.html)
  /
  [`board_stacks()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_blocks.html)
  fired on each board invalidation – `observeEvent` does not
  value-dedupe – so a benign dock interaction (a panel switch or view
  fold, which re-emits the board via
  [\#201](https://github.com/BristolMyersSquibb/blockr.dock/issues/201))
  churned the editor’s working copy even when no link or stack changed.
  A `reactiveVal` +
  [`identical()`](https://rdrr.io/r/base/identical.html) guard now gates
  each re-sync on a real change, so the
  [\#277](https://github.com/BristolMyersSquibb/blockr.dock/issues/277)
  and
  [\#279](https://github.com/BristolMyersSquibb/blockr.dock/issues/279)
  guards become defensive rather than load-bearing.

- The “Edit board” extension no longer flickers the manage-links table’s
  cell selectize inputs on a board re-emit
  ([\#279](https://github.com/BristolMyersSquibb/blockr.dock/issues/279)).
  The observer that keeps the table in sync re-rendered every row
  whenever `upd$curr` was reset – `observeEvent(names(upd$curr))` fires
  on each invalidation, not only when the link ids change – and the
  redundant
  [`DT::replaceData`](https://rdrr.io/pkg/DT/man/replaceData.html)
  unbound and rebound the From / To / Input inputs, briefly blanking a
  selectize until the async redraw landed. A panel switch re-emits the
  board (via
  [\#201](https://github.com/BristolMyersSquibb/blockr.dock/issues/201)),
  so plain navigation churned the table. The table now redraws only when
  the set of link ids actually changes; value edits and no-op re-emits
  are skipped, while applying staged changes still redraws through its
  own path.

- The “Edit board” extension no longer loses unsaved link and stack
  edits when the board reactive re-emits
  ([\#277](https://github.com/BristolMyersSquibb/blockr.dock/issues/277)).
  Two observers reset the staged working copy (`upd$curr` / `stk$curr`)
  to the board’s applied links and stacks on every re-emit, so a staged
  row vanished from the table while its half-filled entry lingered in
  `upd$add` and later failed apply with “Expecting all links to refer to
  known block IDs”. A layout change in an adjacent panel group is enough
  to re-emit the board, which is why
  [\#201](https://github.com/BristolMyersSquibb/blockr.dock/issues/201)
  surfaced this. The refresh now overlays the staged additions, edits
  and removals onto the refreshed applied state instead of clobbering
  them.

- Dock extensions now receive `view_data`, the live all-views layout
  reactive (the same one serialization reads), so an extension can read
  the current arrangement of every view directly instead of folding it
  through `board_layouts(board$board)`. `view_data()` is `NULL` until
  every view has reported its layout once, so consumers should `req()`
  it. The `dock` handle (the active-view `active_dock` mirror) is
  retired from the extension surface at the same time: it is internal
  now, used only by the board-level block insert / remove plugin
  ([\#264](https://github.com/BristolMyersSquibb/blockr.dock/issues/264)).

- Multi-view boards no longer emit a burst of redundant board updates at
  startup
  ([\#271](https://github.com/BristolMyersSquibb/blockr.dock/issues/271)).
  Restoring a multi-group layout makes the dockview client transiently
  activate each group in turn, and the dock -\> board arrangement fold
  (`layouts_to_board_observer`) compared layouts through
  `layout_to_spec`, whose `focus` field tracks the active group – so
  each cross-group focus tick read as a layout change and committed an
  `update(list(views = ...))`, on the order of ten redundant updates
  before the board settled (worst on large boards, e.g. 12 views / 99
  blocks / 30 groups). That fold is removed: a view’s live arrangement
  is now read on demand – on save, or by an extension via `view_data()`
  – instead of mirrored back into `board_layouts` on every dockview
  tick, so the burst is gone. The removal is lossless, since
  `serialize_board.dock_board` already reads the live layout from
  `view_data()` and the fold had no remaining runtime reader;
  `board_layouts` still carries the committed view set, names, active
  view and panel membership, kept current by the membership fold
  ([\#264](https://github.com/BristolMyersSquibb/blockr.dock/issues/264)).

- The dock no longer loops forever or tears its panels down on a slow
  client
  ([\#252](https://github.com/BristolMyersSquibb/blockr.dock/issues/252)).
  The live layout was held in two bindings that formed a cycle: the
  live-sync fold pushed the dockview client’s state into `board_layouts`
  (dock -\> board), and a reconcile step pushed `board_layouts` back to
  the widget (board -\> dock). A board update that originated at the
  dock still triggered a re-push, whose echo folded back; on a slow
  client a partial client state folded an impoverished layout that the
  push then faithfully restored. The board -\> dock arrangement push
  (`reconcile_view_layout()` / `apply_layout_diff()`) is removed: a
  view’s arrangement is client-owned and now flows dock -\> board only.
  `reconcile_views()` still owns what the board is authoritative over –
  which views exist, their names, the active view, and the initial
  layout on load. With one direction live there is no echo to suppress
  and no layout-tracking state to maintain.

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
  is now a pure reducer over `board_layouts()`; all live view surgery
  (instantiate / tear down / restore / rename / switch) runs in a single
  closure-resident reconcile pass driven by the committed board,
  replacing the duplicated delta-driven and UI-driven view CRUD. View
  init is just the empty-registry case of that pass (create every view,
  show the active one), so there is no separate init path. The
  per-session dock state is ordinary closure-private state passed
  explicitly, not a handle threaded back from the board callback through
  `dot_args`. Also makes `augment_board_update.dock_board()` idempotent
  — a view id is minted once rather than re-minted on every augment pass
  — fixing a view-add loop
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
    branches with explicit ratios. `dock_layout()` itself also accepts
    `sizes =` for root-level ratios and `orientation =` for the
    top-level split direction.
  - Removed
    [`dock_view()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md),
    [`dock_grid()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout.md),
    [`is_dock_grid()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock-grid.md),
    and
    [`as_dock_grid()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock-grid.md).
    Use `dock_layout(...)` (or the new
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
    holds internally. `is_dock_layouts()`, `as_dock_layouts()`, and
    `validate_dock_layouts()` remain exported.
  - Unexported
    [`new_dock_layout()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock-layout.md);
    use `dock_layout()` instead.
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
    [`as_dock_layout()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock-layout.md)
    coerces a `dock_layout` (identity), a `board` (its active layout),
    or a spec list to a `dock_layout`;
    [`as.list()`](https://rdrr.io/r/base/list.html) of a `dock_layout`
    returns that spec list. The R object ↔︎ JSON string boundary uses
    explicit verbs: `layout_to_json()` / `layout_from_json()`. Both
    `as_dock_layout(<list>)` and `layout_from_json()` take optional
    `blocks` / `extensions` to resolve bare IDs and validate.
    [`layout_panel_ids()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/panel-ids.md)
    /
    [`panel_obj_ids()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/panel-ids.md)
    inspect the panel / object IDs a layout references. The dockview
    wire format and its converters are not part of the public API — only
    the `dock_layout` object, our JSON, and the spec list are;
    [`as_dock_layout()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock-layout.md)
    rejects a dockview grid-shaped list.

## blockr.dock 0.1.0

CRAN release: 2025-12-11

- Initial CRAN submission
