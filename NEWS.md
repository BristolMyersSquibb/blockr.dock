# blockr.dock (development version)

* A rail's width now survives a viewport too narrow to render it. A rail is
  sized in pixels and is the low-priority view of dockView's shell splitview,
  so a dock that cannot fit it squeezes the rail rather than the centre -- and
  hands the space back to the centre, not the rail, once there is room again.
  The settled-echo mirror used to commit that width, which left the board
  holding the narrowest viewport it had ever been opened at, and every later
  restore came back at that. It now takes a rail's width from the client only
  across an echo that left the dock's own width alone -- a rail sash moves the
  boundary between the rail and the centre and leaves that total untouched,
  while a viewport change moves it -- so a sash drag still persists and a
  resize no longer rewrites what the user set.

  The mirror also ignores the empty dock a view echoes before its restore
  reaches it. That echo places nothing, and committing it blanked the view's
  stored geometry, leaving the arrangement to be rebuilt from whatever the
  client rendered next -- which for a rail is a width, not the same width back
  (#457).

* On a narrow viewport, a view's nested grid now flattens into a single
  vertical stack: every tab group survives as its own row, railed ones
  included, and the page scrolls from one to the next instead of columns
  running off-screen. A row keeps the height the wide layout gave it -- so a
  group the author sized down stays short rather than being padded out --
  capped at `blockr.narrow_group_fraction` of the viewport (0.8 by default, a
  fraction in (0, 1]). The width is read once, at startup, so reflowing takes
  a reload.

  The collapse is opt-in and off until a deployment sets
  `blockr.narrow_breakpoint`. Setting it to `Inf` collapses at every width,
  which is how a board meant only for phones asks to always stack. An unusable
  value aborts rather than falling back to a default -- anything under 50 px,
  `NA`, or a string that is not a number -- since a deployment that sets a
  breakpoint has an intent, and silently rendering the other layout would hide
  the typo until someone opened the board on a phone.

  The stack is a render and nothing more: a narrow session writes no geometry
  back, so the board keeps the layout a wide viewport restores to and a save
  from a phone still persists it (#413).

* Blocks and extensions can now sit in a **rail** -- a tab group pinned to one edge of a view, out of the splitview, rather than a grid cell competing with the panels for width. Every board offers a left and a right edge; a rail is written among a view's grid children with `rail()`, and the default board parks the extensions on the left. Which rails a dock *offers* is a constant, so a grid only records which are *populated* -- an empty rail is invisible, and a board stored before rails existed offers the same edges as any other. A railed panel is absent from the grid tree, so the placement walks are untouched and a view's members are the union of the two, with membership still authoritative for which panels exist while the tree and the rails only say where. Dragging a panel into or out of a rail is stock dockview, and the arrangement rides the settled-echo mirror and `save_dock()` / `restore_dock()` like the rest of the grid does (#431).

  A rail's visibility is derived rather than stored: a rail holding panels
  is shown, an empty one is hidden. That rule replaces a pin concept and a
  persisted flag, and it matches the invariant the grid already keeps. Since
  a hidden rail has no hit area, dragging toward its edge reveals it --
  collapsed, so an empty rail shows its bare strip rather than a full-width
  empty pane -- and the drop expands it. A drag that ends anywhere else
  leaves the derived rule to hide it again. Whether a rail is collapsed *is*
  stored, since unlike visibility it cannot be derived -- an expanded rail
  and a collapsed one hold the same panels -- so a board comes back the way
  it was left.

* The `--blockr-*` design tokens and the host-app-wide theme layer now live in
  blockr.ui, which this package imports and attaches through
  `blockr.ui::theme_dep()`. Nine other packages style themselves from that
  vocabulary, two of them without declaring a dependency here, and the theme
  layer restyled the whole Bootstrap surface -- typography, labels, form
  controls, selectize, buttons, tooltips, popovers and the DataTables chrome
  -- of every app that loaded a docking layout manager. Neither the vocabulary
  nor the theme is this package's to own. What stays is the docking chrome,
  every rule of it scoped to markup generated here (#407).

* Block card chrome that was built on every render and then hidden by the
  stylesheet is gone: the icons and titles fed into the block card's accordion
  headers, which `display: none` also keeps out of the accessibility tree, and
  the export button's icon in the board offcanvas. Dropped with them is a
  `.popover-header` rule that hid a header no popover here renders -- and, the
  stylesheet being host-app-wide, any header a consumer's own popover rendered
  too. The companion `.popover .btn-close` rule stays: bslib injects that button
  into any popover whose trigger does not include "focus", so it is built on the
  client and cannot be dropped at the source (#72).

* Removing blocks that are on screen no longer kills the session with
  "attempt to apply non-function". Cutting a whole stack hit it every time.
  The visible-axis observer drove both card axes straight off the client's
  layout echo, which still names a panel for a tick after core has dropped
  its block, and `mark_cards_rendered()` called the dropped slot. The echo is
  now reconciled against core's visibility slots once, where it is read, so no
  consumer sees a block the server has already dropped (#387).

* A block card whose sections the user has all hidden now comes back hidden,
  and freezes while its controls are off screen. The toggle widget reports
  `NULL` for an empty selection, which is the same value the server holds
  before the card has reported at all, so "the user hid everything" was
  stored as -- and read back as -- "nothing saved": the card reopened both
  sections. The same conflation left the block unfrozen, since the freeze
  gate reads a non-`NULL` report to mean the card has landed. The card now
  reports an empty selection as `character()`, telling it from a card that
  has yet to report, and both halves follow. A board saved before this change
  carries a genuine `NULL` and still restores with both sections open (#426).

* A restored block card paints its saved sections open straight away,
  instead of opening every section and collapsing the hidden ones a
  moment later. The card rendered its accordion open unconditionally and
  left a server observer to correct it, which it could only do once the
  client had reported the toggle widget's value back -- on a four-block
  board that correction landed some five seconds after the card
  appeared, so the user watched the board settle into its saved shape
  rather than opening in it. A locked dock now renders no toggle widget
  at all: it was there purely to drive that correction, hidden behind
  `display: none` yet still a live input a client could set (#418).

* Two selectize widgets that had lost their callers are gone. Adopting
  blockr.ui's link-menu module in the add-link flow deleted the only call to
  `board_select()`, and `block_registry_selectize()` had no caller either.
  Exported in their place is `board_block_select()`, the block picker the
  board itself uses wherever a block is chosen -- the rich selectize listing
  each block by icon, name, ID and defining package -- returned with the
  styling its option rendering requires already attached. Every board-block
  picker in the package now builds through that one function, so a consumer
  outside it gets the widget the product uses rather than a reconstruction
  of it (#427).

* Switching to a view before the board has settled no longer leaves it
  hopping between views for good. The nav reported the server's own
  `sendInputMessage("view_nav", ...)` straight back as a gesture, and with
  two switches in flight the first push's echo landed after the second had
  been applied, missed `switch_view_observer()`'s `client_active` guard and
  was taken for a fresh switch -- whose push echoed in turn. Every hop
  re-evaluated and repainted the whole view, so the board looked like it
  was reloading itself and only a page reload got out of it. The nav no
  longer reports a programmatic update at all: it forgets Shiny's cached
  input value instead, so a later click on the view the server pushed away
  from is still reported. A newly added view is no longer marked active on
  the client either, which the echo was the only thing correcting: an add
  whose delta does not ask to navigate -- an extension creating a view in
  the background -- now leaves the active view alone (#424).

* A block with nothing to configure no longer carries an empty "Block
  inputs" section, nor the toggle that opens it. Core's `new_block()` ui
  default renders no markup at all, so a card for a block configured
  entirely by its links -- `rbind_block`, say -- offered a control that
  opened onto a blank panel (#69).

* Extension state now reaches the saved board. Every extension serialized
  an empty payload while still writing its object and constructor, so a
  save looked complete yet carried none of its extensions' state and a
  restore rebuilt each extension from constructor defaults. An outline
  came back with no annotations, so every block fell to the
  `report = TRUE` default -- a board saved with one block in the report
  came back with all of them in -- and a hand-arranged dag lost its node
  positions (#386).

* The stack sidebar picks a colour with a native `<input type="color">`
  beside the hex field, in place of the hand-rolled hue / lightness
  sliders. The sliders reached only a fixed-saturation slice of the colour
  space -- no black, no white, nothing muted or fully saturated -- and a
  hex value typed from outside that slice was accepted but then silently
  rewritten by the next drag. The native input gives the full gamut, plus
  the platform's own dialog, eyedropper and keyboard handling (#396).

* A block whose inputs changed while it was dormant now carries a muted
  grey status badge instead of none. Core's sixth eval status, `stale`,
  fell through `block_status_badge()` to "no badge", so a block holding
  an out-of-date result looked identical to a healthy one on both the
  dock card icon and the blockr.dag node. A stale block also drops the
  red `failed` badge it would otherwise keep from error conditions
  recorded before the change: those describe inputs it no longer has,
  and it has not re-run since, so the upstream edit may equally have
  fixed the failure as caused it (#408).

* New export `sidebar_owned_by()`, which reports whether a given action
  currently holds one of the board's sidebar panels -- `NULL` when it
  does not, otherwise the panel plus whether it is open and pinned. It
  composes the ownership stamp itself, so a consumer that re-fires an
  action on a new selection can ask "is the form I opened still on
  screen?" without knowing which panel that action fills, which panels a
  board mounts, or how a stamp is spelled. Previously the only way in was
  to read the panel's input value directly, which meant naming dock's
  panels in the consumer (#399).

* A sidebar panel now reports which module wrote the body it is showing.
  Every `show_sidebar()` stamps the writing module's namespaced id on the
  panel -- `NS(<board id>, <action id>)` for a board action -- taken from
  the session the call runs in, so the stamp follows the write rather than
  the gesture and covers paths no consumer can observe, such as a holder
  of the trigger bundle firing a shared-panel action directly. It comes
  back as `owner` beside `open` and `pinned` in the panel's input value,
  letting a consumer that re-fires an editor on a selection tell whether
  the content it put there is still on screen. Filling a panel takes it
  over, so nothing has to declare which panel it writes to release it.
  dock's own auto-close handlers (edit stack / link / inputs, which close
  when their target leaves the board) now gate on this, and no longer
  close a form another action has since written into the shared panel
  (#391).

* A pinned stack / link sidebar now refreshes in place after a commit
  rather than being torn down and rebuilt, so search text, scroll
  position and anything the user entered that the commit did not consume
  survive it. The stack menu's name / colour / id form is server-rendered
  against the board to make that possible -- it still offers a fresh
  stack id after each create -- and with nothing left to rebuild, none of
  the four post-commit paths defers its close to `session$onFlushed()`
  (#393).

* A `select` verb in a view's `views$mod` update is now applied when
  that view's dock is built for the first time in the same update that
  carries the select -- for instance an update that activates a
  never-opened view. Previously the panel-op observer took the select as
  its `ignoreInit` value and dropped it, so the board switched to the
  view but the requested tab was not brought to the front. The pending
  select is now folded into the grid the fresh dock restores from, so the
  tab is fronted on first paint (#324).

* New exports for downstream consumers that translate between bare
  block / extension ids and dock's panel-id scheme: the class predicates
  `is_dock_panel_id()`, `is_block_panel_id()` and `is_ext_panel_id()`,
  and `as_panel_ref()`, which resolves a bare id (or an already
  wire-prefixed one) to a `blk()` / `ext()` ref against a board's block /
  extension id sets, block-first with a hard error on a cross-namespace
  clash (#374).

* "Show code" on a deferred board
  (`background_construction_delay = Inf`) no longer permanently blanks
  the cards of a view first visited after it. The exporter marks every
  block required so the script covers the whole board, and the dock's
  card ledger -- read off that same `required` channel -- mistook the
  export's demand for a built card, so the first visit to the view
  skipped the build and left its panels empty. The dock now reads its
  build ledger off core's `visible` channel, which blockr.core makes a
  logical three-state axis (`NA` never built / `FALSE` built off screen
  / `TRUE` painted), so a `required` write can no longer masquerade as a
  built card (#377).

* New `edit_inputs_action`: a per-block sidebar listing every incoming link
  as an ordered list, so a block's inputs can be managed together rather than
  one edge at a time. For a variadic block each row is a positional slot -
  drag to reorder (each row's source and name move together to its new slot
  via `links$mod`, so `...args` comes out in the new order), rename inline
  (positional <-> named), or remove; an "Add input" block picker below the
  list appends another positional slot when a source is chosen. A finite
  block's rows are its declared ports, each a block-browser selectize that
  picks, redirects or disconnects that port's source. Reorder keeps every
  link's id, so there is no id churn and no new core verb. Surfaces such as
  blockr.dag can trigger it for a selected node.

* A panel id in a `dock_grid()` or view that resolves to no block or
  extension on the board now aborts at `new_dock_board()` rather than
  being dropped in silence. Such an id -- a typo, or an extension's old
  class-derived name (extensions are keyed by their mount name since the
  ids became container-owned) -- used to empty the view, booting the
  board blank with no error, warning, or log entry. Restoring a saved
  board is unchanged: a member or grid leaf whose block is genuinely gone
  still self-heals (#375).

* Block card sections (inputs / outputs / control) are styled from the
  stylesheet, keyed on each panel's stable `data-value`, rather than
  rebuilt per card with `htmltools::tagQuery()` at UI-build time. The
  markup renders identically but the cards build about a third faster --
  a cost that scales with the number of blocks on the active view, so it
  cuts noticeably into the initial app render (#214).

* Closing a dock panel no longer aborts the board update when the same tab is
  closed twice in quick succession. The manual-close plugin leaves a tab's `x`
  in place until the server round-trip removes it, so a rapid double click
  re-fires the close for a panel that is already gone; that stale removal is
  now dropped rather than failing view-membership validation (#362).

* Blocks on a dock group's background tabs render again. dockView mounts a
  group's non-front tabs lazily, so a block card's `move-element` could arrive
  before its panel existed and be dropped, stranding the card in the offcanvas;
  and a bare tab switch did not mark the newly-fronted block visible. Both left
  non-front tabs blank. A dropped move is now stashed and replayed when dockView
  reports the panel active (dockViewR's `dockview:active-panel` event), and the
  visible mark follows the client's live active-panel signal, so selecting a tab
  paints its block (#361).

* Dock extensions can now carry structured, model-facing metadata: the
  `description` argument of `new_dock_extension()` accepts a `new_ext_meta()`
  object documenting each externally controllable variable (with an optional
  type schema and worked examples) and how to drive the extension, read back
  with `ext_meta()` and the per-component `ext_desc()`, `ext_args()`,
  `ext_examples()` and `ext_guidance()` accessors. A bare string keeps working
  as the free-text summary; the earlier `extension_description()` accessor is
  deprecated in favour of `ext_desc()` (#359).

* The navbar busy spinner keeps turning when the browser reports
  `prefers-reduced-motion: reduce`, at a slower 1.6s turn instead of 0.7s.
  It previously dropped the animation entirely, which left a fully styled
  ring frozen in the navbar and reading as a hung session. Windows maps its
  "Animation effects: off" setting (a common managed / VDI default) onto that
  preference, so on those machines the spinner never moved.

* Idle, the navbar busy spinner is now a faint, closed ring rather than a
  gapped three-quarter circle that read as an oversized "C" wherever it sits.
  The darker arc that signals motion is painted on only while the board is
  busy; at rest the ring is a single muted colour and recedes into the navbar.

* Views (pages) can now be reordered from the nav dropdown: each item carries
  up / down controls beside its rename and remove actions. Order is board
  content, so the move travels through the update lifecycle as a new
  `views$order` delta (a total permutation of the view ids) and survives save /
  restore; the server applies it and pushes the settled order back to the nav
  (#351).

* A served board now honours a `?view=<id>` URL query parameter: it opens on
  the named view instead of the board's default active view (matching by
  stable view id, the immutable handle). An absent or unknown id falls back
  to the default, so existing links are unaffected (#323).

* New `edit_link_action`: a sidebar editor for a single existing link. It can
  rename the link's input (turning an unnamed positional slot into a named
  one, or changing an existing name), switch the input slot on a finite
  target, and redirect the link's source or target - under the same
  acyclicity, eligibility and input-name uniqueness rules the "Connect ..."
  menu enforces, with the block pickers rendered like the add-panel picker.
  The link keeps its id across the edit. Surfaces such as blockr.dag can
  trigger it for a selected edge.

* The "Connect ..." sidebar and the block browser's append / prepend forms
  now let a link into a variadic block carry a name: an optional "Input
  name" field appears for variadic targets (leave it blank for a positional
  slot). A supplied name must be unique among the target's inputs, per
  blockr.core's name-or-position model.

* `resolve_free_input()` now resolves a variadic link target to an empty
  (positional) slot instead of a generated `"1"`, `"2"`, ... name, aligning
  the "Connect ..." sidebar with blockr.core's name-or-position variadic
  model (and with blockr.ui's link menu). An integer input name is treated
  by core as a *named* argument, and downstream consumers such as
  blockr.io's Download/Export blocks name files and Excel sheets by the
  link input verbatim, which surfaced as `1.csv` / `2.csv` exports.

* Off-screen views' block cards -- built on first visit rather than at
  startup (#272) -- are now built with the plugin set passed to `serve()`,
  not the board default (#331). `ensure_block_ui()` re-derived the edit /
  control UI from `board_plugins()`, which omits any custom `ctrl_block`,
  so a block's control toggle (the AI "sparkle" when the served plugin is
  `blockr.ai::ai_ctrl_block`) showed on the initially-active view but was
  absent on every other view's cards. The served plugin set now rides
  `active_dock` alongside the visibility channel, so the deferred build --
  the view switch and the add-panel path -- sees the served `ctrl_block`.

* Locked mode is now a server-side trust boundary, not just UI hiding (#127,
  #135, #136). `is_dock_locked()` reads blockr.core's `blockr.locked` option
  (renamed from `blockr.dock_is_locked`), so one flag drives both core's update
  / option gate and dock's UI hides; a deployment setting the old option must
  switch to `blockr.locked`. Hiding a block's input section -- and every block
  on a locked board -- now drives core's per-block `frozen` channel: the
  block's expression is pinned and its inputs are no longer consumed, so a
  forged `Shiny.setInputValue` behind the hidden or read-only controls reaches
  nothing (upstream data still flows, and showing the section again thaws it).
  View switching on a locked board is driven client-side, as core's gate
  rejects the active-view update; the board-options accordion (#135) and the
  empty view's "Add panel" prompt (#136) are dropped when locked.

* At startup the board now builds only the active view's dockView;
  off-screen views' docks are created on first visit rather than all up
  front (#304), mirroring the card deferral below. Building every view's
  dockView left the active group pointing at an off-screen view during the
  startup restore burst, transiently dropping the visible view's blocks and
  starving first paint. `reconcile_views()` now builds only the active
  view's dock and defers the rest; a view without a live dock contributes
  its board-stored grid to `view_data()` rather than blocking it.

* A view's `visible`-axis mark -- the client-confirmed paint blockr.core's
  background-construction gate waits for -- now rides the active view's live
  dockView layout echo, the same signal the `required` axis already tracks,
  rather than a one-shot snapshot of the grid's stored active tab (#304,
  #328). A group's front tab is client-owned (the last-added tab wins) and
  can disagree with the grid, so marking the snapshot left the on-screen
  block suspended and blank on first load while a hidden back tab was marked
  painted. Sourcing the mark from the painted layout follows whichever tab
  dockView actually fronts and re-marks on a tab switch, so the on-screen
  block renders.

* At startup the board now builds only the active view's block cards;
  off-screen views' cards are built on first visit rather than all up
  front (#272). `board_ui()` rendered an edit card for every block across
  every view into the static offcanvas mount, so first paint scaled with
  the total block count, not with what is on screen (~20s of `renderTags`
  on a 99-block, 12-view board). It now renders only the active view's
  cards and defers the rest; `switch_active_view()` (and the active-dock
  panel-op path) inserts a view's cards the first time it is shown, and
  core's `required` channel doubles as the build ledger so a revisit never
  doubles a card.

* Block visibility is coordinated with blockr.core (>= 0.1.4) over its
  two-channel `visibility` interface: a per-block `required` channel the
  dock drives (`TRUE` on screen, `FALSE` built but off screen) and a
  `visible` channel it writes with a view id once the client has painted
  that view. This gives blockr.core an explicit "the initial view is
  painted" signal to gate its background block-server construction on,
  instead of inferring readiness from result-quiescence.

# blockr.dock 0.1.2

* The block-browser, link-menu, stack-menu and sidebar UI components are now
  bundled directly into blockr.dock instead of imported from the (non-CRAN)
  blockr.ui package, so blockr.dock installs from CRAN with no remote
  dependencies.
* Panel operations are now first-class verbs in the `views$mod` update payload
  (`add` / `rm` / `move` / `select`), with panels named by the new typed
  references `blk()` / `ext()` instead of wire-id prefixes. **Breaking:** the
  old set-replace membership form is retired.
* The `views$mod` grammar gains a `resize` verb --
  `resize = list(blk("a", size = 0.3))` sets a panel's group size along its
  splitview axis (a ratio in `(0, 1)`), delivered through dockViewR's `set_size`
  proxy. Like `move` / `select` it is client-owned geometry: pure delivery,
  captured by the grid mirror, no board write (#320).
* A saved dock layout now round-trips exactly through an export / import cycle
  -- the reloaded board re-exports the same tab groups, active tabs and sizes.
  Previously a transient frame the client reports while a restore is still
  settling could be captured into the stored geometry, flattening a tab group
  to separate leaves or emptying a view (#343).
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
* The intrusive page-wide busy pulse is replaced by a small, unobtrusive
  spinner in the navbar, just left of the board-options gear. It always
  occupies its slot as a dim, static ring and, while the session does real
  block evaluation, rises to full contrast and spins -- scoped (as the pulse
  was) to a genuinely recomputing output in the visible view, so startup and
  block evaluation liven it while a bare panel switch does not. Because it is
  always present it never shifts its neighbours and never blinks in or out; a
  configurable minimum-busy delay (`blockr.spinner_delay_ms`, default 200 ms;
  `0` to disable) holds the transition to the busy state so a sub-threshold
  flush no longer flashes it (#345, #355, #360).
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
