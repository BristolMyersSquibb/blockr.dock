#' Set up dock board server logic.
#'
#' Entry point called by blockr.core's board server. Creates the dock
#' infrastructure (always multi-view; single-page boards are a
#' degenerate case with one auto-named "Page" view), starts extension
#' servers, and wires up action triggers.
#'
#' @param board Reactive board state (list with `$board`).
#' @param update Reactive update signal from blockr.core.
#' @param visible Reactive write-channel from blockr.core, fed the set of
#'   on-screen block ids so core can gate off-screen blocks.
#' @param ... Extension server arguments.
#' @param session Shiny session.
#'
#' @return List with `dock`, `actions`, `view_data`, and extension
#'   results.
#'
#' @noRd
board_server_callback <- function(board, update, visible, ...,
                                  session = get_session()) {
  initial_board <- isolate(board$board)

  c_exts <- dock_extensions(initial_board)
  exts <- as.list(c_exts)

  actions <- unlst(
    c(
      list(board_actions(initial_board)),
      lapply(exts, board_actions)
    )
  )

  triggers <- action_triggers(actions)

  # Per-session dock state, closure-private. `docks` is the live manage_dock()
  # registry of built dock modules, keyed by view id. `client_views` is the nav
  # model: the nav-rendered set of views plus their display names, and
  # live_view_data()'s name source. It is distinct from `docks` only during the
  # init flush (board_ui seeds the nav statically); reconcile_views() is its
  # sole mutator. `client_active` mirrors which view the browser shows as
  # active. Per-view panel membership lives on each dock proxy (`live_panels`),
  # kept authoritative by the add/remove touchpoints; it drives `n_panels` / the
  # empty-dock prompt and the membership fold without waiting on the lagging
  # browser echo. `docks` is a `reactiveValues`, so live_view_data() depends on
  # each view's entry and re-evaluates when reconcile creates it, whatever the
  # init flush order.
  docks <- reactiveValues()
  active_dock <- reactiveValues()
  client_active <- reactiveVal(NULL)
  client_views <- reactiveVal(seed_view_state(board_views(initial_board)))

  # The `visible` channel core hands us is the single store for per-block
  # visibility (parked / required / rendered), and doubles as the dock's build
  # ledger read back via built_cards(). Stash it on active_dock -- the dock
  # handle every card-touching path receives (view switch, panel-op apply,
  # core-driven insert / remove) -- so they read and update the one channel.
  active_dock$visible <- visible

  switch_view_observer(session, update, client_active)
  add_view_observer(client_views, session, board, update)
  remove_view_observer(client_views, session, update)
  rename_view_observer(client_views, session, update)

  # Reconcile the live dock session against the committed board (create /
  # destroy / restore / rename / switch views), keeping apply_board_update a
  # pure reducer. No ignoreInit: initial render is the empty-`docks` case.
  observeEvent(
    board$board,
    reconcile_views(
      board, update, docks, active_dock, client_active, client_views,
      session
    )
  )

  # Gate off-screen blocks from the first flush, before the client reports its
  # layout (else core's all-visible default evaluates every block at startup).
  # Seed the channel to what board_ui rendered: the active view's whole
  # membership is built, its front panels `required` (on screen, not yet
  # arranged) and its background tabs `parked`. Off-screen views' cards are
  # inserted on first visit by switch_active_view. Core holds its
  # background-construction gate until the active view reports `rendered`.
  visible(
    card_visibility(
      active_view_block_ids(initial_board),
      visible_block_ids(active_view_grid(initial_board))
    )
  )

  report_visible_observer(visible, client_active, docks)

  # `view_data` is the live all-views layout, split into a `dock_views` +
  # `dock_grids` pair. It is NULL until every view has reported its layout once
  # (the all-or-nothing in live_view_data), so consumers req() it. A view's
  # live arrangement is read on demand here; it no longer folds back into the
  # board's slots.
  view_data <- live_view_data(client_views, docks, client_active)

  # Each extension can read the others' server results through the
  # `extensions` environment: one active binding per id, each resolving to
  # the live result (carrying its `state`). `ext_res` is filled right after
  # this lapply, so a binding resolves on first read -- always post-init
  # (e.g. on a tool call). Lets one extension (such as the assistant) read
  # another's controllable state via `extensions[[id]]`.
  peers <- new.env(parent = emptyenv())

  bind_peer <- function(id) {
    makeActiveBinding(id, function() ext_res[[id]], peers)
  }

  for (ext_id in names(exts)) {
    bind_peer(ext_id)
  }

  # Two bundles of live handles, kept in step. Extension servers are called
  # (below) with `board` + `update` (read / mutate the board), `view_data` +
  # `actions` (the live products) and the `extensions` peer env. The callback
  # RETURNS `dock`, `view_data`, `actions` and the extensions' results to core,
  # which spreads them into every plugin's args. `view_data` + `actions` are in
  # both -- they are consumed on each side (serialization and the edit-block
  # plugin read the returned pair). The gaps are deliberate: `dock`
  # (active_dock) is returned for core's block insert / remove plugin but
  # withheld from extensions, which read layout via `view_data`; `board` /
  # `update` are core's own inputs, not echoed back; the peer env stays
  # internal (core gets the resolved `ext_res`).
  #
  # The extension results ride under a single `extensions` key rather than as
  # bare per-extension entries. Bare, they enter core's arg-matching namespace:
  # a container-owned key (`edit`, `dag`, ...) would partial-match a formal
  # (`edit_block`, or a consumer's `dag_extension`) and either hijack the block
  # server or silently mis-deliver. Bundled, a consumer that wants an
  # extension's result names it explicitly -- `extensions[[extension_ids(
  # board$board, <class>)]]` -- keyed by id, resolved from the class it knows.
  # `register_actions()` hands actions the same bundle for the same reason.
  ext_res <- set_names(
    map(
      extension_server,
      exts,
      names(exts),
      MoreArgs = list(
        list(
          board = board,
          update = update,
          view_data = view_data,
          actions = triggers,
          extensions = peers
        ),
        list(...)
      )
    ),
    names(exts)
  )

  # Externally controllable extension state is applied here, in the closure
  # that owns ext_res, rather than from the (now pure) apply hook. The update
  # payload carries the mod directly; the reactiveVal writes are idempotent.
  observeEvent(
    update()$extensions$mod,
    apply_extensions_mod(update()$extensions$mod, ext_res)
  )

  register_actions(actions, triggers, board, update, ext_res)

  # Returned to core, spread into every plugin's args (see the two-bundle note
  # above): `dock` for block placement, `view_data` for serialization, `actions`
  # for the edit-block plugin, and the extensions' resolved results.
  list(
    dock = active_dock,
    actions = triggers,
    view_data = view_data,
    extensions = ext_res
  )
}

# The initial `client_views`: one bare (empty) view per view id, carrying the
# display name. Gives live_view_data and the nav the view set before any
# dockview has reported its live layout; the dock modules themselves are
# created by the reconcile pass (its empty-`docks` case). Which view is active
# is tracked solely by `client_active`, not here.
seed_view_state <- function(views) {
  reconstruct_dock_views(lapply(views, bare_view))
}

# An empty view standing in for a view in `client_views`: carries the view's
# display name so live_view_data / the nav keep the id -> name mapping without
# its membership or geometry.
bare_view <- function(x) {
  new_dock_view(character(), view_name(x))
}

#' Observe view tab switches.
#'
#' A tab click (`input$view_nav` carries the target view id) requests an
#' active-view change through the update lifecycle; the reconcile pass does
#' the DOM switch. Guarded so a no-op (e.g. the nav echoing a programmatic
#' `value` set back) does not re-enter the lifecycle.
#'
#' @param session Shiny session.
#' @param update Board update signal.
#' @param client_active Reactive holding the client-shown active view id.
#'
#' @noRd
switch_view_observer <- function(session, update, client_active) {
  observeEvent(
    session$input$view_nav,
    {
      target <- session$input$view_nav

      if (!identical(target, isolate(client_active()))) {
        update(list(views = list(active = target)))
      }
    },
    ignoreInit = TRUE
  )
}

report_visible_observer <- function(visible, client_active, docks) {

  # Owns the channel's membership axis: the active view's front panels are
  # `required`, everything else built is `parked`. It leaves `rendered` alone --
  # the per-view arrange observer promotes required -> rendered once the client
  # has painted (mark_cards_rendered), so the two touch disjoint transitions.
  # `req(layout())` waits for the client's report (NULL before then).
  on_screen <- reactive({
    active <- req(client_active())
    dock <- req(docks[[active]])
    layout <- req(dock$layout())

    sort(visible_block_ids(layout))
  })

  observeEvent(on_screen(), show_cards(visible, on_screen()))
}

# The grid mirror's observer, split from manage_dock for testability. It reacts
# to the view's settled `_state` echo and commits the client layout verbatim in
# canonical form, only when it differs (beyond the sash-size tolerance) from
# what is stored -- so a sash drag or tab switch is at most one board commit, a
# re-echo after quiescence none, and the echo of the mirror's own restore
# converges without churn (it matches what was pushed). It does not restrict to
# membership: a panel absent from the view is an inert ghost, pruned at the
# compose / restore boundary, never by this writer.
observe_grid_echo <- function(id, dock, board, commit_grid) {

  # Bridge (#301): a server-side stand-in for the gesture-settled `_state`
  # emission dockViewR does not yet ship. Current dockViewR streams per-frame
  # states through a sash drag, so without this the mirror would commit per
  # frame -- an interactive regression versus main's cadence. Unlike the
  # pre-rework debounce (which raced a second writer and diffed specs), this
  # mirror is the sole writer, commits verbatim canonical values, and diffs
  # nothing: the only cost is 250 ms of bounded staleness on a slot whose
  # readers are all boundaries. Remove once the settled `_state` chain lands.
  settled <- debounce(reactive(dock$layout()), 250)

  observeEvent(
    settled(),
    {
      views <- board_views(board$board)

      if (!id %in% names(views)) {
        return()
      }

      state <- settled()

      if (is.null(state)) {
        return()
      }

      grid <- as_dock_grid(as_dock_layout(state))
      stored <- board_grids(board$board)[[id]]

      # Same geometry within the sash-position noise floor -> nothing to commit,
      # so window-resize jitter is absorbed while a real drag still writes.
      if (isTRUE(all.equal(stored, grid, tolerance = grid_size_tol()))) {
        return()
      }

      commit_grid(grid)
    },
    ignoreInit = TRUE
  )
}

# Returns NULL while any view is still pending — its dock not yet created by
# reconcile, or its dockview layout not yet reported — so downstream observers
# `req()` past the initial flush. Reading `docks[[v_id]]` (a reactiveValues)
# takes a dependency on each view's entry, so this re-evaluates when reconcile
# creates the dock — whatever the init flush order — rather than stranding on
# the empty-`docks` early return. The active view comes from `client_active`,
# not `client_views`. The live all-views layout is returned split into a
# `dock_views` (membership + names + active) and a `dock_grids` (geometry) --
# the same shape the board stores.
live_view_data <- function(client_views, docks, client_active) {
  reactive({

    state <- client_views()

    grids <- lapply(names(state), live_view_grid, docks = docks)

    if (any(lgl_ply(grids, is.null))) {
      return(NULL)
    }

    grids <- set_names(grids, names(state))

    views <- reconstruct_dock_views(
      set_names(map(live_view_membership, grids, state), names(state))
    )

    ca <- client_active()

    if (!is.null(ca)) {
      active_view(views) <- ca
    }

    list(views = views, grids = new_dock_grids(grids))
  })
}

live_view_grid <- function(v_id, docks) {

  dk <- docks[[v_id]]

  if (is.null(dk)) {
    return(NULL)
  }

  ly <- dk$layout()

  if (is.null(ly)) {
    return(NULL)
  }

  as_dock_grid(as_dock_layout(ly))
}

live_view_membership <- function(grid, view) {
  new_dock_view(layout_panel_ids(grid), view_name(view))
}

#' Hide all block and extension UI for a view.
#'
#' Moves block/extension cards off-screen (back to offcanvas) so they are
#' not visible while the view is inactive.
#'
#' @param view_id View id (string).
#' @param docks `reactiveValues` of dock module results, keyed by view id.
#'
#' @noRd
hide_view_ui <- function(view_id, docks) {
  if (is.null(view_id) || !(view_id %in% names(docks))) {
    return()
  }
  dock <- docks[[view_id]]
  bns <- dock_board_ns(dock)
  for (bid in as_obj_id(block_panel_ids(dock$proxy))) {
    hide_block_ui(bid, dock$proxy$session, board_ns = bns)
  }
  for (eid in as_obj_id(ext_panel_ids(dock$proxy))) {
    hide_ext_ui(eid, dock$proxy$session, board_ns = bns)
  }
}

#' Show all block and extension UI for a view.
#'
#' Restores block/extension cards into the viewport when a view
#' becomes active.
#'
#' @param view_id View id (string).
#' @param docks `reactiveValues` of dock module results, keyed by view id.
#'
#' @noRd
show_view_ui <- function(view_id, docks) {
  if (is.null(view_id) || !(view_id %in% names(docks))) {
    return()
  }
  dock <- docks[[view_id]]
  bns <- dock_board_ns(dock)
  for (bid in as_obj_id(block_panel_ids(dock$proxy))) {
    show_block_ui(bid, dock$proxy$session, board_ns = bns)
  }
  for (eid in as_obj_id(ext_panel_ids(dock$proxy))) {
    show_ext_ui(eid, dock$proxy$session, board_ns = bns)
  }
}

# Insert a view's dock container, start its manage_dock module, and register
# the result in `docks` under the view id. `active` stamps the active CSS
# class at insert so the initially-shown view needs no switch round-trip.
create_view <- function(v_id, layout, board, update, session, docks, visible,
                        blocks = NULL, extensions = NULL, active = FALSE) {

  ns <- session$ns

  insertUI(
    selector = paste0("#", ns("view_container")),
    where = "beforeEnd",
    ui = div(
      id = ns(as_view_handle_id(v_id)),
      class = paste(
        "blockr-view-dock",
        if (isTRUE(active)) "blockr-view-dock-active"
      ),
      dockViewR::dock_view_output(
        ns(NS(v_id, dock_id())),
        width = "100%",
        height = "100%"
      ),
      uiOutput(NS(ns(v_id), "empty_prompt"))
    ),
    immediate = TRUE,
    session = session
  )

  docks[[v_id]] <- manage_dock(
    v_id, board, update, visible,
    layout = layout,
    blocks = blocks,
    extensions = extensions
  )

  invisible()
}

# Destroy a view's dock module, deregister it from `docks`, and remove its DOM
# container. Parks the view's block / ext UI in the offcanvas first so a block
# surviving in another view keeps its single-instance board-level UI.
remove_view <- function(v_id, session, docks) {

  if (!(v_id %in% names(docks))) {
    return(invisible())
  }

  hide_view_ui(v_id, docks)
  destroy_module(v_id, session = session)
  removeUI(
    selector = paste0("#", session$ns(as_view_handle_id(v_id))),
    immediate = TRUE,
    session = session
  )
  trim_rv(docks, v_id)

  invisible()
}

# Make the live dock session match the committed board's view set:
# instantiate added views, tear down removed ones, relabel renamed ones, and
# switch to the active view. The board owns which views exist, their names and
# the active one; a view's per-view arrangement is client-owned, read live via
# view_data() and never folded back into the board, so reconcile never
# pushes a layout to its live dock. Takes the reactive board handle (not a
# snapshot) so the live list reaches manage_dock, whose interaction observers
# read board$board after the fact (#194); the committed board is snapshotted
# once here for this pass's reads. Driven by board$board
# so apply_board_update.dock_board stays a pure reducer and dock state never
# crosses the package boundary. Idempotent — a board already matching the live
# view set produces no surgery — so it composes with the membership fold
# (DOM -> board) without ping-ponging.
reconcile_views <- function(board, update, docks, active_dock,
                            client_active, client_views, session) {

  brd <- board$board

  views <- board_views(brd)
  grids <- board_grids(brd)
  want <- names(views)
  labels <- view_names(views)
  server_active <- active_view(views)
  state <- isolate(client_views())
  have <- names(docks)
  shown <- names(state)
  visible <- isolate(active_dock$visible)

  for (v in setdiff(want, have)) {

    create_view(
      v,
      view_grid(views[[v]], if (is.null(grids)) NULL else grids[[v]]),
      board,
      update,
      session,
      docks,
      visible,
      blocks = board_blocks(brd),
      extensions = dock_extensions(brd),
      active = identical(v, server_active)
    )

    state[[v]] <- bare_view(views[[v]])
  }

  # Add a nav item only for views the nav doesn't already show. `board_ui`
  # renders every seeded view statically, so this set is empty on init;
  # gating on `docks` (also empty on init) instead re-adds each as a
  # blank-labelled duplicate (#189). Label from the container `view_names()`,
  # which resolves whether the name sits on the layout or is derived from id.
  for (v in setdiff(want, shown)) {
    session$sendInputMessage(
      "view_nav",
      list(add = list(id = v, name = labels[[v]]))
    )
  }

  for (v in setdiff(have, want)) {

    remove_view(v, session, docks)
    session$sendInputMessage("view_nav", list(remove = v))
    state[[v]] <- NULL
  }

  for (v in intersect(want, have)) {

    new_nm <- view_name(views[[v]])

    if (!identical(new_nm, view_name(state[[v]]))) {
      view_name(state[[v]]) <- new_nm
      session$sendInputMessage(
        "view_nav",
        list(rename = list(id = v, to = new_nm))
      )
    }
  }

  client_views(state)

  if (!is.null(server_active) &&
        !identical(server_active, isolate(client_active()))) {
    switch_active_view(
      server_active, docks, active_dock, client_active, brd, session
    )
  }

  invisible()
}

#' Run a single dockViewR instance as a Shiny module.
#'
#' Sets up the dock view, restores an initial layout, and handles panel
#' add/remove interactions. Used both for single-dock boards and for each
#' view in multi-view boards.
#'
#' @param id Module ID.
#' @param board,update Reactive board state and update signal.
#' @param layout Optional initial placement `dock_grid`; defaults to the
#'   board's active view grid.
#'
#' @return The view's `dock` handle: a list holding the dockViewR `proxy`
#'   alongside `board_ns`, `live_panels`, `layout` (reactive), `n_panels`,
#'   `prev_active_group`, and `active_group_trail`.
#'
#' @noRd
manage_dock <- function(
  id,
  board,
  update,
  visible,
  layout = NULL,
  blocks = NULL,
  extensions = NULL
) {
  init_board <- isolate(board$board)
  init_layout <- coal(layout, active_view_grid(init_board))
  init_blocks <- coal(blocks, board_blocks(init_board))
  init_exts <- coal(extensions, dock_extensions(init_board))

  # Block/ext cards live at the board (parent) namespace level
  board_ns <- get_session()$ns

  moduleServer(id, function(input, output, session) {

    # `dock` is our handle for this view's live dock: it *contains* the
    # dockViewR proxy alongside the server-side state the dock helpers need --
    # the board-level namespace, the authoritative panel-membership set, and the
    # module's reactive outputs. The proxy itself is never mutated. n_panels
    # derives from the membership set, so the empty-dock prompt no longer waits
    # on the browser's `n-panels` echo.
    proxy <- set_dock_view_output(session = session)
    live_panels <- reactiveVal(as.character(layout_panel_ids(init_layout)))
    prev_active_group <- reactiveVal()
    active_group_trail <- reactiveVal()
    n_panels <- reactive(length(live_panels()))

    dock <- list(
      proxy = proxy,
      board_ns = board_ns,
      live_panels = live_panels,
      layout = reactive(dockViewR::get_dock(proxy)),
      n_panels = n_panels,
      prev_active_group = prev_active_group,
      active_group_trail = active_group_trail,
      visible = visible
    )

    # Apply server-initiated panel ops to this view's live dock -- the sole
    # mutator of the dock. Every membership change (the add-panel modal, a
    # closed tab, an extension op) reaches the board via `update()` and lands
    # here. Keyed on the applied `views$mod` for the view (the `set_panel_title`
    # precedent), never on a board diff — `move` / `select` write nothing to the
    # board, so a state-diff observer never sees them. Each op is idempotent
    # against the live panel set, so a re-augmented payload settles without
    # ping-pong.
    observeEvent(
      update()$views$mod[[id]],
      apply_panel_ops(
        update()$views$mod[[id]],
        dock,
        board$board,
        rm_blocks = update()$blocks$rm %||% character(),
        active = identical(active_view(board$board), id)
      ),
      ignoreInit = TRUE
    )

    # The settled-echo grid mirror: the sole writer of this view's stored
    # geometry. `commit_grid` is the write capability, held only by the
    # observer below. dockViewR emits one settled `_state` per gesture (a
    # gesture's layout mutations coalesce onto a microtask), so a re-echo after
    # quiescence canonicalises identically and writes nothing.
    commit_grid <- function(grid) {
      update(list(views = list(grid = set_names(list(grid), id))))
    }

    observe_grid_echo(id, dock, board, commit_grid)

    if (get_log_level() >= debug_log_level) {
      observeEvent(
        input[[dock_input("active-group")]],
        {
          # nolint next: object_usage_linter.
          ag <- input[[dock_input("active-group")]]
          log_debug("active group is now {ag}")
        }
      )
    }

    observeEvent(
      req(input[[dock_input("initialized")]]),
      {
        restore_layout(init_layout, dock$proxy,
                       blocks = init_blocks, extensions = init_exts)

        for (pid in as_dock_panel_id(as_dock_grid(init_layout))) {
          if (is_block_panel_id(pid)) {
            show_block_panel(pid, add_panel = FALSE, dock = dock)
          } else if (is_ext_panel_id(pid)) {
            show_ext_panel(pid, add_panel = FALSE, dock = dock)
          } else {
            blockr_abort(
              "Unknown panel type {class(pid)}.",
              class = "dock_panel_invalid"
            )
          }
        }

        # Every card the layout calls for is now moved into its panel: this
        # view is arranged. Mark its on-screen blocks `rendered` on the channel
        # -- the client-confirmed paint core's construction gate waits for.
        mark_cards_rendered(visible, visible_block_ids(init_layout))
      },
      once = TRUE
    )

    # A closed tab (dockview's remove plugin is `manual`, so it waits for the
    # server) emits an `rm` panel-op; the apply observer above removes it.
    # Membership is authoritative from the update, so no later reconcile can
    # restore a view still listing the closed panel (#217).
    observeEvent(
      input[[dock_input("panel-to-remove")]],
      update(remove_panel_delta(id, input[[dock_input("panel-to-remove")]]))
    )

    observeEvent(
      input[[dock_input("panel-to-add")]],
      suggest_panels_to_add(dock, board, session = session)
    )

    # Empty-dock prompt — rendered for all empty docks, visibility
    # handled by the dock wrapper's CSS (opacity/pointer-events).
    output$empty_prompt <- renderUI({
      if (n_panels() == 0) empty_dock_prompt(session$ns)
    })

    observeEvent(
      input$empty_dock_add,
      suggest_panels_to_add(dock, board, panels = list(), session = session)
    )

    # The add-panel modal emits an `add` panel-op; the apply observer places the
    # panels. The `+` was clicked on a group, so anchor the add `within` a
    # member of that group (`near`); an empty dock has no group and falls back
    # to the view's default spot.
    observeEvent(
      input$confirm_add,
      {
        req(input$add_dock_panel)

        ref_group <- input[[dock_input("panel-to-add")]]

        near <- if (not_null(ref_group)) {
          group_front_panel(dock, ref_group)
        }

        update(add_panel_delta(id, input$add_dock_panel, near))

        removeModal()
      }
    )

    observeEvent(
      input[[dock_input("active-group")]],
      {
        cur_ag <- input[[dock_input("active-group")]]
        pre_ag <- active_group_trail()
        if (!identical(pre_ag, cur_ag)) {
          log_trace("setting previous active group to {pre_ag}")
          prev_active_group(pre_ag)
        }
        active_group_trail(cur_ag)
      }
    )

    observeEvent(
      update()$blocks$mod,
      {
        blks <- update()$blocks$mod

        for (blk_id in names(blks)) {

          delta <- blks[[blk_id]]

          if (!"block_name" %in% names(delta)) {
            next
          }

          blk_panel_id <- as_block_panel_id(blk_id)

          # This observer fires for every view; skip the docks that do not hold
          # this block. live_panels() is the authoritative server-side
          # membership set -- pushing a title to a panel absent from this dock
          # reaches dockView with an unknown id and throws client-side.
          if (!as.character(blk_panel_id) %in% live_panels()) {
            next
          }

          new_name <- delta[["block_name"]]
          old_title <- get_dock_panel(blk_panel_id, dock$proxy)$title

          if (identical(new_name, old_title)) {
            next
          }

          log_debug("setting panel title {blk_panel_id} to '{new_name}'")

          dockViewR::set_panel_title(
            dock$proxy,
            blk_panel_id,
            new_name
          )
        }
      }
    )

    dock
  })
}

#' Observe view addition requests.
#'
#' Shows a modal to name the new view and pick blocks/extensions, then emits
#' an `add` + `active` views delta; the reconcile pass instantiates the dock
#' and switches to it.
#'
#' @param client_views Reactive record of the client-shown views.
#' @param session Shiny session.
#' @param board Reactive board state.
#' @param update Board update signal.
#'
#' @noRd
add_view_observer <- function(client_views, session, board, update) {
  input <- session$input
  output <- session$output
  ns <- session$ns

  # Show modal for view creation
  observeEvent(input$view_nav_add, {
    req(views_can_crud(client_views()))

    state <- client_views()
    existing <- view_names(state)
    n <- length(state) + 1L
    while (paste("Page", n) %in% existing) n <- n + 1L
    default_name <- paste("Page", n)

    brd <- board$board
    blk_options <- build_block_options(brd, board_block_ids(brd))
    ext_options <- build_ext_options(brd, dock_ext_ids(brd))

    showModal(
      modalDialog(
        title = "New view",
        size = "l",
        easyClose = TRUE,
        footer = NULL,
        tagList(
          css_modal(),
          textInput(
            ns("view_new_name"),
            "View name",
            value = default_name
          ),
          if (length(blk_options)) {
            tagList(
              css_block_selectize(),
              selectizeInput(
                ns("view_new_blocks"),
                label = "Blocks to show",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  options = blk_options,
                  valueField = "value",
                  labelField = "label",
                  searchField = c("label", "description", "searchtext"),
                  placeholder = "Select blocks...",
                  openOnFocus = FALSE,
                  plugins = list("remove_button"),
                  render = js_blk_selectize_render()
                )
              )
            )
          },
          if (length(ext_options)) {
            selectizeInput(
              ns("view_new_exts"),
              label = "Extensions to show",
              choices = NULL,
              multiple = TRUE,
              options = list(
                options = ext_options,
                valueField = "value",
                labelField = "label",
                searchField = c("label", "description", "searchtext"),
                placeholder = "Select extensions...",
                openOnFocus = FALSE,
                plugins = list("remove_button"),
                render = js_blk_selectize_render()
              )
            )
          },
          uiOutput(ns("view_name_validation")),
          confirm_button(ns("confirm_view_add"), label = "Create view")
        )
      )
    )
  })

  # Name validation feedback
  output$view_name_validation <- renderUI({
    req(input$view_new_name)
    msg <- validate_view_name(
      trimws(input$view_new_name),
      view_names(client_views())
    )
    if (!is.null(msg)) tags$div(class = "text-danger", msg)
  })

  # Confirm view creation. The new view is created through the update
  # lifecycle: a stable id is minted in `augment_board_update.dock_board()`
  # and `apply_views_add()` instantiates the dock — the same path a
  # delta-driven add takes, so id assignment happens in exactly one place.
  observeEvent(input$confirm_view_add, {
    state <- client_views()
    new_name <- trimws(input$view_new_name)

    if (!is.null(validate_view_name(new_name, view_names(state)))) {
      return()
    }

    removeModal()

    brd <- board$board
    sel_blks <- intersect(
      coal(input$view_new_blocks, character()),
      board_block_ids(brd)
    )
    sel_exts <- intersect(
      coal(input$view_new_exts, character()),
      dock_ext_ids(brd)
    )

    members <- c(
      as.character(as_ext_panel_id(sel_exts)),
      as.character(as_block_panel_id(sel_blks))
    )

    # Switch to the new view on creation. Its id is minted in augment, so
    # we point `active` at its `add` key (the display name); the dock
    # resolves that to the minted id in `normalize_views_delta()`.
    update(
      list(
        views = list(
          add = set_names(list(dock_view(members)), new_name),
          active = new_name
        )
      )
    )
  })
}

#' Observe view removal requests.
#'
#' Shows a confirmation modal, then emits an `rm` views delta; the reconcile
#' pass destroys the dock module, removes the DOM container, and switches to
#' another view if the removed one was active.
#'
#' @param client_views Reactive record of the client-shown views.
#' @param session Shiny session.
#' @param update Board update signal.
#'
#' @noRd
remove_view_observer <- function(client_views, session, update) {
  input <- session$input
  ns <- session$ns

  # Show confirmation modal. `input$view_nav_remove` carries the view id;
  # the modal shows the display name.
  observeEvent(input$view_nav_remove, {
    req(views_can_crud(client_views()))

    rm_id <- input$view_nav_remove
    state <- client_views()

    if (!rm_id %in% names(state)) {
      return()
    }

    if (length(state) <= 1L) {
      notify("Cannot remove the last view.")
      return()
    }

    rm_name <- coal(view_name(state[[rm_id]]), rm_id, fail_all = FALSE)

    showModal(
      modalDialog(
        title = "Remove view",
        size = "s",
        easyClose = TRUE,
        footer = NULL,
        tagList(
          tags$p(
            "Are you sure you want to remove view ",
            tags$strong(rm_name),
            "?"
          ),
          div(
            style = "display: flex; justify-content: flex-end; gap: 8px;
              margin-top: 20px;",
            modalButton("Cancel"),
            actionButton(
              ns("confirm_view_remove"),
              "Remove",
              class = "btn-danger"
            )
          )
        )
      )
    )
  })

  # Removal flows through the update lifecycle: `apply_views_rm()` tears
  # down the dock module, drops the view from the board and the live
  # state, and re-syncs the nav switcher.
  observeEvent(input$confirm_view_remove, {
    removeModal()

    rm_id <- input$view_nav_remove
    state <- client_views()

    if (!rm_id %in% names(state) || length(state) <= 1L) {
      return()
    }

    update(list(views = list(rm = rm_id)))
  })
}

#' Observe view rename requests.
#'
#' Rename is a pure name-attribute write: `input$view_nav_rename` carries the
#' stable view id and the new name. It travels through the update lifecycle as
#' a `rename` delta; the reconcile pass writes the new name into `client_views`
#' and the nav. The id — and hence the dock module, DOM element and registry
#' key — is untouched.
#'
#' @param client_views Reactive record of the client-shown views.
#' @param session Shiny session.
#' @param update Board update signal.
#'
#' @noRd
rename_view_observer <- function(client_views, session, update) {
  input <- session$input

  observeEvent(input$view_nav_rename, {
    req(views_can_crud(client_views()))

    rename <- input$view_nav_rename

    if (!rename$id %in% names(client_views())) {
      return()
    }

    update(
      list(views = list(rename = set_names(list(rename$to), rename$id)))
    )
  })
}

#' Show a modal for adding panels to the dock.
#'
#' Lists blocks and extensions not yet shown in the dock. If none are
#' available, either triggers `suggest_new` or notifies the user.
#'
#' @param dock Dock proxy.
#' @param board Reactive board state.
#' @param suggest_new If truthy, called when no panels are available
#'   (used to prompt adding a new block).
#' @param panels Currently visible panels (auto-detected if `NULL`).
#' @param session Shiny session.
#'
#' @noRd
suggest_panels_to_add <- function(
  dock,
  board,
  suggest_new = FALSE,
  panels = NULL,
  session = get_session()
) {
  ns <- session$ns

  if (is.null(panels)) {
    panels <- dock_panel_ids(dock$proxy)
  }

  stopifnot(is.list(panels), all(lgl_ply(panels, is_dock_panel_id)))

  blk_opts <- setdiff(
    board_block_ids(board$board),
    as_obj_id(panels[lgl_ply(panels, is_block_panel_id)])
  )

  ext_opts <- setdiff(
    dock_ext_ids(board$board),
    as_obj_id(panels[lgl_ply(panels, is_ext_panel_id)])
  )

  options_data <- c(
    build_block_options(board$board, blk_opts, value_fun = as_block_panel_id),
    build_ext_options(board$board, ext_opts, value_fun = as_ext_panel_id)
  )

  if (length(options_data)) {
    showModal(
      modalDialog(
        title = "Add panel",
        size = "l",
        easyClose = TRUE,
        footer = NULL,
        tagList(
          css_modal(),
          css_block_selectize(),
          selectizeInput(
            ns("add_dock_panel"),
            label = "Select panel to add",
            choices = NULL,
            multiple = TRUE,
            options = list(
              options = options_data,
              valueField = "value",
              labelField = "label",
              searchField = c("label", "description", "searchtext"),
              placeholder = "Type to search...",
              openOnFocus = FALSE,
              plugins = list("remove_button"),
              render = js_blk_selectize_render()
            )
          ),
          confirm_button(ns("confirm_add"), label = "Add Panel"),
          auto_focus_script(ns("add_dock_panel"))
        )
      )
    )
  } else if (!isFALSE(suggest_new)) {
    suggest_new(TRUE)
  } else if (
    length(board_block_ids(board$board)) == 0L &&
      length(dock_ext_ids(board$board)) == 0L
  ) {
    notify("The board has no blocks yet. Add a new block to get started.")
  } else {
    notify(
      paste(
        "All blocks and extensions are already in this view.",
        "Add a new block to the board first."
      )
    )
  }
}

#' Default icon for extensions in the panel picker.
#' @noRd
extension_default_icon <- function() {
  as.character(bsicons::bs_icon("gear"))
}

#' Build a single selectize option entry.
#'
#' Shared structure for block and extension options in panel pickers.
#'
#' @param value Option value (ID, possibly prefixed).
#' @param label Display label.
#' @param id Raw object ID.
#' @param package Package name string.
#' @param icon Icon HTML string.
#' @param color Colour string.
#'
#' @return A named list.
#'
#' @noRd
build_one_option <- function(value, label, id, package, icon, color) {
  list(
    value = value,
    label = label,
    description = paste0("ID: ", id),
    package = package,
    icon = icon,
    color = color,
    searchtext = paste(label, id, package)
  )
}

#' Build selectize option entries for blocks.
#'
#' @param board Board object.
#' @param blk_ids Character vector of block IDs to include.
#' @param value_fun Coercion applied to each ID to form the option value;
#'   defaults to `identity` (bare IDs, for a block-only selectize). The
#'   panel picker passes `as_block_panel_id` so a single mixed selectize
#'   can be disambiguated on read-back.
#'
#' @return A list of option lists suitable for `selectizeInput`.
#'
#' @noRd
build_block_options <- function(board, blk_ids, value_fun = identity) {
  if (!length(blk_ids)) {
    return(list())
  }

  blks <- board_blocks(board)[blk_ids]
  meta <- blks_metadata(blks)

  lapply(seq_along(blk_ids), function(i) {
    id <- blk_ids[i]
    build_one_option(
      value = as.character(value_fun(id)),
      label = block_name(blks[[id]]),
      id = id,
      package = meta$package[i],
      icon = meta$icon[i],
      color = meta$color[i]
    )
  })
}

#' Build selectize option entries for extensions.
#'
#' @param board Board object.
#' @param ext_ids Character vector of extension IDs to include.
#' @param value_fun Coercion applied to each ID to form the option value;
#'   defaults to `identity` (bare IDs, for an extension-only selectize). The
#'   panel picker passes `as_ext_panel_id` so a single mixed selectize
#'   can be disambiguated on read-back.
#'
#' @return A list of option lists suitable for `selectizeInput`.
#'
#' @noRd
build_ext_options <- function(board, ext_ids, value_fun = identity) {
  if (!length(ext_ids)) {
    return(list())
  }

  all_exts <- as.list(dock_extensions(board))

  lapply(ext_ids, function(ext_id) {
    ext <- all_exts[[ext_id]]
    ext_name <- extension_name(ext)
    ext_pkg <- ctor_pkg(extension_ctor(ext))

    build_one_option(
      value = as.character(value_fun(ext_id)),
      label = ext_name,
      id = ext_id,
      package = coal(ext_pkg, "local"),
      icon = extension_default_icon(),
      color = "#999999"
    )
  })
}

#' Validate a view name.
#'
#' With identity carried by a stable id, the name is a free-form display
#' label: the only remaining rules are display concerns — non-empty and
#' (to keep tabs unambiguous) not a duplicate of another view's name.
#'
#' @param name Trimmed view name string.
#' @param existing Character vector of existing view names.
#'
#' @return Error message string, or `NULL` if valid.
#'
#' @noRd
validate_view_name <- function(name, existing) {
  if (nchar(name) == 0L) {
    "Name cannot be empty."
  } else if (name %in% existing) {
    "A view with this name already exists."
  } else {
    NULL
  }
}
