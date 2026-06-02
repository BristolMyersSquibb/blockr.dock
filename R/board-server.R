#' Set up dock board server logic.
#'
#' Entry point called by blockr.core's board server. Creates the dock
#' infrastructure (always multi-view; single-page boards are a
#' degenerate case with one auto-named "Page" view), starts extension
#' servers, and wires up action triggers.
#'
#' @param board Reactive board state (list with `$board`).
#' @param update Reactive update signal from blockr.core.
#' @param ... Extension server arguments.
#' @param session Shiny session.
#'
#' @return List with `dock`, `actions`, `view_data`, and extension
#'   results.
#'
#' @noRd
board_server_callback <- function(board, update, ..., session = get_session()) {
  initial_board <- isolate(board$board)

  c_blks <- board_blocks(initial_board)
  c_exts <- dock_extensions(initial_board)

  exts <- as.list(c_exts)

  actions <- unlst(
    c(
      list(board_actions(initial_board)),
      lapply(exts, board_actions)
    )
  )

  triggers <- action_triggers(actions)

  views <- board_layouts(initial_board)

  dock_mgr <- new_dock_manager()
  dock_mgr$board_rv <- board
  dock_mgr$update <- update
  dock_mgr$triggers <- triggers

  vs <- init_view_docks(
    views,
    board,
    update,
    triggers,
    session,
    dock_mgr,
    blocks = c_blks,
    extensions = c_exts
  )
  dock_mgr$vs <- vs

  switch_view_observer(vs, session, dock_mgr)
  add_view_observer(vs, session, dock_mgr, board, update)
  remove_view_observer(vs, session, dock_mgr)
  rename_view_observer(vs, session, dock_mgr)

  # Extensions receive active_dock — a reactiveValues that always mirrors
  # whichever view is currently active (swapped by update_active_dock).
  dock <- dock_mgr$active_dock
  view_data <- live_view_data(vs, dock_mgr)

  sync_layouts_to_board(view_data, update, board)

  # Each extension can read the others' server results through the
  # `extensions` environment: one active binding per id, each resolving to
  # the live result (carrying its `state`). `dock_mgr$ext_res` is filled
  # right after this lapply, so a binding resolves on first read -- always
  # post-init (e.g. on a tool call). Lets one extension (such as the
  # assistant) read another's controllable state via `extensions[[id]]`.
  peers <- new.env(parent = emptyenv())

  bind_peer <- function(id) {
    makeActiveBinding(id, function() dock_mgr$ext_res[[id]], peers)
  }

  for (ext_id in names(exts)) {
    bind_peer(ext_id)
  }

  ext_res <- lapply(
    exts,
    extension_server,
    list(
      board = board,
      update = update,
      dock = dock,
      actions = triggers,
      extensions = peers
    ),
    list(...)
  )

  # Externally controllable extension state is written from the apply path
  # (apply_board_update.dock_board), which reaches these live reactiveVals
  # through dock_mgr.
  dock_mgr$ext_res <- ext_res

  register_actions(actions, triggers, board, update, ext_res)

  c(
    list(
      dock = dock,
      actions = triggers,
      view_data = view_data,
      dock_mgr = dock_mgr
    ),
    ext_res
  )
}

#' Create a dock manager.
#'
#' Plain environment bundling all view dock infrastructure so it can
#' be passed as a single parameter to helper functions.
#'
#' @return An environment with:
#' \describe{
#'   \item{`docks`}{Environment mapping view ids to dock module results
#'     (each a list with `layout`, `proxy`, `dock_id`, etc.). The view id
#'     is also the dock module id, so DOM and module ids derive from it.}
#'   \item{`active_dock`}{A `reactiveValues` that mirrors the dock module
#'     result of the currently active view. Extensions hold a stable
#'     reference to this; its contents are swapped by `update_active_dock()`
#'     when the user switches views.}
#' }
#'
#' @noRd
new_dock_manager <- function() {
  mgr <- new.env(parent = emptyenv())
  mgr$docks <- new.env(parent = emptyenv())
  mgr$active_dock <- reactiveValues()
  mgr
}

#' Initialise dock modules for each view.
#'
#' For every view in `views`, inserts a dock view output container
#' into the DOM and starts a `manage_dock()` module. View layouts are
#' read from `views` once for seeding; afterwards, layouts live
#' exclusively in `dock_mgr$docks` (no duplication in `vs$state`).
#'
#' @param views A `dock_layouts` object (from the initial board).
#' @param board,update,triggers Reactive board state, update signal, and
#'   action triggers — forwarded to `manage_dock()`.
#' @param session Shiny session.
#' @param dock_mgr Dock manager created by `new_dock_manager()`.
#'
#' @return A `reactiveValues` with a single slot `$state` — a
#'   `dock_layouts` object tracking view ids, display names, and which is
#'   active. Layouts are empty; the authoritative layout data lives in
#'   `dock_mgr$docks[[view_id]]$layout()`.
#'
#' @noRd
init_view_docks <- function(views, board, update, triggers, session, dock_mgr,
                            blocks, extensions) {
  ns <- session$ns
  active_v <- active_view(views)
  current_active <- reactiveVal(active_v)

  for (v_id in names(views)) {
    v_ly <- views[[v_id]]
    dock_output_id <- ns(NS(v_id, dock_id()))

    insertUI(
      selector = paste0("#", ns("view_container")),
      where = "beforeEnd",
      ui = div(
        id = ns(as_view_handle_id(v_id)),
        class = paste(
          "blockr-view-dock",
          if (identical(v_id, active_v)) "blockr-view-dock-active"
        ),
        dockViewR::dock_view_output(
          dock_output_id,
          width = "100%",
          height = "100%"
        ),
        uiOutput(NS(ns(v_id), "empty_prompt"))
      ),
      immediate = TRUE,
      session = session
    )

    local_id <- v_id
    dock_res <- manage_dock(
      v_id,
      board,
      update,
      triggers,
      layout = v_ly,
      is_active = reactive(identical(current_active(), local_id)),
      blocks = blocks,
      extensions = extensions
    )
    dock_res$dock_id <- v_id
    dock_mgr$docks[[v_id]] <- dock_res
  }

  update_active_dock(dock_mgr$active_dock, dock_mgr$docks[[active_v]])
  dock_mgr$current_active <- current_active

  bare <- reconstruct_dock_layouts(lapply(views, bare_view))
  active_view(bare) <- active_v
  reactiveValues(state = bare)
}

# An empty layout standing in for a view in `vs$state`: carries the
# view's display name so live_view_data / serialization keep the
# id -> name mapping without duplicating the layout itself.
bare_view <- function(x) {
  ly <- new_dock_layout()
  nm <- view_name(x)
  if (!is.null(nm)) {
    view_name(ly) <- nm
  }
  ly
}

#' Observe view tab switches.
#'
#' When the user selects a different tab via `input$view_nav` (carrying the
#' target view id), hides block/ext UI for the old view, shows it for the
#' new one, updates `vs$state`, and swaps `dock_mgr$active_dock` to point at
#' the new view's dock.
#'
#' @param vs Reactive view state (from `init_view_docks()`).
#' @param session Shiny session.
#' @param dock_mgr Dock manager.
#'
#' @noRd
switch_view_observer <- function(vs, session, dock_mgr) {
  input <- session$input

  observeEvent(
    input$view_nav,
    {
      new_id <- input$view_nav
      state <- vs$state
      old_id <- active_view(state)

      session$sendCustomMessage(
        "switch-view",
        list(
          id = session$ns(
            as_view_handle_id(dock_mgr$docks[[new_id]]$dock_id)
          )
        )
      )

      if (!identical(old_id, new_id)) {
        hide_view_ui(old_id, dock_mgr$docks)
        show_view_ui(new_id, dock_mgr$docks)

        active_view(state) <- new_id
        vs$state <- state
        update_active_dock(dock_mgr$active_dock, dock_mgr$docks[[new_id]])
        dock_mgr$current_active(new_id)
      }
    },
    ignoreInit = TRUE
  )
}

# Returns NULL while any view's dockview `_state` input is still
# pending — observers downstream can `req()` past the initial flush.
live_view_data <- function(vs, dock_mgr) {
  reactive({
    state <- vs$state
    v_list <- lapply(names(state), function(v_id) {
      ly <- dock_mgr$docks[[v_id]]$layout()
      if (is.null(ly)) {
        return(NULL)
      }
      out <- dockview_to_layout(ly)
      nm <- view_name(state[[v_id]])
      if (!is.null(nm)) {
        view_name(out) <- nm
      }
      out
    })

    if (any(lgl_ply(v_list, is.null))) {
      return(NULL)
    }

    res <- reconstruct_dock_layouts(set_names(v_list, names(state)))
    active_view(res) <- active_view(state)
    res
  })
}

# Writes go through update(list(views = ...)) — board$board is
# read-only at the plugin boundary; routing through the update channel
# lets apply_board_update.dock_board mutate rv$board where it's writable
# and composes with augment/apply hooks from other subclasses. Debounced
# because drag-resize emits many rapid events per gesture.
sync_layouts_to_board <- function(view_data, update, board, millis = 250) {
  src <- if (millis > 0L) shiny::debounce(view_data, millis) else view_data
  layouts_to_board_observer(src, update, board)
}

layouts_to_board_observer <- function(view_data, update, board) {
  observe({
    new_layouts <- req(view_data())
    current <- isolate(board_layouts(board$board))

    if (identical(current, new_layouts)) {
      return()
    }

    delta <- diff_dock_layouts(current, new_layouts)

    if (length(delta)) {
      update(list(views = delta))
    }
  })
}

# Structured `views` delta between two `dock_layouts`, keyed by stable
# view id, so live UI state mirrors back through the update lifecycle
# without resending unchanged views. Per-view comparison uses the wire
# spec (ignores volatile fields and the display name — renames travel on
# their own slot); the active-view change rides the `$active` slot. Since
# the id is stable across a rename, a rename never surfaces here as a
# removal of the old view paired with an addition of a new one.
diff_dock_layouts <- function(current, new_layouts) {

  cur_ids <- names(current)
  new_ids <- names(new_layouts)

  add_ids <- setdiff(new_ids, cur_ids)
  rm_ids <- setdiff(cur_ids, new_ids)
  common <- intersect(cur_ids, new_ids)

  mod_views <- list()
  for (v in common) {

    cur_spec <- layout_to_spec(current[[v]])
    new_spec <- layout_to_spec(new_layouts[[v]])

    if (!identical(cur_spec, new_spec)) {
      mod_views[[v]] <- new_layouts[[v]]
    }
  }

  cur_active <- active_view(current)
  new_active <- active_view(new_layouts)

  out <- list()

  if (length(add_ids)) {
    out$add <- as.list(unclass(new_layouts))[add_ids]
  }
  if (length(rm_ids)) {
    out$rm <- rm_ids
  }
  if (length(mod_views)) {
    out$mod <- mod_views
  }
  if (!identical(cur_active, new_active) && !is.null(new_active)) {
    out$active <- new_active
  }

  out
}

#' Hide all block and extension UI for a view.
#'
#' Moves block/extension cards off-screen (back to offcanvas) so they are
#' not visible while the view is inactive.
#'
#' @param view_id View id (string).
#' @param docks Environment of dock module results (`dock_mgr$docks`).
#'
#' @noRd
hide_view_ui <- function(view_id, docks) {
  if (!exists(view_id, envir = docks, inherits = FALSE)) {
    return()
  }
  proxy <- docks[[view_id]]$proxy
  bns <- proxy_board_ns(proxy)
  for (bid in as_obj_id(block_panel_ids(proxy))) {
    hide_block_ui(bid, proxy$session, board_ns = bns)
  }
  for (eid in as_obj_id(ext_panel_ids(proxy))) {
    hide_ext_ui(eid, proxy$session, board_ns = bns)
  }
}

#' Show all block and extension UI for a view.
#'
#' Restores block/extension cards into the viewport when a view
#' becomes active.
#'
#' @param view_id View id (string).
#' @param docks Environment of dock module results (`dock_mgr$docks`).
#'
#' @noRd
show_view_ui <- function(view_id, docks) {
  if (!exists(view_id, envir = docks, inherits = FALSE)) {
    return()
  }
  proxy <- docks[[view_id]]$proxy
  bns <- proxy_board_ns(proxy)
  for (bid in as_obj_id(block_panel_ids(proxy))) {
    show_block_ui(bid, proxy$session, board_ns = bns)
  }
  for (eid in as_obj_id(ext_panel_ids(proxy))) {
    show_ext_ui(eid, proxy$session, board_ns = bns)
  }
}

#' Run a single dockViewR instance as a Shiny module.
#'
#' Sets up the dock view, restores an initial layout, and handles panel
#' add/remove interactions. Used both for single-dock boards and for each
#' view in multi-view boards.
#'
#' @param id Module ID.
#' @param board,update Reactive board state and update signal.
#' @param actions Action triggers.
#' @param layout Optional initial `dock_layout`; defaults to the board's
#'   `active_layout()`.
#' @param is_active A [shiny::reactive()] returning `TRUE` when this dock
#'   is the currently active view. Controls whether the empty-dock prompt
#'   is shown. Defaults to always-active (single-dock boards).
#'
#' @return A list with `layout` (reactive), `proxy`, `prev_active_group`,
#'   `n_panels`, and `active_group_trail`.
#'
#' @noRd
manage_dock <- function(
  id,
  board,
  update,
  actions,
  layout = NULL,
  is_active = reactive(TRUE),
  blocks = NULL,
  extensions = NULL
) {
  init_board <- isolate(board$board)
  init_layout <- coal(layout, active_layout(init_board))
  init_blocks <- coal(blocks, board_blocks(init_board))
  init_exts <- coal(extensions, dock_extensions(init_board))

  # Block/ext cards live at the board (parent) namespace level
  board_ns <- get_session()$ns

  moduleServer(id, function(input, output, session) {
    dock <- set_dock_view_output(session = session)
    dock$board_ns <- board_ns

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
        restore_layout(init_layout, dock,
                       blocks = init_blocks, extensions = init_exts)

        for (pid in as_dock_panel_id(init_layout)) {
          if (is_block_panel_id(pid)) {
            show_block_panel(pid, add_panel = FALSE, proxy = dock)
          } else if (is_ext_panel_id(pid)) {
            show_ext_panel(pid, add_panel = FALSE, proxy = dock)
          } else {
            blockr_abort(
              "Unknown panel type {class(pid)}.",
              class = "dock_panel_invalid"
            )
          }
        }
      },
      once = TRUE
    )

    n_panels <- reactiveVal(
      length(determine_active_views(init_layout))
    )

    observeEvent(
      req(input[[dock_input("n-panels")]]),
      n_panels(input[[dock_input("n-panels")]])
    )

    observeEvent(
      input[[dock_input("panel-to-remove")]],
      {
        pid <- as_dock_panel_id(
          input[[dock_input("panel-to-remove")]]
        )

        if (is_block_panel_id(pid)) {
          hide_block_panel(pid, rm_panel = TRUE, proxy = dock)
          n_panels(n_panels() - 1L)
        } else if (is_ext_panel_id(pid)) {
          hide_ext_panel(pid, rm_panel = TRUE, proxy = dock)
          n_panels(n_panels() - 1L)
        } else {
          blockr_abort(
            "Unknown panel type {class(pid)}.",
            class = "dock_panel_invalid"
          )
        }
      }
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

    observeEvent(
      input$confirm_add,
      {
        req(input$add_dock_panel)

        ref_group <- input[[dock_input("panel-to-add")]]

        pos <- if (!is.null(ref_group)) {
          list(referenceGroup = ref_group, direction = "within")
        } else {
          # Empty dock — no reference group, let DockView place freely
          TRUE
        }

        for (pid in input$add_dock_panel) {
          if (maybe_block_panel_id(pid)) {
            show_block_panel(
              board_blocks(board$board)[as_obj_id(new_block_panel_id(pid))],
              add_panel = pos,
              proxy = dock
            )

            n_panels(n_panels() + 1L)
          } else if (maybe_ext_panel_id(pid)) {
            exts <- as.list(dock_extensions(board$board))

            show_ext_panel(
              exts[[as_obj_id(new_ext_panel_id(pid))]],
              add_panel = pos,
              proxy = dock
            )

            n_panels(n_panels() + 1L)
          } else {
            blockr_abort(
              "Unknown panel specification {pid}.",
              class = "dock_panel_invalid"
            )
          }
        }

        removeModal()
      }
    )

    prev_active_group <- reactiveVal()
    active_group_trail <- reactiveVal()

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

          if (!"block_name" %in% names(blks[[blk_id]])) {
            next
          }

          new_name <- block_name(board_blocks(board$board)[[blk_id]])
          blk_panel_id <- as_block_panel_id(blk_id)

          old_title <- get_dock_panel(blk_panel_id, dock)$title

          if (identical(new_name, old_title)) {
            next
          }

          log_debug("setting panel title {blk_panel_id} to '{new_name}'")

          dockViewR::set_panel_title(
            dock,
            blk_panel_id,
            new_name
          )
        }
      }
    )

    list(
      layout = reactive(dockViewR::get_dock(dock)),
      proxy = dock,
      prev_active_group = prev_active_group,
      n_panels = n_panels,
      active_group_trail = active_group_trail
    )
  })
}

#' Observe view addition requests.
#'
#' Shows a modal to name the new view and pick blocks/extensions,
#' then inserts a new dock module, updates `vs$state`, and switches to it.
#'
#' @param vs Reactive view state.
#' @param session Shiny session.
#' @param dock_mgr Dock manager.
#' @param board,update,triggers Reactive board state, update signal, and
#'   action triggers — forwarded to `manage_dock()`.
#'
#' @noRd
add_view_observer <- function(vs, session, dock_mgr, board, update) {
  input <- session$input
  output <- session$output
  ns <- session$ns

  # Show modal for view creation
  observeEvent(input$view_nav_add, {
    req(views_can_crud(vs$state))

    state <- vs$state
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
    msg <- validate_view_name(trimws(input$view_new_name), view_names(vs$state))
    if (!is.null(msg)) tags$div(class = "text-danger", msg)
  })

  # Confirm view creation. The new view is created through the update
  # lifecycle: a stable id is minted in `augment_board_update.dock_board()`
  # and `apply_views_add()` instantiates the dock — the same path a
  # delta-driven add takes, so id assignment happens in exactly one place.
  observeEvent(input$confirm_view_add, {
    state <- vs$state
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
    v_blks <- board_blocks(brd)[sel_blks]
    v_exts <- as_dock_extensions(
      as.list(dock_extensions(brd))[sel_exts]
    )
    # Mark the new view active so it is switched to on creation; the id is
    # minted in augment, so we can't reference it by id here — the add
    # layout's active marker is how "add and activate" travels.
    v_ly <- set_active_view(
      resolve_dock_layout(blocks = v_blks, extensions = v_exts)
    )

    update(list(views = list(add = set_names(list(v_ly), new_name))))
  })
}

#' Observe view removal requests.
#'
#' Shows a confirmation modal, then destroys the dock module, removes
#' the DOM container, updates `vs$state`, and switches to another view
#' if the removed one was active.
#'
#' @param vs Reactive view state.
#' @param session Shiny session.
#' @param dock_mgr Dock manager.
#'
#' @noRd
remove_view_observer <- function(vs, session, dock_mgr) {
  input <- session$input
  ns <- session$ns

  # Show confirmation modal. `input$view_nav_remove` carries the view id;
  # the modal shows the display name.
  observeEvent(input$view_nav_remove, {
    req(views_can_crud(vs$state))

    rm_id <- input$view_nav_remove
    state <- vs$state

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
    state <- vs$state

    if (!rm_id %in% names(state) || length(state) <= 1L) {
      return()
    }

    dock_mgr$update(list(views = list(rm = rm_id)))
  })
}

#' Observe view rename requests.
#'
#' Rename is a pure name-attribute write: `input$view_nav_rename` carries
#' the stable view id and the new name. The view's id — and hence its dock
#' module, DOM element and `dock_mgr$docks` key — is untouched; only the
#' display name changes, in `vs$state` and (via a `rename` delta) on the
#' board. No structure is re-keyed.
#'
#' @param vs Reactive view state.
#' @param session Shiny session.
#' @param dock_mgr Dock manager.
#'
#' @noRd
rename_view_observer <- function(vs, session, dock_mgr) {
  input <- session$input

  observeEvent(input$view_nav_rename, {
    req(views_can_crud(vs$state))

    rename <- input$view_nav_rename
    id <- rename$id
    state <- vs$state

    if (!id %in% names(state)) {
      return()
    }

    view_name(state[[id]]) <- rename$to
    vs$state <- state

    dock_mgr$update(
      list(views = list(rename = set_names(list(rename$to), id)))
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
    panels <- dock_panel_ids(dock)
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
