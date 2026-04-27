#' Set up dock board server logic.
#'
#' Entry point called by blockr.core's board server. Creates dock
#' infrastructure (single dock or multi-workspace), starts extension
#' servers, and wires up action triggers.
#'
#' @param board Reactive board state (list with `$board`).
#' @param update Reactive update signal from blockr.core.
#' @param ... Extension server arguments.
#' @param session Shiny session.
#'
#' @return List with `dock`, `actions`, and extension results. When
#'   workspaces are present, also includes `ws_data`.
#'
#' @noRd
board_server_callback <- function(board, update, ..., session = get_session()) {

  initial_board <- isolate(board$board)

  exts <- as.list(dock_extensions(initial_board))

  actions <- unlst(
    c(
      list(board_actions(initial_board)),
      lapply(exts, board_actions)
    )
  )

  triggers <- action_triggers(actions)

  workspaces <- board_workspaces(initial_board)

  if (!is.null(workspaces)) {
    dock_mgr <- new_dock_manager()
    ws <- init_workspace_docks(
      workspaces, board, update, triggers, session, dock_mgr
    )

    switch_workspace_observer(ws, session, dock_mgr)
    add_ws_observer(ws, session, dock_mgr, board, update, triggers)
    remove_ws_observer(ws, session, dock_mgr)
    rename_ws_observer(ws, session, dock_mgr)

    # Extensions receive active_dock — a reactiveValues that always mirrors
    # whichever workspace is currently active (swapped by update_active_dock).
    dock <- dock_mgr$active_dock
    ws_data <- live_workspace_data(ws, dock_mgr)

  } else {
    dock <- manage_dock("dock_main", board, update, triggers)
    ws_data <- NULL
  }

  ext_res <- lapply(
    exts,
    extension_server,
    list(board = board, update = update, dock = dock, actions = triggers),
    list(...)
  )

  register_actions(actions, triggers, board, update, ext_res)

  c(
    list(dock = dock, actions = triggers),
    if (!is.null(ws_data)) list(ws_data = ws_data),
    ext_res
  )
}

#' Create a dock manager.
#'
#' Plain environment bundling all workspace dock infrastructure so it can
#' be passed as a single parameter to helper functions.
#'
#' @return An environment with:
#' \describe{
#'   \item{`docks`}{Environment mapping workspace names to dock module results
#'     (each a list with `layout`, `proxy`, `dock_id`, etc.).}
#'   \item{`active_dock`}{A `reactiveValues` that mirrors the dock module
#'     result of the currently active workspace. Extensions hold a stable
#'     reference to this; its contents are swapped by `update_active_dock()`
#'     when the user switches workspaces.}
#'   \item{`next_id`}{Closure returning a unique dock module ID on each call.}
#' }
#'
#' @noRd
new_dock_manager <- function() {
  mgr <- new.env(parent = emptyenv())
  mgr$docks <- new.env(parent = emptyenv())
  mgr$active_dock <- reactiveValues()
  used_ids <- character()
  mgr$next_id <- function() {
    did <- paste0("dock_", rand_names(used_ids))
    used_ids <<- c(used_ids, did)
    did
  }
  mgr
}

#' Initialise dock modules for each workspace.
#'
#' For every workspace in `workspaces`, inserts a dock view output container
#' into the DOM and starts a `manage_dock()` module. Workspace layouts are
#' read from `workspaces` once for seeding; afterwards, layouts live
#' exclusively in `dock_mgr$docks` (no duplication in `ws$state`).
#'
#' @param workspaces A `dock_workspaces` object (from the initial board).
#' @param board,update,triggers Reactive board state, update signal, and
#'   action triggers — forwarded to `manage_dock()`.
#' @param session Shiny session.
#' @param dock_mgr Dock manager created by `new_dock_manager()`.
#'
#' @return A `reactiveValues` with a single slot `$state` — a
#'   `dock_workspaces` object tracking workspace names and which is active.
#'   Layouts are empty; the authoritative layout data lives in
#'   `dock_mgr$docks[[ws_name]]$layout()`.
#'
#' @noRd
init_workspace_docks <- function(workspaces, board, update, triggers,
                                 session, dock_mgr) {
  ns <- session$ns
  active_ws <- active_workspace(workspaces)

  for (ws_name in names(workspaces)) {
    ws_ly <- workspace_layout(workspaces[[ws_name]])
    ws_did <- dock_mgr$next_id()
    dock_output_id <- NS(ns(ws_did), dock_id())

    insertUI(
      selector = paste0("#", ns("ws_container")),
      where = "beforeEnd",
      ui = div(
        id = ns(paste0("ws_wrap_", ws_did)),
        class = paste(
          "blockr-ws-dock",
          if (identical(ws_name, active_ws)) "blockr-ws-dock-active",
          if (is_dock_locked()) "blockr-ws-dock-locked"
        ),
        dockViewR::dock_view_output(
          dock_output_id,
          width = "100%",
          height = "100%"
        )
      ),
      immediate = TRUE,
      session = session
    )

    dock_res <- manage_dock(ws_did, board, update, triggers, layout = ws_ly)
    dock_res$dock_id <- ws_did
    dock_mgr$docks[[ws_name]] <- dock_res
  }

  update_active_dock(dock_mgr$active_dock, dock_mgr$docks[[active_ws]])

  bare <- dock_workspaces(
    set_names(
      lapply(names(workspaces), function(x) dock_workspace()),
      names(workspaces)
    )
  )
  active_workspace(bare) <- active_ws
  reactiveValues(state = bare)
}

#' Observe workspace tab switches.
#'
#' When the user selects a different tab via `input$ws_nav`, hides block/ext
#' UI for the old workspace, shows it for the new one, updates `ws$state`,
#' and swaps `dock_mgr$active_dock` to point at the new workspace's dock.
#'
#' @param ws Reactive workspace state (from `init_workspace_docks()`).
#' @param session Shiny session.
#' @param dock_mgr Dock manager.
#'
#' @noRd
switch_workspace_observer <- function(ws, session, dock_mgr) {
  input <- session$input

  observeEvent(input$ws_nav, {
    new_ws <- input$ws_nav
    state <- ws$state
    old_ws <- active_workspace(state)

    session$sendCustomMessage("switch-workspace", list(
      id = session$ns(
        paste0("ws_wrap_", dock_mgr$docks[[new_ws]]$dock_id)
      )
    ))

    if (!identical(old_ws, new_ws)) {
      hide_workspace_ui(old_ws, dock_mgr$docks)
      show_workspace_ui(new_ws, dock_mgr$docks)

      active_workspace(state) <- new_ws
      ws$state <- state
      update_active_dock(dock_mgr$active_dock, dock_mgr$docks[[new_ws]])
    }
  }, ignoreInit = TRUE)
}

#' Build a reactive `dock_workspaces` with live layouts for serialization.
#'
#' Merges workspace names/active from `ws$state` (source of truth for
#' membership) with live layouts from `dock_mgr$docks` (source of truth
#' for panel arrangement). Called by `serialize_board.dock_board()`.
#'
#' @param ws Reactive workspace state.
#' @param dock_mgr Dock manager.
#'
#' @return A [shiny::reactive()] returning a `dock_workspaces` object.
#'
#' @noRd
live_workspace_data <- function(ws, dock_mgr) {
  reactive({
    state <- ws$state
    ws_list <- lapply(names(state), function(ws_name) {
      dock_workspace(
        layout = as_dock_layout(dock_mgr$docks[[ws_name]]$layout())
      )
    })
    res <- dock_workspaces(set_names(ws_list, names(state)))
    active_workspace(res) <- active_workspace(state)
    res
  })
}

#' Hide all block and extension UI for a workspace.
#'
#' Moves block/extension cards off-screen (back to offcanvas) so they are
#' not visible while the workspace is inactive.
#'
#' @param ws_name Workspace name (string).
#' @param docks Environment of dock module results (`dock_mgr$docks`).
#'
#' @noRd
hide_workspace_ui <- function(ws_name, docks) {
  if (!exists(ws_name, envir = docks, inherits = FALSE)) return()
  proxy <- docks[[ws_name]]$proxy
  bns <- proxy_board_ns(proxy)
  for (bid in as_obj_id(block_panel_ids(proxy))) {
    hide_block_ui(bid, proxy$session, board_ns = bns)
  }
  for (eid in as_obj_id(ext_panel_ids(proxy))) {
    hide_ext_ui(eid, proxy$session, board_ns = bns)
  }
}

#' Show all block and extension UI for a workspace.
#'
#' Restores block/extension cards into the viewport when a workspace
#' becomes active.
#'
#' @param ws_name Workspace name (string).
#' @param docks Environment of dock module results (`dock_mgr$docks`).
#'
#' @noRd
show_workspace_ui <- function(ws_name, docks) {
  if (!exists(ws_name, envir = docks, inherits = FALSE)) return()
  proxy <- docks[[ws_name]]$proxy
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
#' workspace in multi-workspace boards.
#'
#' @param id Module ID.
#' @param board,update Reactive board state and update signal.
#' @param actions Action triggers.
#' @param layout Optional initial `dock_layout`; defaults to the board's
#'   `dock_layout()`.
#'
#' @return A list with `layout` (reactive), `proxy`, `prev_active_group`,
#'   `n_panels`, and `active_group_trail`.
#'
#' @noRd
manage_dock <- function(id, board, update, actions, layout = NULL) {

  # Resolve layout: use provided layout or fall back to board's dock_layout
  init_layout <- layout %||% isolate(dock_layout(board$board))

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
        restore_dock(init_layout, dock)

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

    observeEvent(
      req(n_panels() == 0),
      suggest_panels_to_add(
        dock,
        board,
        actions[["add_block_action"]],
        panels = list(),
        session
      )
    )

    observeEvent(
      input$confirm_add,
      {
        req(input$add_dock_panel)

        pos <- list(
          referenceGroup = input[[dock_input("panel-to-add")]],
          direction = "within"
        )

        for (pid in input$add_dock_panel) {

          if (grepl("^blk-", pid)) {

            show_block_panel(
              board_blocks(board$board)[sub("^blk-", "", pid)],
              add_panel = pos,
              proxy = dock
            )

            n_panels(n_panels() + 1L)

          } else if (grepl("^ext-", pid)) {

            exts <- as.list(dock_extensions(board$board))

            show_ext_panel(
              exts[[sub("^ext-", "", pid)]],
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

          blk <- blks[[blk_id]]
          new_name <- block_name(blk)
          blk_panel_id <- as_block_panel_id(blk_id)

          old_title <- get_dock_panel(blk_panel_id, dock)$title

          if (identical(new_name, old_title)) {
            next
          }

          if (is.null(old_title)) {
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

#' Observe workspace addition requests.
#'
#' Shows a modal to name the new workspace and pick blocks/extensions,
#' then inserts a new dock module, updates `ws$state`, and switches to it.
#'
#' @param ws Reactive workspace state.
#' @param session Shiny session.
#' @param dock_mgr Dock manager.
#' @param board,update,triggers Reactive board state, update signal, and
#'   action triggers — forwarded to `manage_dock()`.
#'
#' @noRd
add_ws_observer <- function(ws, session, dock_mgr, board, update, triggers) {
  input <- session$input
  ns <- session$ns

  # Show modal for workspace creation
  observeEvent(input$ws_nav_add, {
    req(ws_can_crud(ws$state))

    state <- ws$state
    default_name <- paste("Page", length(state) + 1L)

    brd <- board$board
    blk_options <- build_block_options(brd, board_block_ids(brd))
    ext_options <- build_ext_options(brd, dock_ext_ids(brd))

    showModal(
      modalDialog(
        title = "New workspace",
        size = "l",
        easyClose = TRUE,
        footer = NULL,
        tagList(
          css_modal(),
          textInput(
            ns("ws_new_name"),
            "Workspace name",
            value = default_name
          ),
          if (length(blk_options)) {
            tagList(
              css_block_selectize(),
              selectizeInput(
                ns("ws_new_blocks"),
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
              ns("ws_new_exts"),
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
          uiOutput(ns("ws_name_validation")),
          confirm_button(ns("confirm_ws_add"), label = "Create workspace")
        )
      )
    )
  })

  # Name validation feedback
  output <- session$output
  output$ws_name_validation <- renderUI({
    req(input$ws_new_name)
    msg <- validate_ws_name(trimws(input$ws_new_name), names(ws$state))
    if (!is.null(msg)) tags$div(class = "text-danger", msg)
  })

  # Confirm workspace creation
  observeEvent(input$confirm_ws_add, {
    state <- ws$state
    new_name <- trimws(input$ws_new_name)

    if (!is.null(validate_ws_name(new_name, names(state)))) return()

    removeModal()

    # Build layout from selected blocks and extensions
    brd <- board$board
    sel_blks <- intersect(
      input$ws_new_blocks %||% character(),
      board_block_ids(brd)
    )
    sel_exts <- intersect(
      input$ws_new_exts %||% character(),
      dock_ext_ids(brd)
    )
    ws_ly <- create_dock_layout(
      blocks = board_blocks(brd)[sel_blks],
      extensions = as_dock_extensions(
        as.list(dock_extensions(brd))[sel_exts]
      )
    )

    state[[new_name]] <- dock_workspace()
    ws$state <- state

    # Insert dock output container into the DOM
    ws_id <- dock_mgr$next_id()
    dock_output_id <- NS(NS(ns(NULL), ws_id), dock_id())

    insertUI(
      selector = paste0("#", ns("ws_container")),
      where = "beforeEnd",
      ui = div(
        id = ns(paste0("ws_wrap_", ws_id)),
        class = paste(
          "blockr-ws-dock",
          if (is_dock_locked()) "blockr-ws-dock-locked"
        ),
        dockViewR::dock_view_output(
          dock_output_id,
          width = "100%",
          height = "100%"
        )
      )
    )

    # Start the dock module for this workspace
    dock_res <- manage_dock(ws_id, board, update, triggers, layout = ws_ly)
    dock_res$dock_id <- ws_id
    dock_mgr$docks[[new_name]] <- dock_res

    # Update the navigation dropdown and switch to new workspace
    session$sendInputMessage("ws_nav", list(add = new_name))

    # Switch to the new workspace
    session$sendCustomMessage("switch-workspace", list(
      id = ns(paste0("ws_wrap_", ws_id))
    ))
  })
}

#' Observe workspace removal requests.
#'
#' Shows a confirmation modal, then destroys the dock module, removes
#' the DOM container, updates `ws$state`, and switches to another workspace
#' if the removed one was active.
#'
#' @param ws Reactive workspace state.
#' @param session Shiny session.
#' @param dock_mgr Dock manager.
#'
#' @noRd
remove_ws_observer <- function(ws, session, dock_mgr) {
  input <- session$input
  ns <- session$ns

  # Show confirmation modal
  observeEvent(input$ws_nav_remove, {
    req(ws_can_crud(ws$state))

    rm_name <- input$ws_nav_remove
    state <- ws$state

    if (length(state) <= 1L) {
      notify("Cannot remove the last workspace.")
      return()
    }

    showModal(
      modalDialog(
        title = "Remove workspace",
        size = "s",
        easyClose = TRUE,
        footer = NULL,
        tagList(
          tags$p(
            "Are you sure you want to remove workspace ",
            tags$strong(rm_name), "?"
          ),
          div(
            style = "display: flex; justify-content: flex-end; gap: 8px;
              margin-top: 20px;",
            modalButton("Cancel"),
            actionButton(
              ns("confirm_ws_remove"),
              "Remove",
              class = "btn-danger"
            )
          )
        )
      )
    )
  })

  # Perform removal after confirmation
  observeEvent(input$confirm_ws_remove, {
    removeModal()

    rm_name <- input$ws_nav_remove
    state <- ws$state
    was_active <- identical(active_workspace(state), rm_name)

    # Hide block/ext UI from the removed workspace (move back to offcanvas)
    if (exists(rm_name, envir = dock_mgr$docks, inherits = FALSE)) {
      rm_dock <- dock_mgr$docks[[rm_name]]

      if (was_active) {
        hide_workspace_ui(rm_name, dock_mgr$docks)
      }

      # Destroy module observers/inputs/outputs and remove DOM wrapper
      destroy_module(rm_dock$dock_id, session = session)
      removeUI(
        selector = paste0("#", ns(paste0("ws_wrap_", rm_dock$dock_id))),
        immediate = TRUE,
        session = session
      )
      rm(list = rm_name, envir = dock_mgr$docks)
    }

    state[[rm_name]] <- NULL

    if (was_active) {
      active_workspace(state) <- names(state)[1L]
    }

    ws$state <- state

    active_name <- active_workspace(state)

    # switch-workspace must fire before show_workspace_ui so the target
    # dock has blockr-ws-dock-active; otherwise move-element silently
    # drops DOM moves into inactive docks.
    session$sendCustomMessage("switch-workspace", list(
      id = ns(paste0("ws_wrap_", dock_mgr$docks[[active_name]]$dock_id))
    ))

    # Show block/ext UI in the target workspace
    if (was_active) {
      show_workspace_ui(active_name, dock_mgr$docks)
      update_active_dock(dock_mgr$active_dock, dock_mgr$docks[[active_name]])
    }

    # sendInputMessage auto-namespaces, so pass un-namespaced ID
    session$sendInputMessage("ws_nav", list(
      remove = rm_name,
      value = active_name
    ))
  })
}

#' Observe workspace rename requests.
#'
#' Updates the workspace name in both `ws$state` and `dock_mgr$docks`.
#' The internal `dock_id` is unchanged so DOM lookups remain valid.
#'
#' @param ws Reactive workspace state.
#' @param session Shiny session.
#' @param dock_mgr Dock manager.
#'
#' @noRd
rename_ws_observer <- function(ws, session, dock_mgr) {
  input <- session$input

  observeEvent(input$ws_nav_rename, {
    req(ws_can_crud(ws$state))

    rename <- input$ws_nav_rename
    state <- ws$state

    nms <- names(state)
    idx <- match(rename$from, nms)
    if (is.na(idx)) return()

    nms[idx] <- rename$to
    names(state) <- nms

    if (identical(active_workspace(state), rename$from)) {
      active_workspace(state) <- rename$to
    }

    ws$state <- state

    # Update docks key; dock_id inside the entry stays unchanged so
    # DOM wrapper lookups still target the original element.
    if (exists(rename$from, envir = dock_mgr$docks, inherits = FALSE)) {
      dock_mgr$docks[[rename$to]] <- dock_mgr$docks[[rename$from]]
      rm(list = rename$from, envir = dock_mgr$docks)
    }
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
suggest_panels_to_add <- function(dock, board, suggest_new = FALSE,
                                  panels = NULL, session = get_session()) {

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
    build_block_options(board$board, blk_opts, prefix = "blk-"),
    build_ext_options(board$board, ext_opts, prefix = "ext-")
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
  } else {
    notify("No further panels can be added. Remove some panels first.")
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
#' @param prefix String prepended to each value (e.g. `"blk-"`).
#'
#' @return A list of option lists suitable for `selectizeInput`.
#'
#' @noRd
build_block_options <- function(board, blk_ids, prefix = "") {
  if (!length(blk_ids)) return(list())

  blks <- board_blocks(board)[blk_ids]
  meta <- blks_metadata(blks)

  lapply(seq_along(blk_ids), function(i) {
    id <- blk_ids[i]
    build_one_option(
      value = paste0(prefix, id),
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
#' @param prefix String prepended to each value (e.g. `"ext-"`).
#'
#' @return A list of option lists suitable for `selectizeInput`.
#'
#' @noRd
build_ext_options <- function(board, ext_ids, prefix = "") {
  if (!length(ext_ids)) return(list())

  all_exts <- as.list(dock_extensions(board))

  lapply(ext_ids, function(ext_id) {
    ext <- all_exts[[ext_id]]
    ext_name <- extension_name(ext)
    ext_pkg <- ctor_pkg(extension_ctor(ext))

    build_one_option(
      value = paste0(prefix, ext_id),
      label = ext_name,
      id = ext_id,
      package = coal(ext_pkg, "local"),
      icon = extension_default_icon(),
      color = "#999999"
    )
  })
}

#' Validate a workspace name.
#'
#' @param name Trimmed workspace name string.
#' @param existing Character vector of existing workspace names.
#'
#' @return Error message string, or `NULL` if valid.
#'
#' @noRd
validate_ws_name <- function(name, existing) {
  if (nchar(name) == 0L) {
    "Name cannot be empty."
  } else if (!grepl("^[a-zA-Z0-9 _-]+$", name)) {
    "Only letters, numbers, spaces, hyphens and underscores are allowed."
  } else if (name %in% existing) {
    "A workspace with this name already exists."
  } else {
    NULL
  }
}
