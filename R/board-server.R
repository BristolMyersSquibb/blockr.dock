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
    input <- session$input

    # Mutable workspace state (board$board is read-only)
    ws_state <- reactiveVal(workspaces)

    # Environment (not list) so mutations in sub-functions are visible here
    docks <- new.env(parent = emptyenv())
    for (ws_name in names(workspaces)) {
      ws_ly <- workspace_layout(workspaces[[ws_name]])
      ws_did <- ws_dock_id(ws_name)
      dock_res <- manage_dock(ws_did, board, update, triggers, layout = ws_ly)
      dock_res$dock_id <- ws_did
      docks[[ws_name]] <- dock_res
    }

    # Workspace switching
    observeEvent(input$ws_nav, {
      new_ws <- input$ws_nav
      ws <- ws_state()
      old_ws <- active_workspace(ws)

      if (!identical(old_ws, new_ws)) {
        # Move elements from old workspace back to offcanvas
        if (exists(old_ws, envir = docks, inherits = FALSE)) {
          old_proxy <- docks[[old_ws]]$proxy
          old_bns <- proxy_board_ns(old_proxy)
          for (bid in as_obj_id(block_panel_ids(old_proxy))) {
            hide_block_ui(bid, old_proxy$session, board_ns = old_bns)
          }
          for (eid in as_obj_id(ext_panel_ids(old_proxy))) {
            hide_ext_ui(eid, old_proxy$session, board_ns = old_bns)
          }
        }

        # Move elements into new workspace panels
        if (exists(new_ws, envir = docks, inherits = FALSE)) {
          new_proxy <- docks[[new_ws]]$proxy
          new_bns <- proxy_board_ns(new_proxy)
          for (bid in as_obj_id(block_panel_ids(new_proxy))) {
            show_block_ui(bid, new_proxy$session, board_ns = new_bns)
          }
          for (eid in as_obj_id(ext_panel_ids(new_proxy))) {
            show_ext_ui(eid, new_proxy$session, board_ns = new_bns)
          }
        }

        # Update active workspace and switch the dock used by extensions
        active_workspace(ws) <- new_ws
        ws_state(ws)
        update_active_dock(active_dock, docks[[new_ws]])
      }

      session$sendCustomMessage("switch-workspace", list(
        id = session$ns(paste0("ws_wrap_", docks[[new_ws]]$dock_id))
      ))
    }, ignoreInit = TRUE)

    # Workspace CRUD (only when unlocked)
    add_ws_observer(session, board, ws_state, update, triggers, docks)
    remove_ws_observer(session, ws_state, docks)
    rename_ws_observer(session, ws_state, docks)

    # Reactive dock that always points to the active workspace's dock
    active_dock <- reactiveValues()
    update_active_dock(active_dock, docks[[active_workspace(workspaces)]])
    dock <- active_dock

  } else {
    dock <- manage_dock("dock_main", board, update, triggers)
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
    ext_res
  )
}

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
          ag <- input[[dock_input("active-group")]] # nolint: object_usage_linter.
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

          if (new_name == old_title) {
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

add_ws_observer <- function(session, board, ws_state, update, triggers, docks) {
  input <- session$input
  ns <- session$ns

  # Show modal for workspace creation
  observeEvent(input$ws_nav_add, {
    req(ws_can_crud(ws_state()))

    ws <- ws_state()
    default_name <- paste("Page", length(ws) + 1L)

    # Build block options
    blk_ids <- board_block_ids(board$board)
    blk_options <- list()

    if (length(blk_ids)) {
      blks <- board_blocks(board$board)[blk_ids]
      meta <- blks_metadata(blks)

      for (i in seq_along(blk_ids)) {
        id <- blk_ids[i]
        blk_options[[length(blk_options) + 1L]] <- list(
          value = id,
          label = block_name(blks[[id]]),
          description = paste0("ID: ", id),
          package = meta$package[i],
          icon = meta$icon[i],
          color = meta$color[i],
          searchtext = paste(block_name(blks[[id]]), id, meta$package[i])
        )
      }
    }

    # Build extension options
    ext_ids <- dock_ext_ids(board$board)
    ext_options <- list()

    if (length(ext_ids)) {
      all_exts <- as.list(dock_extensions(board$board))

      for (ext_id in ext_ids) {
        ext <- all_exts[[ext_id]]
        ext_name <- extension_name(ext)
        ext_pkg <- ctor_pkg(extension_ctor(ext))

        ext_options[[length(ext_options) + 1L]] <- list(
          value = ext_id,
          label = ext_name,
          description = paste0("ID: ", ext_id),
          package = coal(ext_pkg, "local"),
          icon = extension_default_icon(),
          color = "#999999",
          searchtext = paste(ext_name, ext_id, ext_pkg)
        )
      }
    }

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
    ws <- ws_state()
    name <- trimws(input$ws_new_name)

    if (nchar(name) == 0L) {
      tags$div(class = "text-danger", "Name cannot be empty.")
    } else if (name %in% names(ws)) {
      tags$div(class = "text-danger", "A workspace with this name already exists.")
    } else {
      NULL
    }
  })

  # Confirm workspace creation
  observeEvent(input$confirm_ws_add, {
    ws <- ws_state()
    new_name <- trimws(input$ws_new_name)

    # Validate
    if (nchar(new_name) == 0L || new_name %in% names(ws)) return()

    removeModal()

    # Build layout from selected blocks and extensions
    selected_ids <- c(input$ws_new_blocks, input$ws_new_exts)
    ws_ly <- create_dock_layout(
      blocks = board_blocks(board$board)[
        intersect(input$ws_new_blocks %||% character(), board_block_ids(board$board))
      ],
      extensions = as_dock_extensions(
        as.list(dock_extensions(board$board))[
          intersect(input$ws_new_exts %||% character(), dock_ext_ids(board$board))
        ]
      )
    )

    ws[[new_name]] <- dock_workspace(layout = ws_ly)
    ws_state(ws)

    # Insert dock output container into the DOM
    ws_id <- ws_dock_id(new_name)
    dock_output_id <- NS(NS(ns(NULL), ws_id), dock_id())

    insertUI(
      selector = paste0("#", ns("ws_container")),
      where = "beforeEnd",
      ui = div(
        id = ns(paste0("ws_wrap_", ws_id)),
        class = "blockr-ws-dock",
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
    docks[[new_name]] <- dock_res

    # Update the navigation dropdown and switch to new workspace
    session$sendInputMessage("ws_nav", list(add = new_name))

    # Switch to the new workspace
    session$sendCustomMessage("switch-workspace", list(
      id = ns(paste0("ws_wrap_", ws_id))
    ))
  })
}

remove_ws_observer <- function(session, ws_state, docks) {
  input <- session$input
  ns <- session$ns

  # Show confirmation modal
  observeEvent(input$ws_nav_remove, {
    req(ws_can_crud(ws_state()))

    rm_name <- input$ws_nav_remove
    ws <- ws_state()

    if (length(ws) <= 1L) {
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
    ws <- ws_state()

    ws[[rm_name]] <- NULL

    if (identical(active_workspace(ws), rm_name)) {
      active_workspace(ws) <- names(ws)[1L]
    }

    ws_state(ws)

    # sendInputMessage auto-namespaces, so pass un-namespaced ID
    session$sendInputMessage("ws_nav", list(
      remove = rm_name,
      value = active_workspace(ws)
    ))

    active_name <- active_workspace(ws)
    session$sendCustomMessage("switch-workspace", list(
      id = ns(paste0("ws_wrap_", docks[[active_name]]$dock_id))
    ))
  })
}

rename_ws_observer <- function(session, ws_state, docks) {
  input <- session$input

  observeEvent(input$ws_nav_rename, {
    req(ws_can_crud(ws_state()))

    rename <- input$ws_nav_rename
    ws <- ws_state()

    nms <- names(ws)
    idx <- match(rename$from, nms)
    if (is.na(idx)) return()

    nms[idx] <- rename$to
    names(ws) <- nms

    if (identical(active_workspace(ws), rename$from)) {
      active_workspace(ws) <- rename$to
    }

    ws_state(ws)

    # Update docks key; dock_id inside the entry stays unchanged so
    # DOM wrapper lookups still target the original element.
    # docks is an environment (reference semantics), so plain <- suffices.
    if (exists(rename$from, envir = docks, inherits = FALSE)) {
      docks[[rename$to]] <- docks[[rename$from]]
      rm(list = rename$from, envir = docks)
    }
  })
}

suggest_panels_to_add <- function(dock, board, suggest_new = FALSE,
                                  panels = NULL, session = get_session()) {

  ns <- session$ns

  if (is.null(panels)) {
    panels <- dock_panel_ids(dock)
    if (length(panels) == 0L) {
      panels <- list()
    } else if (length(panels) == 1L) {
      panels <- list(panels)
    }
  }

  stopifnot(is.list(panels), all(lgl_ply(panels, is_dock_panel_id)))

  options_data <- list()

  # Get available blocks
  blk_opts <- setdiff(
    board_block_ids(board$board),
    as_obj_id(panels[lgl_ply(panels, is_block_panel_id)])
  )

  if (length(blk_opts)) {
    blks <- board_blocks(board$board)[blk_opts]
    meta <- blks_metadata(blks)

    for (i in seq_along(blk_opts)) {
      id <- blk_opts[i]
      options_data[[length(options_data) + 1L]] <- list(
        value = paste0("blk-", id),
        label = block_name(blks[[id]]),
        description = paste0("ID: ", id),
        package = meta$package[i],
        icon = meta$icon[i],
        color = meta$color[i],
        searchtext = paste(block_name(blks[[id]]), id, meta$package[i])
      )
    }
  }

  # Get available extensions
  ext_opts <- setdiff(
    dock_ext_ids(board$board),
    as_obj_id(panels[lgl_ply(panels, is_ext_panel_id)])
  )

  if (length(ext_opts)) {
    all_exts <- as.list(dock_extensions(board$board))

    for (ext_id in ext_opts) {
      ext <- all_exts[[ext_id]]
      ext_name <- extension_name(ext)
      ext_pkg <- ctor_pkg(extension_ctor(ext))

      options_data[[length(options_data) + 1L]] <- list(
        value = paste0("ext-", ext_id),
        label = ext_name,
        description = paste0("ID: ", ext_id),
        package = coal(ext_pkg, "local"),
        icon = extension_default_icon(),
        color = "#999999",
        searchtext = paste(ext_name, ext_id, ext_pkg)
      )
    }
  }

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

extension_default_icon <- function() {
  as.character(bsicons::bs_icon("gear"))
}
