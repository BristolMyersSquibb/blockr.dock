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

    docks <- list()
    for (ws_name in names(workspaces)) {
      docks[[ws_name]] <- manage_dock(
        ws_dock_id(ws_name), board, update, triggers, ws_name = ws_name
      )
    }

    # Workspace switching
    observeEvent(input$ws_nav, {
      session$sendCustomMessage("switch-workspace", list(
        id = session$ns(paste0("ws_wrap_", ws_dock_id(input$ws_nav)))
      ))
    })

    # Workspace CRUD (only when unlocked)
    add_ws_observer(session, board, ws_state, update, triggers, docks)
    remove_ws_observer(session, ws_state)
    rename_ws_observer(session, ws_state)

    # Use active workspace's dock for extensions
    dock <- docks[[active_workspace(workspaces)]]

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

manage_dock <- function(id, board, update, actions, ws_name = NULL) {

  moduleServer(id, function(input, output, session) {

    dock <- set_dock_view_output(session = session)

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
        layout <- ws_layout(board, ws_name)

        restore_dock(layout, dock)

        for (pid in as_dock_panel_id(layout)) {
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
      isolate(length(determine_active_views(ws_layout(board, ws_name))))
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

# Get workspace-specific or global layout from the board
ws_layout <- function(board, ws_name = NULL) {
  if (!is.null(ws_name)) {
    workspace_layout(board$board[["layout"]][[ws_name]])
  } else {
    dock_layout(board$board)
  }
}

add_ws_observer <- function(session, board, ws_state, update, triggers, docks) {
  input <- session$input

  observeEvent(input$ws_nav_add, {
    req(ws_can_crud(ws_state()))

    ws <- ws_state()
    new_name <- paste("Page", length(ws) + 1L)
    ws[[new_name]] <- dock_workspace()
    ws_state(ws)

    docks[[new_name]] <<- manage_dock(
      ws_dock_id(new_name), board, update, triggers, ws_name = new_name
    )

    # sendInputMessage auto-namespaces, so pass un-namespaced ID
    session$sendInputMessage("ws_nav", list(add = new_name))
  })
}

remove_ws_observer <- function(session, ws_state) {
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

    session$sendCustomMessage("switch-workspace", list(
      id = ns(paste0("ws_wrap_", ws_dock_id(active_workspace(ws))))
    ))
  })
}

rename_ws_observer <- function(session, ws_state) {
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
