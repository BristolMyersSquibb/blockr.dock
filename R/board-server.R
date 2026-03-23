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

  dock <- manage_dock(board, update, triggers, session)

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

manage_dock <- function(board, update, actions, session = get_session()) {

  initial_board <- isolate(board$board)

  if (has_workspaces(initial_board)) {
    return(manage_dock_workspaces(board, update, actions, session))
  }

  dock <- set_dock_view_output(session = session)

  input <- session$input

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
      layout <- dock_layout(board$board)

      restore_dock(layout, dock)

      for (id in as_dock_panel_id(layout)) {
        if (is_block_panel_id(id)) {
          show_block_panel(id, add_panel = FALSE, proxy = dock)
        } else if (is_ext_panel_id(id)) {
          show_ext_panel(id, add_panel = FALSE, proxy = dock)
        } else {
          blockr_abort(
            "Unknown panel type {class(id)}.",
            class = "dock_panel_invalid"
          )
        }
      }
    },
    once = TRUE
  )

  n_panels <- reactiveVal(
    isolate(length(determine_active_views(dock_layout(board$board))))
  )

  observeEvent(
    req(input[[dock_input("n-panels")]]),
    n_panels(input[[dock_input("n-panels")]])
  )

  observeEvent(
    input[[dock_input("panel-to-remove")]],
    {
      id <- as_dock_panel_id(
        input[[dock_input("panel-to-remove")]]
      )

      if (is_block_panel_id(id)) {
        hide_block_panel(id, rm_panel = TRUE, proxy = dock)
        n_panels(n_panels() - 1L)
      } else if (is_ext_panel_id(id)) {
        hide_ext_panel(id, rm_panel = TRUE, proxy = dock)
        n_panels(n_panels() - 1L)
      } else {
        blockr_abort(
          "Unknown panel type {class(id)}.",
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

      for (id in input$add_dock_panel) {

        if (grepl("^blk-", id)) {

          show_block_panel(
            board_blocks(board$board)[sub("^blk-", "", id)],
            add_panel = pos,
            proxy = dock
          )

          n_panels(n_panels() + 1L)

        } else if (grepl("^ext-", id)) {

          exts <- as.list(dock_extensions(board$board))

          show_ext_panel(
            exts[[sub("^ext-", "", id)]],
            add_panel = pos,
            proxy = dock
          )

          n_panels(n_panels() + 1L)

        } else {

          blockr_abort(
            "Unknown panel specification {id}.",
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

      for (id in names(blks)) {

        blk <- blks[[id]]
        new_name <- block_name(blk)
        blk_panel_id <- as_block_panel_id(id)

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
    prev_active_group = prev_active_group
  )
}

manage_dock_workspaces <- function(board, update, actions,
                                   session = get_session()) {

  initial_board <- isolate(board$board)
  workspaces <- dock_workspaces(initial_board)

  # Flatten to leaf workspaces (each gets a DockView)
  leaves <- ws_leaves(workspaces)
  leaf_names <- names(leaves)

  # Build leaf_name -> parent_name map (NULL for top-level leaves)
  leaf_parent <- list()
  for (nm in names(workspaces)) {
    ws <- workspaces[[nm]]
    if (is_ws_parent(ws)) {
      for (cnm in names(ws[["children"]])) {
        leaf_parent[[cnm]] <- nm
      }
    }
    # Top-level leaves have no parent entry (NULL)
  }

  input <- session$input

  active_ws <- reactiveVal(leaf_names[1L])

  # Build block->workspace and ext->workspace maps
  # blocks: block_id -> character vector of leaf workspace names (1:many)
  # exts: ext_id -> character vector of leaf workspace names (1:many)
  block_ws <- list()
  ext_ws <- list()
  for (ws in leaf_names) {
    for (bid in leaves[[ws]][["block_ids"]]) {
      block_ws[[bid]] <- c(block_ws[[bid]], ws)
    }
    for (eid in leaves[[ws]][["ext_ids"]]) {
      ext_ws[[eid]] <- c(ext_ws[[eid]], ws)
    }
  }
  ws_map <- reactiveVal(list(blocks = block_ws, exts = ext_ws))

  proxies <- list()
  ws_prev_active <- list()
  ws_active_trail <- list()

  for (ws in leaf_names) {
    local({
      workspace <- ws
      ws_spec <- leaves[[workspace]]

      proxy <- set_dock_view_output(workspace = workspace, session = session)
      proxies[[workspace]] <<- proxy

      ws_prev_active[[workspace]] <<- reactiveVal()
      ws_active_trail[[workspace]] <<- reactiveVal()

      ws_dinput <- function(x) dock_input(x, workspace = workspace)

      if (get_log_level() >= debug_log_level) {
        observeEvent(
          input[[ws_dinput("active-group")]],
          {
            ag <- input[[ws_dinput("active-group")]]
            log_debug("workspace {workspace}: active group is now {ag}")
          }
        )
      }

      # Initialized: restore layout and show panels
      observeEvent(
        req(input[[ws_dinput("initialized")]], input[["screen_width"]]),
        {
          layout <- ws_spec[["layout"]]

          if (is_dock_locked()) {
            for (nm in names(layout$panels)) {
              layout$panels[[nm]]$tabComponent <- "custom"
              layout$panels[[nm]]$params$removeCallback <- NULL
            }
          }

          screen_width <- input[["screen_width"]]
          if (!is.null(screen_width) && screen_width < 768) {
            collect_views <- function(node) {
              if (node$type == "leaf") return(node$data$views)
              unlist(lapply(node$data, collect_views), recursive = FALSE)
            }
            all_views <- collect_views(layout$grid$root)
            all_views <- as.list(all_views)
            layout$grid$root <- list(
              type = "branch",
              data = list(
                list(
                  type = "leaf",
                  data = list(
                    views = all_views,
                    activeView = all_views[[1]],
                    id = "1"
                  ),
                  size = 1
                )
              ),
              size = 1
            )
          }

          restore_dock(layout, proxy)

          for (id in as_dock_panel_id(layout)) {
            if (is_block_panel_id(id)) {
              show_block_panel(
                id, add_panel = FALSE, proxy = proxy,
                workspace = workspace
              )
            } else if (is_ext_panel_id(id)) {
              show_ext_panel(
                id, add_panel = FALSE, proxy = proxy,
                workspace = workspace,
                reparent = identical(workspace, leaf_names[1L])
              )
            } else {
              blockr_abort(
                "Unknown panel type {class(id)}.",
                class = "dock_panel_invalid"
              )
            }
          }
        },
        once = TRUE
      )

      # Panel remove
      observeEvent(
        input[[ws_dinput("panel-to-remove")]],
        {
          id <- as_dock_panel_id(input[[ws_dinput("panel-to-remove")]])

          if (is_block_panel_id(id)) {
            hide_block_panel(id, rm_panel = TRUE, proxy = proxy)
          } else if (is_ext_panel_id(id)) {
            hide_ext_panel(id, rm_panel = TRUE, proxy = proxy)
          } else {
            blockr_abort(
              "Unknown panel type {class(id)}.",
              class = "dock_panel_invalid"
            )
          }
        }
      )

      # Panel add
      observeEvent(
        input[[ws_dinput("panel-to-add")]],
        suggest_panels_to_add(
          proxy,
          board,
          ws_block_ids = ws_spec[["block_ids"]],
          ws_ext_ids = dock_ext_ids(initial_board),
          session = session
        )
      )

      # N-panels tracking
      n_panels_ws <- reactiveVal(
        isolate(length(determine_active_views(ws_spec[["layout"]])))
      )

      observeEvent(
        req(input[[ws_dinput("n-panels")]]),
        n_panels_ws(input[[ws_dinput("n-panels")]])
      )

      observeEvent(
        req(n_panels_ws() == 0),
        {
          suggest_panels_to_add(
            proxy,
            board,
            actions[["add_block_action"]],
            ws_block_ids = ws_spec[["block_ids"]],
            ws_ext_ids = dock_ext_ids(initial_board),
            session = session
          )
          n_panels_ws(1L)
        }
      )

      # Active group tracking
      observeEvent(
        input[[ws_dinput("active-group")]],
        {
          cur_ag <- input[[ws_dinput("active-group")]]
          pre_ag <- ws_active_trail[[workspace]]()
          if (!identical(pre_ag, cur_ag)) {
            log_trace("workspace {workspace}: prev active group to {pre_ag}")
            ws_prev_active[[workspace]](pre_ag)
          }
          ws_active_trail[[workspace]](cur_ag)
        }
      )
    })
  }

  all_ext_ids <- dock_ext_ids(initial_board)

  switch_ws <- function(ws) {
    old_ws <- active_ws()
    if (identical(ws, old_ws)) return(invisible())

    # Hide all extension UIs (move to offcanvas)
    for (eid in all_ext_ids) {
      hide_ext_ui(eid, session)
    }

    # Hide multi-workspace block UIs (move to offcanvas)
    wm <- ws_map()
    multi_ws_bids <- names(Filter(
      function(ws_vec) length(ws_vec) > 1L,
      wm$blocks
    ))
    for (bid in multi_ws_bids) {
      hide_block_ui(bid, session)
    }

    active_ws(ws)

    session$sendCustomMessage(
      "switch-workspace",
      list(active = ws, parent = leaf_parent[[ws]])
    )

    # Reparent extensions into new workspace based on ws_map membership
    # (dock_panel_ids is unreliable — dockViewR JS state can lose track
    # of extension panels after all block panels are closed)
    ws_ext_ids <- names(Filter(
      function(ws_vec) ws %in% ws_vec,
      wm$exts
    ))
    for (eid in ws_ext_ids) {
      show_ext_ui(eid, session, workspace = ws)
    }

    # Reparent multi-workspace blocks based on ws_map membership
    for (bid in multi_ws_bids) {
      if (ws %in% wm$blocks[[bid]]) {
        show_block_ui(bid, session, workspace = ws)
      }
    }
  }

  # Active workspace observer
  observeEvent(
    input[["active_workspace"]],
    {
      ws <- input[["active_workspace"]]
      req(ws %in% leaf_names)
      switch_ws(ws)
    }
  )

  # Detach input — route to active workspace
  observeEvent(
    input$detach_input,
    {
      block_id <- input$detach_input$block_id
      req(block_id, block_id %in% board_block_ids(board$board))

      workspace <- active_ws()
      proxy <- proxies[[workspace]]

      blk <- board_blocks(board$board)[[block_id]]
      blk_name <- block_name(blk)
      bpid <- as_block_panel_id(block_id)

      show_block_input_panel(
        block_id,
        position = list(referencePanel = bpid, direction = "left"),
        proxy = proxy,
        block_name = blk_name,
        session = session,
        workspace = workspace
      )
    }
  )

  # Confirm add — route to active workspace
  observeEvent(
    input$confirm_add,
    {
      req(input$add_dock_panel)

      ws <- active_ws()
      proxy <- proxies[[ws]]

      pos <- list(
        referenceGroup = input[[dock_input("panel-to-add", workspace = ws)]],
        direction = "within"
      )

      for (id in input$add_dock_panel) {
        if (grepl("^blk-", id)) {
          show_block_panel(
            board_blocks(board$board)[sub("^blk-", "", id)],
            add_panel = pos,
            proxy = proxy,
            workspace = ws
          )
        } else if (grepl("^ext-", id)) {
          exts <- as.list(dock_extensions(board$board))
          show_ext_panel(
            exts[[sub("^ext-", "", id)]],
            add_panel = pos,
            proxy = proxy,
            workspace = ws
          )
        } else {
          blockr_abort(
            "Unknown panel specification {id}.",
            class = "dock_panel_invalid"
          )
        }
      }

      removeModal()
    }
  )

  # Block title updates — route to all workspaces the block belongs to
  observeEvent(
    update()$blocks$mod,
    {
      blks <- update()$blocks$mod
      wm <- ws_map()

      for (id in names(blks)) {
        blk <- blks[[id]]
        new_name <- block_name(blk)
        blk_panel_id <- as_block_panel_id(id)

        block_workspaces <- wm$blocks[[id]]
        if (is.null(block_workspaces)) next

        for (workspace in block_workspaces) {
          proxy <- proxies[[workspace]]
          old_title <- get_dock_panel(blk_panel_id, proxy)$title
          if (is.null(old_title) || new_name == old_title) next

          log_debug("setting panel title {blk_panel_id} to '{new_name}'")
          dockViewR::set_panel_title(proxy, blk_panel_id, new_name)
        }
      }
    }
  )

  list(
    layout = reactive(dockViewR::get_dock(proxies[[active_ws()]])),
    proxy = proxies[[leaf_names[1L]]],
    proxies = proxies,
    active_ws = active_ws,
    ws_map = ws_map,
    switch_workspace = switch_ws,
    prev_active_group = reactive(ws_prev_active[[active_ws()]]())
  )
}

suggest_panels_to_add <- function(dock, board, suggest_new = FALSE,
                                  panels = NULL, session = get_session(),
                                  ws_block_ids = NULL, ws_ext_ids = NULL) {

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
  all_blk_ids <- ws_block_ids %||% board_block_ids(board$board)
  blk_opts <- setdiff(
    all_blk_ids,
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
  all_ext_ids <- ws_ext_ids %||% dock_ext_ids(board$board)
  ext_opts <- setdiff(
    all_ext_ids,
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
