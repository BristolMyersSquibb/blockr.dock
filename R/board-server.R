board_server_callback <- function(board, update, ..., session = get_session()) {

  dock <- manage_dock(board, session)

  exts <- isolate(
    dock_extensions(board$board)
  )

  intercom <- set_names(
    replicate(length(exts), reactiveVal()),
    exts
  )

  ext_state <- set_names(
    vector("list", length(exts)),
    names(exts)
  )

  for (i in names(exts)) {
    ext_state[[i]] <- extension_server(
      exts[[i]],
      list(board = board, update = update, dock = dock),
      intercom,
      list(...)
    )
  }

  c(
    list(dock = dock),
    ext_state
  )
}

manage_dock <- function(board, session = get_session()) {

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

  observeEvent(
    input[[dock_input("panel-to-remove")]],
    {
      id <- as_dock_panel_id(
        input[[dock_input("panel-to-remove")]]
      )

      if (is_block_panel_id(id)) {
        hide_block_panel(id, rm_panel = TRUE, proxy = dock)
      } else if (is_ext_panel_id(id)) {
        hide_ext_panel(id, rm_panel = TRUE, proxy = dock)
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
    suggest_panels_to_add(dock, board, session)
  )

  observeEvent(
    req(input[[dock_input("n-panels")]] == 0),
    suggest_panels_to_add(dock, board, session)
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
            sub("^blk-", "", id),
            add_panel = pos,
            proxy = dock
          )
        } else if (grepl("^ext-", id)) {
          show_ext_panel(
            dock_extensions(board$board)[[sub("^ext-", "", id)]],
            add_panel = pos,
            proxy = dock
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

  observeEvent(
    input$cancel_add,
    removeModal()
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

  list(
    layout = reactive(dockViewR::get_dock(dock)),
    proxy = dock,
    prev_active_group = prev_active_group
  )
}


suggest_panels_to_add <- function(dock, board, session) {

  ns <- session$ns

  panels <- dock_panel_ids(dock)

  if (length(panels) == 0L) {
    panels <- list()
  } else if (length(panels) == 1L) {
    panels <- list(panels)
  }

  stopifnot(is.list(panels), all(lgl_ply(panels, is_dock_panel_id)))

  opts <- list()

  blk_opts <- setdiff(
    board_block_ids(board$board),
    as_obj_id(panels[lgl_ply(panels, is_block_panel_id)])
  )

  if (length(blk_opts)) {
    blk_opts <- set_names(
      paste0("blk-", blk_opts),
      paste0(
        chr_ply(board_blocks(board$board)[blk_opts], block_name),
        " (",
        blk_opts,
        ")"
      )
    )

    opts <- c(opts, list(Blocks = blk_opts))
  }

  ext_opts <- setdiff(
    dock_ext_ids(board$board),
    as_obj_id(panels[lgl_ply(panels, is_ext_panel_id)])
  )

  if (length(ext_opts)) {

    ext_opts <- set_names(
      paste0("ext-", ext_opts),
      paste0(
        chr_ply(dock_extensions(board$board)[ext_opts], extension_name),
        " (",
        ext_opts,
        ")"
      )
    )

    opts <- c(opts, list(Extensions = ext_opts))
  }

  if (length(opts)) {
    showModal(
      modalDialog(
        title = "Select panel to add",
        easy_close = TRUE,
        selectInput(
          ns("add_dock_panel"),
          label = "Panel",
          choices = opts,
          multiple = TRUE
        ),
        footer = tagList(
          actionButton(
            ns("cancel_add"),
            label = "Cancel",
            class = "btn-danger"
          ),
          actionButton(
            ns("confirm_add"),
            label = "OK",
            class = "btn-success"
          )
        )
      )
    )
  } else {
    notify("No further panels can be added. Remove some panels first.")
  }
}
