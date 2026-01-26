board_server_callback <- function(board, update, ..., session = get_session()) {

  initial_board <- isolate(board$board)

  # Initialize panel system
  init_sidebar(session)
  sidebar_server(board, update, session)

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
    {
      panels <- dock_panel_ids(dock)
      visible_block_ids <- character()
      visible_ext_ids <- character()

      if (length(panels) > 0) {
        if (length(panels) == 1) panels <- list(panels)
        for (p in panels) {
          if (is_block_panel_id(p)) {
            visible_block_ids <- c(visible_block_ids, as_obj_id(p))
          } else if (is_ext_panel_id(p)) {
            visible_ext_ids <- c(visible_ext_ids, as_obj_id(p))
          }
        }
      }

      show_sidebar(new_sidebar("add_panel", context = list(
        reference_group = input[[dock_input("panel-to-add")]],
        visible_block_ids = visible_block_ids,
        visible_ext_ids = visible_ext_ids
      )))
    }
  )

  n_panels <- reactiveVal(
    isolate(length(determine_active_views(dock_layout(board$board))))
  )


  observeEvent(
    req(input[[dock_input("n-panels")]]),
    n_panels(input[[dock_input("n-panels")]])
  )

  observeEvent(
    req(n_panels() == 0),
    {
      # Show sidebar for adding panels (instead of modal)
      show_sidebar(new_sidebar("add_panel", context = list(
        reference_group = NULL,
        visible_block_ids = character(),
        visible_ext_ids = character()
      )))
      n_panels(1L)
    }
  )

  # Handle dock panel card click from sidebar
  observeEvent(
    input$dock_panel_click,
    {
      req(input$dock_panel_click$id)

      panel <- get_sidebar(session)
      reference_group <- panel$context$reference_group

      pos <- list(
        referenceGroup = reference_group,
        direction = "within"
      )

      id <- input$dock_panel_click$id

      if (grepl("^blk-", id)) {
        show_block_panel(
          board_blocks(board$board)[sub("^blk-", "", id)],
          add_panel = pos,
          proxy = dock
        )
      } else if (grepl("^ext-", id)) {
        exts <- as.list(dock_extensions(board$board))

        show_ext_panel(
          exts[[sub("^ext-", "", id)]],
          add_panel = pos,
          proxy = dock
        )
      } else {
        blockr_abort(
          "Unknown panel specification {id}.",
          class = "dock_panel_invalid"
        )
      }

      hide_sidebar()
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

extension_default_icon <- function() {
  as.character(bsicons::bs_icon("gear"))
}
