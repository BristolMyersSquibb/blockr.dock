# =============================================================================
# Sidebar Server
# =============================================================================

#' Sidebar Server Logic
#'
#' Sets up the server-side logic for sidebars including content rendering
#' and card click handlers.
#'
#' @param board Reactive board state
#' @param update Update function for board mutations
#' @param session Shiny session
#'
#' @keywords internal
sidebar_server <- function(board, update,
                           session = shiny::getDefaultReactiveDomain()) {

  ns <- session$ns
  input <- session$input
  output <- session$output
  sidebar <- get_sidebar(session)

  # ==========================================================================
  # Debounced Search Inputs
  # ==========================================================================

  add_block_search <- reactive(input$add_block_search) |> debounce(150)
  append_block_search <- reactive(input$append_block_search) |> debounce(150)
  prepend_block_search <- reactive(input$prepend_block_search) |> debounce(150)
  add_link_search <- reactive(input$add_link_search) |> debounce(150)
  add_panel_search <- reactive(input$add_panel_search) |> debounce(150)
  create_stack_search <- reactive(input$create_stack_search) |> debounce(150)
  edit_stack_search <- reactive(input$edit_stack_search) |> debounce(150)

  # ==========================================================================
  # Block Sidebar Content Renderers
  # ==========================================================================

  output$add_block_content <- renderUI({
    req(identical(sidebar$sidebar_id, "add_block"))
    render_block_sidebar_content(
      ns = ns,
      board = board$board,
      mode = "add",
      search_term = add_block_search()
    )
  })

  output$append_block_content <- renderUI({
    req(identical(sidebar$sidebar_id, "append_block"))
    render_block_sidebar_content(
      ns = ns,
      board = board$board,
      mode = "append",
      search_term = append_block_search()
    )
  })

  output$prepend_block_content <- renderUI({
    req(identical(sidebar$sidebar_id, "prepend_block"))
    render_block_sidebar_content(
      ns = ns,
      board = board$board,
      mode = "prepend",
      search_term = prepend_block_search()
    )
  })

  # ==========================================================================
  # Link Sidebar Content Renderer
  # ==========================================================================

  output$add_link_content <- renderUI({
    req(identical(sidebar$sidebar_id, "add_link"))
    source_block <- sidebar$context$source_block
    req(source_block)
    render_link_sidebar_content(
      ns = ns,
      board = board$board,
      source_block = source_block,
      search_term = add_link_search()
    )
  })

  # ==========================================================================
  # Stack Sidebar Content Renderers
  # ==========================================================================

  output$create_stack_content <- renderUI({
    req(identical(sidebar$sidebar_id, "create_stack"))
    render_stack_sidebar_content(
      ns = ns,
      board = board$board,
      mode = "create",
      search_term = create_stack_search(),
      current_selection = input$stack_block_selection
    )
  })

  output$edit_stack_content <- renderUI({
    req(identical(sidebar$sidebar_id, "edit_stack"))
    stack_id <- sidebar$context$stack_id
    req(stack_id)
    stack <- board_stacks(board$board)[[stack_id]]
    render_stack_sidebar_content(
      ns = ns,
      board = board$board,
      mode = "edit",
      stack = stack,
      stack_id = stack_id,
      search_term = edit_stack_search(),
      current_selection = input$edit_stack_blocks
    )
  })

  # Stack Sidebar Footer Renderers
  output$create_stack_footer <- renderUI({
    req(identical(sidebar$sidebar_id, "create_stack"))
    render_stack_sidebar_footer(
      ns = ns,
      board = board$board,
      mode = "create"
    )
  })

  output$edit_stack_footer <- renderUI({
    req(identical(sidebar$sidebar_id, "edit_stack"))
    stack_id <- sidebar$context$stack_id
    req(stack_id)
    stack <- board_stacks(board$board)[[stack_id]]
    render_stack_sidebar_footer(
      ns = ns,
      board = board$board,
      mode = "edit",
      stack = stack,
      stack_id = stack_id
    )
  })

  # ==========================================================================
  # Dock Sidebar (Add Panel) Content Renderer
  # ==========================================================================

  output$add_panel_content <- renderUI({
    req(identical(sidebar$sidebar_id, "add_panel"))
    render_dock_sidebar_content(
      ns = ns,
      board = board$board,
      visible_block_ids = sidebar$context$visible_block_ids %||% character(),
      visible_ext_ids = sidebar$context$visible_ext_ids %||% character(),
      search_term = add_panel_search()
    )
  })

  # ==========================================================================
  # Block Card Click Handlers
  # ==========================================================================
  # These handle clicks from sidebar block cards. They live here (not in

  # actions) because the sidebar content uses the board's namespace, not
  # the nested action module namespace.

  # Add block: card click (instant add with auto-generated ID)
  observeEvent(
    input$add_block_card_click,
    {
      req(input$add_block_card_click$ctor)

      new_blk <- create_block_with_name(
        input$add_block_card_click$ctor,
        chr_ply(board_blocks(board$board), block_name)
      )

      id <- rand_names(board_block_ids(board$board))
      bk <- as_blocks(set_names(list(new_blk), id))
      update(list(blocks = list(add = bk)))
      hide_sidebar()
    }
  )

  # Add block: accordion confirm (with optional custom name/ID)
  observeEvent(
    input$add_block_card_confirm,
    {
      req(input$add_block_card_confirm$ctor)
      ctor <- input$add_block_card_confirm$ctor

      # Get custom name and ID from accordion inputs
      custom_name <- input[[paste0("block_name_", ctor)]]
      custom_id <- input[[paste0("block_id_", ctor)]]

      new_blk <- create_block_with_name(
        ctor,
        chr_ply(board_blocks(board$board), block_name)
      )

      # Apply custom name if provided
      if (!is.null(custom_name) && nzchar(custom_name)) {
        block_name(new_blk) <- custom_name
      }

      # Use custom ID or auto-generate
      id <- if (!is.null(custom_id) && nzchar(custom_id)) {
        if (custom_id %in% board_block_ids(board$board)) {
          notify(
            "Block ID already exists, generating new one.", type = "warning"
          )
          rand_names(board_block_ids(board$board))
        } else {
          custom_id
        }
      } else {
        rand_names(board_block_ids(board$board))
      }

      bk <- as_blocks(set_names(list(new_blk), id))
      update(list(blocks = list(add = bk)))
      hide_sidebar()
    }
  )

  # Append block: card click (instant add with auto-generated ID + link)
  observeEvent(
    input$append_block_card_click,
    {
      req(input$append_block_card_click$ctor)
      # When pinned and first action done, add standalone blocks (no links)
      if (isTRUE(sidebar$is_pinned) && isTRUE(sidebar$pinned_first_action_done)) {
        new_blk <- create_block_with_name(
          input$append_block_card_click$ctor,
          chr_ply(board_blocks(board$board), block_name)
        )
        id <- rand_names(board_block_ids(board$board))
        bk <- as_blocks(set_names(list(new_blk), id))
        update(list(blocks = list(add = bk)))
        return()
      }

      source_block <- sidebar$context$source_block
      req(source_block)

      new_blk <- create_block_with_name(
        input$append_block_card_click$ctor,
        chr_ply(board_blocks(board$board), block_name)
      )

      # Get inputs for the new block (all available since it's new)
      avail_input <- block_inputs(new_blk)

      if (length(avail_input) == 0) {
        notify(
          "No inputs are available for the selected block.", type = "warning"
        )
        return()
      }

      blk_id <- rand_names(board_block_ids(board$board))
      lnk_id <- rand_names(board_link_ids(board$board))

      new_blk <- as_blocks(set_names(list(new_blk), blk_id))
      new_lnk <- new_link(
        from = source_block,
        to = blk_id,
        input = avail_input[1]
      )
      new_lnk <- as_links(set_names(list(new_lnk), lnk_id))

      update(list(
        blocks = list(add = new_blk),
        links = list(add = new_lnk)
      ))

      # Mark first action done for pinned mode
      if (isTRUE(sidebar$is_pinned)) {
        sidebar$pinned_first_action_done <- TRUE
      }
      hide_sidebar()
    }
  )

  # Append block: accordion confirm (with optional custom name/ID/input/link ID)
  observeEvent(
    input$append_block_card_confirm,
    {
      req(input$append_block_card_confirm$ctor)

      ctor <- input$append_block_card_confirm$ctor
      custom_name <- input[[paste0("block_name_", ctor)]]
      custom_id <- input[[paste0("block_id_", ctor)]]

      # When pinned and first action done, add standalone block (no link)
      if (isTRUE(sidebar$is_pinned) && isTRUE(sidebar$pinned_first_action_done)) {
        new_blk <- create_block_with_name(
          ctor,
          chr_ply(board_blocks(board$board), block_name)
        )
        if (!is.null(custom_name) && nzchar(custom_name)) {
          block_name(new_blk) <- custom_name
        }
        id <- if (!is.null(custom_id) && nzchar(custom_id)) {
          if (custom_id %in% board_block_ids(board$board)) {
            notify("Block ID already exists, generating new one.", type = "warning")
            rand_names(board_block_ids(board$board))
          } else {
            custom_id
          }
        } else {
          rand_names(board_block_ids(board$board))
        }
        bk <- as_blocks(set_names(list(new_blk), id))
        update(list(blocks = list(add = bk)))
        return()
      }

      source_block <- sidebar$context$source_block
      req(source_block)
      custom_input <- input[[paste0("block_input_", ctor)]]
      custom_link_id <- input[[paste0("link_id_", ctor)]]

      new_blk <- create_block_with_name(
        ctor,
        chr_ply(board_blocks(board$board), block_name)
      )

      if (!is.null(custom_name) && nzchar(custom_name)) {
        block_name(new_blk) <- custom_name
      }

      avail_input <- block_inputs(new_blk)

      if (length(avail_input) == 0) {
        notify(
          "No inputs are available for the selected block.", type = "warning"
        )
        return()
      }

      # Use custom input if provided, otherwise first available
      use_custom <- !is.null(custom_input) && custom_input %in% avail_input
      input_name <- if (use_custom) {
        custom_input
      } else {
        avail_input[1]
      }

      blk_id <- if (!is.null(custom_id) && nzchar(custom_id)) {
        if (custom_id %in% board_block_ids(board$board)) {
          notify(
            "Block ID already exists, generating new one.", type = "warning"
          )
          rand_names(board_block_ids(board$board))
        } else {
          custom_id
        }
      } else {
        rand_names(board_block_ids(board$board))
      }

      lnk_id <- if (!is.null(custom_link_id) && nzchar(custom_link_id)) {
        if (custom_link_id %in% board_link_ids(board$board)) {
          notify(
            "Link ID already exists, generating new one.", type = "warning"
          )
          rand_names(board_link_ids(board$board))
        } else {
          custom_link_id
        }
      } else {
        rand_names(board_link_ids(board$board))
      }

      new_blk <- as_blocks(set_names(list(new_blk), blk_id))
      new_lnk <- new_link(
        from = source_block,
        to = blk_id,
        input = input_name
      )
      new_lnk <- as_links(set_names(list(new_lnk), lnk_id))

      update(list(
        blocks = list(add = new_blk),
        links = list(add = new_lnk)
      ))

      # Mark first action done for pinned mode
      if (isTRUE(sidebar$is_pinned)) {
        sidebar$pinned_first_action_done <- TRUE
      }
      hide_sidebar()
    }
  )

  # Prepend block: card click (instant add with auto-generated ID + link)
  observeEvent(
    input$prepend_block_card_click,
    {
      req(input$prepend_block_card_click$ctor)

      # When pinned and first action done, add standalone block (no link)
      if (isTRUE(sidebar$is_pinned) && isTRUE(sidebar$pinned_first_action_done)) {
        new_blk <- create_block_with_name(
          input$prepend_block_card_click$ctor,
          chr_ply(board_blocks(board$board), block_name)
        )
        id <- rand_names(board_block_ids(board$board))
        bk <- as_blocks(set_names(list(new_blk), id))
        update(list(blocks = list(add = bk)))
        return()
      }

      target_block <- sidebar$context$target_block
      target_input <- sidebar$context$target_input
      req(target_block, target_input)

      new_blk <- create_block_with_name(
        input$prepend_block_card_click$ctor,
        chr_ply(board_blocks(board$board), block_name)
      )

      blk_id <- rand_names(board_block_ids(board$board))
      lnk_id <- rand_names(board_link_ids(board$board))

      new_blk <- as_blocks(set_names(list(new_blk), blk_id))
      new_lnk <- new_link(
        from = blk_id,
        to = target_block,
        input = target_input
      )
      new_lnk <- as_links(set_names(list(new_lnk), lnk_id))

      update(list(
        blocks = list(add = new_blk),
        links = list(add = new_lnk)
      ))

      # Mark first action done for pinned mode
      if (isTRUE(sidebar$is_pinned)) {
        sidebar$pinned_first_action_done <- TRUE
      }
      hide_sidebar()
    }
  )

  # Prepend block: accordion confirm (with optional custom name/ID/link ID)
  observeEvent(
    input$prepend_block_card_confirm,
    {
      req(input$prepend_block_card_confirm$ctor)

      ctor <- input$prepend_block_card_confirm$ctor
      custom_name <- input[[paste0("block_name_", ctor)]]
      custom_id <- input[[paste0("block_id_", ctor)]]

      # When pinned and first action done, add standalone block (no link)
      if (isTRUE(sidebar$is_pinned) && isTRUE(sidebar$pinned_first_action_done)) {
        new_blk <- create_block_with_name(
          ctor,
          chr_ply(board_blocks(board$board), block_name)
        )
        if (!is.null(custom_name) && nzchar(custom_name)) {
          block_name(new_blk) <- custom_name
        }
        id <- if (!is.null(custom_id) && nzchar(custom_id)) {
          if (custom_id %in% board_block_ids(board$board)) {
            notify("Block ID already exists, generating new one.", type = "warning")
            rand_names(board_block_ids(board$board))
          } else {
            custom_id
          }
        } else {
          rand_names(board_block_ids(board$board))
        }
        bk <- as_blocks(set_names(list(new_blk), id))
        update(list(blocks = list(add = bk)))
        return()
      }

      target_block <- sidebar$context$target_block
      target_input <- sidebar$context$target_input
      req(target_block, target_input)
      custom_link_id <- input[[paste0("link_id_", ctor)]]

      new_blk <- create_block_with_name(
        ctor,
        chr_ply(board_blocks(board$board), block_name)
      )

      if (!is.null(custom_name) && nzchar(custom_name)) {
        block_name(new_blk) <- custom_name
      }

      blk_id <- if (!is.null(custom_id) && nzchar(custom_id)) {
        if (custom_id %in% board_block_ids(board$board)) {
          notify(
            "Block ID already exists, generating new one.", type = "warning"
          )
          rand_names(board_block_ids(board$board))
        } else {
          custom_id
        }
      } else {
        rand_names(board_block_ids(board$board))
      }

      lnk_id <- if (!is.null(custom_link_id) && nzchar(custom_link_id)) {
        if (custom_link_id %in% board_link_ids(board$board)) {
          notify(
            "Link ID already exists, generating new one.", type = "warning"
          )
          rand_names(board_link_ids(board$board))
        } else {
          custom_link_id
        }
      } else {
        rand_names(board_link_ids(board$board))
      }

      new_blk <- as_blocks(set_names(list(new_blk), blk_id))
      new_lnk <- new_link(
        from = blk_id,
        to = target_block,
        input = target_input
      )
      new_lnk <- as_links(set_names(list(new_lnk), lnk_id))

      update(list(
        blocks = list(add = new_blk),
        links = list(add = new_lnk)
      ))

      # Mark first action done for pinned mode
      if (isTRUE(sidebar$is_pinned)) {
        sidebar$pinned_first_action_done <- TRUE
      }
      hide_sidebar()
    }
  )

  # ==========================================================================
  # Link Card Click Handlers
  # ==========================================================================

  # Link: card click (instant add with first available input)
  observeEvent(
    input$link_card_click,
    {
      req(input$link_card_click$target)
      # When pinned and first action done, don't create more links
      if (isTRUE(sidebar$is_pinned) && isTRUE(sidebar$pinned_first_action_done)) return()
      source_block <- sidebar$context$source_block
      req(source_block)

      target <- input$link_card_click$target
      input_name <- input$link_card_click$input

      lnk_id <- rand_names(board_link_ids(board$board))
      new_lnk <- new_link(from = source_block, to = target, input = input_name)
      new_lnk <- as_links(set_names(list(new_lnk), lnk_id))

      update(list(links = list(add = new_lnk)))
      # Mark first action done for pinned mode
      if (isTRUE(sidebar$is_pinned)) {
        sidebar$pinned_first_action_done <- TRUE
      }
      hide_sidebar()
    }
  )

  # Link: accordion confirm (with optional custom input/ID)
  observeEvent(
    input$link_card_confirm,
    {
      req(input$link_card_confirm$target)
      # When pinned and first action done, don't create more links
      if (isTRUE(sidebar$is_pinned) && isTRUE(sidebar$pinned_first_action_done)) return()
      source_block <- sidebar$context$source_block
      req(source_block)

      target <- input$link_card_confirm$target
      # Get input from accordion selector if available
      sel_input <- input[[paste0("link_input_", target)]]
      input_name <- sel_input %||% input$link_card_confirm$input
      custom_id <- input[[paste0("link_id_", target)]]

      lnk_id <- if (!is.null(custom_id) && nzchar(custom_id)) {
        if (custom_id %in% board_link_ids(board$board)) {
          notify(
            "Link ID already exists, generating new one.", type = "warning"
          )
          rand_names(board_link_ids(board$board))
        } else {
          custom_id
        }
      } else {
        rand_names(board_link_ids(board$board))
      }

      new_lnk <- new_link(from = source_block, to = target, input = input_name)
      new_lnk <- as_links(set_names(list(new_lnk), lnk_id))

      update(list(links = list(add = new_lnk)))
      # Mark first action done for pinned mode
      if (isTRUE(sidebar$is_pinned)) {
        sidebar$pinned_first_action_done <- TRUE
      }
      hide_sidebar()
    }
  )

  # ==========================================================================
  # Stack Confirm Handlers
  # ==========================================================================

  # Create Stack
  observeEvent(
    input$stack_confirm,
    {
      sel_blks <- input$stack_block_selection
      block_ids <- if (length(sel_blks) && any(nzchar(sel_blks))) {
        sel_blks[nzchar(sel_blks)]
      } else {
        character()
      }

      stk_name <- input$stack_name
      stk_color <- input$stack_color
      stk_id <- input$stack_id

      if (!length(stk_color) || !nzchar(stk_color)) {
        stk_color <- suggest_new_colors(stack_color(board_stacks(board$board)))
      }
      if (!length(stk_id) || !nzchar(stk_id)) {
        stk_id <- rand_names(board_stack_ids(board$board))
      }
      if (!length(stk_name) || !nzchar(stk_name)) {
        stk_name <- id_to_sentence_case(stk_id)
      }

      new_stk <- new_dock_stack(
        blocks = block_ids, name = stk_name, color = stk_color
      )
      stacks_to_add <- as_stacks(set_names(list(new_stk), stk_id))
      update(list(stacks = list(add = stacks_to_add)))
      hide_sidebar()
    }
  )

  # Edit Stack
  observeEvent(
    input$edit_stack_confirm,
    {
      stack_id <- sidebar$context$stack_id
      req(stack_id)

      stack <- board_stacks(board$board)[[stack_id]]
      if (is.null(stack)) return()

      sel_blks <- input$edit_stack_blocks
      block_ids <- if (length(sel_blks) && any(nzchar(sel_blks))) {
        sel_blks[nzchar(sel_blks)]
      } else {
        character()
      }

      stk_name <- input$edit_stack_name
      stk_color <- input$edit_stack_color

      stack_blocks(stack) <- block_ids
      if (length(stk_name) && nzchar(stk_name)) {
        stack_name(stack) <- stk_name
      }
      if (length(stk_color) && nzchar(stk_color)) {
        stack_color(stack) <- stk_color
      }

      stacks_to_mod <- as_stacks(set_names(list(stack), stack_id))
      update(list(stacks = list(mod = stacks_to_mod)))
      hide_sidebar()
    }
  )

  invisible()
}
