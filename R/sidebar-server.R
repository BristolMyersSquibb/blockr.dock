# =============================================================================
# Sidebar Server
# =============================================================================

# =============================================================================
# Block Creation Helpers
# =============================================================================

#' Validate or Generate ID
#'
#' Validates a custom ID against existing IDs, or generates a new one.
#' Shows a warning notification if the custom ID already exists.
#'
#' @param custom_id Custom ID to validate (can be NULL or empty)
#' @param existing_ids Character vector of existing IDs
#' @param type Type label for warning message (e.g., "Block", "Link")
#'
#' @return A valid unique ID
#' @keywords internal
validate_or_generate_id <- function(custom_id, existing_ids, type = "Block") {
  if (!is.null(custom_id) && nzchar(custom_id)) {
    if (custom_id %in% existing_ids) {
      notify(
        paste(type, "ID already exists, generating new one."), type = "warning"
      )
      rand_names(existing_ids)
    } else {
      custom_id
    }
  } else {
    rand_names(existing_ids)
  }
}

#' Create Block for Sidebar
#'
#' Creates a new block with optional custom name and ID.
#' Handles ID validation and generation.
#'
#' @param ctor Block constructor name
#' @param board The board object
#' @param custom_name Optional custom name for the block
#' @param custom_id Optional custom ID for the block
#'
#' @return A list with `block` and `id` elements
#' @keywords internal
create_block_for_sidebar <- function(ctor, board, custom_name = NULL,
                                     custom_id = NULL) {
  new_blk <- create_block_with_name(
    ctor,
    chr_ply(board_blocks(board), block_name)
  )

  if (!is.null(custom_name) && nzchar(custom_name)) {
    block_name(new_blk) <- custom_name
  }

  blk_id <- validate_or_generate_id(custom_id, board_block_ids(board), "Block")

  list(block = new_blk, id = blk_id)
}

#' Get Available Input for Block
#'
#' Gets the available inputs for a block, handling variadic blocks specially.
#' Returns NULL and shows a warning if no inputs are available.
#'
#' @param block The block object
#' @param preferred_input Optional preferred input name
#'
#' @return Input name to use, or NULL if none available
#' @keywords internal
get_block_input <- function(block, preferred_input = NULL) {
  avail_input <- block_inputs(block)

  # Handle variadic blocks (like write_block) - they accept numbered inputs
  if (length(avail_input) == 0 && is.na(block_arity(block))) {
    avail_input <- "1"
  }

  if (length(avail_input) == 0) {
    notify(
      "No inputs are available for the selected block.", type = "warning"
    )
    return(NULL)
  }

  # Use preferred input if valid, otherwise first available
  if (!is.null(preferred_input) && preferred_input %in% avail_input) {
    preferred_input
  } else {
    avail_input[1]
  }
}

# =============================================================================
# Sidebar Server
# =============================================================================

#' Sidebar Server Logic
#'
#' Sets up the server-side logic for sidebars including dynamic content
#' rendering via S3 dispatch and card click handlers.
#'
#' @param board Reactive board state
#' @param update Update function for board mutations
#' @param generate_code_ui Optional UI for generate code plugin (settings)
#' @param session Shiny session
#'
#' @keywords internal
sidebar_server <- function(board, update, generate_code_ui = NULL,
                           session = shiny::getDefaultReactiveDomain()) {

  ns <- session$ns
  input <- session$input
  output <- session$output

  # ==========================================================================
  # Initialize Sidebar Reactive
  # ==========================================================================
  # Must initialize here because renderUI runs early (suspendWhenHidden = FALSE)
  if (is.null(session$userData$blockr_sidebar_type)) {
    session$userData$blockr_sidebar_type <- reactiveVal(NULL)
  }

  # ==========================================================================
  # Debounced Search Input (single search for all sidebar content)
  # ==========================================================================

  sidebar_search <- reactive(input$sidebar_search) |> debounce(150)

  # ==========================================================================
  # Dynamic Sidebar Content Renderer (S3 Dispatch)
  # ==========================================================================

  output$sidebar_dynamic_content <- renderUI({
    # Get current sidebar type from reactive
    type <- session$userData$blockr_sidebar_type()
    req(type)

    # Call S3 dispatch - finds sidebar_content_ui.add_block, etc.
    result <- sidebar_content_ui(
      type,
      ns = ns,
      board = board$board,
      search_term = sidebar_search(),
      current_selection = switch(
        class(type)[1],
        "create_stack" = input$stack_block_selection,
        "edit_stack" = input$edit_stack_blocks,
        NULL
      ),
      generate_code_ui = generate_code_ui,
      input = input
    )

    # Update header/search visibility via JS
    session$sendCustomMessage("blockr-sidebar-meta", list(
      title = result$title %||% "",
      subtitle = result$subtitle %||% "",
      show_search = isTRUE(result$show_search)
    ))

    result$content
  })

  # Disable suspend when hidden - content needs to render even when hidden
  outputOptions(output, "sidebar_dynamic_content", suspendWhenHidden = FALSE)

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
      result <- create_block_for_sidebar(
        input$add_block_card_click$ctor, board$board
      )
      bk <- as_blocks(set_names(list(result$block), result$id))
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
      result <- create_block_for_sidebar(
        ctor,
        board$board,
        custom_name = input[[paste0("block_name_", ctor)]],
        custom_id = input[[paste0("block_id_", ctor)]]
      )
      bk <- as_blocks(set_names(list(result$block), result$id))
      update(list(blocks = list(add = bk)))
      hide_sidebar()
    }
  )

  # Append block: card click (instant add with auto-generated ID + link)
  observeEvent(
    input$append_block_card_click,
    {
      req(input$append_block_card_click$ctor)
      ctx <- get_sidebar_context()
      source_block <- ctx$source_block
      req(source_block)

      # Create block and check inputs
      blk_result <- create_block_for_sidebar(
        input$append_block_card_click$ctor, board$board
      )
      input_name <- get_block_input(blk_result$block)
      if (is.null(input_name)) return()

      lnk_id <- rand_names(board_link_ids(board$board))

      new_blk <- as_blocks(set_names(list(blk_result$block), blk_result$id))
      new_lnk <- new_link(from = source_block, to = blk_result$id, input = input_name)
      new_lnk <- as_links(set_names(list(new_lnk), lnk_id))

      update(list(
        blocks = list(add = new_blk),
        links = list(add = new_lnk)
      ))

      hide_sidebar()
    }
  )

  # Append block: accordion confirm (with optional custom name/ID/input/link ID)
  observeEvent(
    input$append_block_card_confirm,
    {
      req(input$append_block_card_confirm$ctor)
      ctx <- get_sidebar_context()
      source_block <- ctx$source_block
      req(source_block)

      ctor <- input$append_block_card_confirm$ctor
      custom_name <- input[[paste0("block_name_", ctor)]]
      custom_id <- input[[paste0("block_id_", ctor)]]
      custom_input <- input[[paste0("block_input_", ctor)]]
      custom_link_id <- input[[paste0("link_id_", ctor)]]

      # Create block first to check inputs and get preferred input
      blk_result <- create_block_for_sidebar(
        ctor, board$board, custom_name, custom_id
      )
      input_name <- get_block_input(blk_result$block, custom_input)
      if (is.null(input_name)) return()

      lnk_id <- validate_or_generate_id(
        custom_link_id, board_link_ids(board$board), "Link"
      )

      new_blk <- as_blocks(set_names(list(blk_result$block), blk_result$id))
      new_lnk <- new_link(from = source_block, to = blk_result$id, input = input_name)
      new_lnk <- as_links(set_names(list(new_lnk), lnk_id))

      update(list(
        blocks = list(add = new_blk),
        links = list(add = new_lnk)
      ))

      hide_sidebar()
    }
  )

  # Prepend block: card click (instant add with auto-generated ID + link)
  observeEvent(
    input$prepend_block_card_click,
    {
      req(input$prepend_block_card_click$ctor)
      ctx <- get_sidebar_context()
      target_block <- ctx$target_block
      target_input <- ctx$target_input
      req(target_block, target_input)

      blk_result <- create_block_for_sidebar(
        input$prepend_block_card_click$ctor, board$board
      )

      lnk_id <- rand_names(board_link_ids(board$board))

      new_blk <- as_blocks(set_names(list(blk_result$block), blk_result$id))
      new_lnk <- new_link(
        from = blk_result$id,
        to = target_block,
        input = target_input
      )
      new_lnk <- as_links(set_names(list(new_lnk), lnk_id))

      update(list(
        blocks = list(add = new_blk),
        links = list(add = new_lnk)
      ))

      hide_sidebar()
    }
  )

  # Prepend block: accordion confirm (with optional custom name/ID/link ID)
  observeEvent(
    input$prepend_block_card_confirm,
    {
      req(input$prepend_block_card_confirm$ctor)
      ctx <- get_sidebar_context()
      target_block <- ctx$target_block
      target_input <- ctx$target_input
      req(target_block, target_input)

      ctor <- input$prepend_block_card_confirm$ctor
      blk_result <- create_block_for_sidebar(
        ctor,
        board$board,
        custom_name = input[[paste0("block_name_", ctor)]],
        custom_id = input[[paste0("block_id_", ctor)]]
      )

      lnk_id <- validate_or_generate_id(
        input[[paste0("link_id_", ctor)]], board_link_ids(board$board), "Link"
      )

      new_blk <- as_blocks(set_names(list(blk_result$block), blk_result$id))
      new_lnk <- new_link(
        from = blk_result$id,
        to = target_block,
        input = target_input
      )
      new_lnk <- as_links(set_names(list(new_lnk), lnk_id))

      update(list(
        blocks = list(add = new_blk),
        links = list(add = new_lnk)
      ))

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
      ctx <- get_sidebar_context()
      source_block <- ctx$source_block
      req(source_block)

      target <- input$link_card_click$target
      input_name <- input$link_card_click$input

      lnk_id <- rand_names(board_link_ids(board$board))
      new_lnk <- new_link(from = source_block, to = target, input = input_name)
      new_lnk <- as_links(set_names(list(new_lnk), lnk_id))

      update(list(links = list(add = new_lnk)))
      hide_sidebar()
    }
  )

  # Link: accordion confirm (with optional custom input/ID)
  observeEvent(
    input$link_card_confirm,
    {
      req(input$link_card_confirm$target)
      ctx <- get_sidebar_context()
      source_block <- ctx$source_block
      req(source_block)

      target <- input$link_card_confirm$target
      # Get input from accordion selector if available
      sel_input <- input[[paste0("link_input_", target)]]
      input_name <- sel_input %||% input$link_card_confirm$input

      lnk_id <- validate_or_generate_id(
        input[[paste0("link_id_", target)]], board_link_ids(board$board), "Link"
      )

      new_lnk <- new_link(from = source_block, to = target, input = input_name)
      new_lnk <- as_links(set_names(list(new_lnk), lnk_id))

      update(list(links = list(add = new_lnk)))
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
      ctx <- get_sidebar_context()
      stack_id <- ctx$stack_id
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

  # ==========================================================================
  # Settings Sidebar Trigger
  # ==========================================================================

  observeEvent(input$open_settings_sidebar, {
    show_sidebar("settings")
  })

  invisible()
}
