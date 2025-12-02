#' @include action-class.R
#' @rdname action
#' @export
add_stack_action <- new_action(
  {
    observeEvent(
      trigger(),
      showModal(
        stack_modal(ns = session$ns, board = board$board, mode = "create")
      )
    )

    observeEvent(
      input$stack_confirm,
      {
        stack_id <- input$stack_id
        stack_name <- input$stack_name
        stack_color <- input$stack_color
        selected_blocks <- input$stack_block_selection

        if (
          !nchar(stack_id) ||
            stack_id %in% board_stack_ids(board$board)
        ) {
          notify(
            "Please choose a valid stack ID.",
            type = "warning",
            session = session
          )

          return()
        }

        # Get blocks - use selected blocks if any, otherwise empty
        # character vector
        has_blocks <- length(selected_blocks) > 0 &&
          !is.null(selected_blocks) &&
          any(nchar(selected_blocks) > 0)
        block_ids <- if (has_blocks) {
          selected_blocks[nchar(selected_blocks) > 0]
        } else {
          character()
        }

        # Create stack name - use input if provided, otherwise
        # generate from ID
        if (is.null(stack_name) || !nchar(stack_name)) {
          stack_name <- id_to_sentence_case(stack_id)
        }

        # Create the stack
        new_stk <- new_dag_stack(
          blocks = block_ids,
          name = stack_name,
          color = stack_color
        )

        new_stk <- as_stacks(set_names(list(new_stk), stack_id))

        update(list(stacks = list(add = new_stk)))

        removeModal()
      }
    )
  }
)

#' @include action-class.R
#' @rdname action
#' @export
edit_stack_action <- new_action(
  {
    ns <- session$ns

    observeEvent(
      trigger(),
      {
        stack <- board_stacks(board$board)[[trigger()]]

        showModal(
          stack_modal(
            ns = ns,
            board = board$board,
            mode = "edit",
            stack = stack,
            stack_id = trigger()
          )
        )
      }
    )

    observeEvent(
      input$edit_stack_confirm,
      {
        id <- trigger()
        stack <- board_stacks(board$board)[[id]]

        blocks <- input$edit_stack_blocks

        if (is.null(blocks)) {
          blocks <- character(0)
        }

        stack_blocks(stack) <- blocks
        stack_color(stack) <- input$edit_stack_color
        stack_name(stack) <- input$edit_stack_name

        stack <- as_stacks(set_names(list(stack), id))

        update(list(stacks = list(mod = stack)))

        removeModal()
      }
    )
  }
)

#' @include action-class.R
#' @rdname action
#' @export
remove_stack_action <- new_action(
  {
    observeEvent(
      trigger(),
      update(list(stacks = list(rm = trigger())))
    )
  }
)
