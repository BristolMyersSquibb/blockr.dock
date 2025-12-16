add_stack_action <- function(trigger, board, update) {

  new_action(
    function(input, output, session) {

      observeEvent(
        trigger(),
        showModal(
          stack_modal(ns = session$ns, board = board$board, mode = "create")
        )
      )

      observeEvent(
        input$stack_confirm,
        {
          stk_id <- input$stack_id

          if (!nchar(stk_id) || stk_id %in% board_stack_ids(board$board)) {

            notify(
              "Please choose a valid stack ID.",
              type = "warning",
              session = session
            )

            return()
          }

          sel_blks <- input$stack_block_selection

          if (length(sel_blks) && any(nchar(sel_blks))) {
            block_ids <- sel_blks[nchar(sel_blks) > 0]
          } else {
            block_ids <- character()
          }

          if (!all(block_ids %in% board_block_ids(board$board))) {

            notify(
              "Please choose valid block IDs.",
              type = "warning",
              session = session
            )

            return()
          }

          stk_nme <- input$stack_name

          if (!length(stk_nme) || !nchar(stk_nme)) {
            stk_nme <- id_to_sentence_case(stk_id)
          }

          stk_col <- input$stack_color

          if (is.null(stk_col) || !nchar(stk_col) || !is_hex_color(stk_col)) {

            notify(
              "Please choose a valid stack color.",
              type = "warning",
              session = session
            )

            return()
          }

          new_stk <- new_dock_stack(
            blocks = block_ids,
            name = stk_nme,
            color = stk_col
          )

          new_stk <- as_stacks(set_names(list(new_stk), stk_id))

          update(list(stacks = list(add = new_stk)))

          removeModal()
        }
      )

      NULL
    },
    id = "add_stack_action"
  )
}

edit_stack_action <- function(trigger, board, update) {

  new_action(
    function(input, output, session) {

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

          sel_blks <- input$edit_stack_blocks

          if (length(sel_blks) && any(nchar(sel_blks))) {
            block_ids <- sel_blks[nchar(sel_blks) > 0]
          } else {
            block_ids <- character()
          }

          if (!all(block_ids %in% board_block_ids(board$board))) {

            notify(
              "Please choose valid block IDs.",
              type = "warning",
              session = session
            )

            return()
          }

          stack_blocks(stack) <- block_ids

          stk_col <- input$edit_stack_color

          if (is.null(stk_col) || !nchar(stk_col) || !is_hex_color(stk_col)) {

            notify(
              "Please choose a valid stack color.",
              type = "warning",
              session = session
            )

            return()
          }

          stack_color(stack) <- stk_col

          stk_nme <- input$edit_stack_name

          if (!length(stk_nme) || !nchar(stk_nme)) {

            notify(
              "Please choose a valid stack name.",
              type = "warning",
              session = session
            )

            return()
          }

          stack_name(stack) <- stk_nme

          stack <- as_stacks(set_names(list(stack), id))

          update(list(stacks = list(mod = stack)))

          removeModal()
        }
      )

      NULL
    },
    id = "edit_stack_action"
  )
}

remove_stack_action <- function(trigger, board, update) {
  new_action(
    function(input, output, session) {
      observeEvent(
        trigger(),
        update(list(stacks = list(rm = trigger())))
      )
      NULL
    },
    id = "remove_stack_action"
  )
}
