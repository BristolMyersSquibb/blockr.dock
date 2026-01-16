add_block_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      ns <- session$ns
      blk <- reactiveVal()

      observeEvent(
        trigger(),
        {
          log_debug("showing add block action modal")
          blk(NULL)

          showModal(
            block_modal(
              ns = ns,
              board = board$board,
              mode = "add"
            )
          )
        }
      )

      observeEvent(
        input$add_block_selection,
        {
          req(input$add_block_selection)

          new_blk <- create_block_with_name(
            input$add_block_selection,
            chr_ply(board_blocks(board$board), block_name)
          )

          updateTextInput(
            session,
            "add_block_name",
            value = block_name(new_blk)
          )

          blk(new_blk)
        }
      )

      observeEvent(
        input$add_block_confirm,
        {
          id <- input$add_block_id
          bk <- blk()

          if (!nchar(id) || id %in% board_block_ids(board$board)) {
            notify(
              "Please choose a valid block ID.",
              type = "warning",
              session = session
            )

            return()
          }

          if (!is_block(bk)) {
            notify(
              "Please choose a block type.",
              type = "warning",
              session = session
            )

            return()
          }

          if (!identical(input$add_block_name, block_name(bk))) {
            block_name(bk) <- input$add_block_name
          }

          bk <- as_blocks(set_names(list(bk), id))

          update(list(blocks = list(add = bk)))
          removeModal()
        }
      )

      NULL
    },
    id = "add_block_action"
  )
}

append_block_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      ns <- session$ns
      blk <- reactiveVal()

      observeEvent(
        trigger(),
        {
          blk(NULL)
          showModal(
            block_modal(
              ns = ns,
              board = board$board,
              mode = "append"
            )
          )
        }
      )

      observeEvent(
        input$append_block_selection,
        {
          req(input$append_block_selection)

          new_blk <- create_block_with_name(
            input$append_block_selection,
            chr_ply(board_blocks(board$board), block_name)
          )

          res <- block_input_select(
            new_blk,
            mode = "update",
            session = session,
            inputId = "append_block_input"
          )

          if (is.null(res)) {
            notify(
              "No inputs are available for the selected block.",
              type = "warning"
            )
            return()
          }

          updateTextInput(
            session,
            "append_block_name",
            value = block_name(new_blk)
          )

          blk(new_blk)
        }
      )

      observeEvent(
        input$append_block_confirm,
        {
          blk_id <- input$append_block_id
          lnk_id <- input$append_link_id

          if (!nchar(blk_id) || blk_id %in% board_block_ids(board$board)) {
            notify(
              "Please choose a valid block ID.",
              type = "warning",
              session = session
            )
            return()
          }

          if (!nchar(lnk_id) || lnk_id %in% board_link_ids(board$board)) {
            notify(
              "Please choose a valid link ID.",
              type = "warning",
              session = session
            )
            return()
          }

          new_blk <- blk()

          if (!is_block(new_blk)) {
            notify(
              "Please choose a block type.",
              type = "warning",
              session = session
            )
            return()
          }

          if (!identical(input$append_block_name, block_name(new_blk))) {
            block_name(new_blk) <- input$append_block_name
          }

          new_blk <- as_blocks(set_names(list(new_blk), blk_id))

          new_lnk <- new_link(
            from = trigger(),
            to = blk_id,
            input = input$append_block_input
          )

          new_lnk <- as_links(set_names(list(new_lnk), lnk_id))

          update(
            list(
              blocks = list(add = new_blk),
              links = list(add = new_lnk)
            )
          )

          removeModal()
        }
      )

      NULL
    },
    id = "append_block_action"
  )
}

prepend_block_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      ns <- session$ns
      blk <- reactiveVal()

      observeEvent(
        trigger(),
        {
          blk(NULL)
          showModal(
            block_modal(
              ns = ns,
              board = board$board,
              mode = "prepend"
            )
          )
        }
      )

      observeEvent(
        input$prepend_block_selection,
        {
          req(input$prepend_block_selection)

          new_blk <- create_block_with_name(
            input$prepend_block_selection,
            chr_ply(board_blocks(board$board), block_name)
          )

          updateTextInput(
            session,
            "prepend_block_name",
            value = block_name(new_blk)
          )

          blk(new_blk)
        }
      )

      observeEvent(
        input$prepend_block_confirm,
        {
          blk_id <- input$prepend_block_id
          lnk_id <- input$prepend_link_id

          if (!nchar(blk_id) || blk_id %in% board_block_ids(board$board)) {
            notify(
              "Please choose a valid block ID.",
              type = "warning",
              session = session
            )
            return()
          }

          if (!nchar(lnk_id) || lnk_id %in% board_link_ids(board$board)) {
            notify(
              "Please choose a valid link ID.",
              type = "warning",
              session = session
            )
            return()
          }

          new_blk <- blk()

          if (!is_block(new_blk)) {
            notify(
              "Please choose a block type.",
              type = "warning",
              session = session
            )
            return()
          }

          if (!identical(input$prepend_block_name, block_name(new_blk))) {
            block_name(new_blk) <- input$prepend_block_name
          }

          new_blk <- as_blocks(set_names(list(new_blk), blk_id))

          # Compared to append block, trigger is a bit special as
          # it has to provide the target node
          # and the input port to connect to.
          new_lnk <- new_link(
            from = blk_id,
            to = trigger()$target,
            input = trigger()$input
          )

          new_lnk <- as_links(set_names(list(new_lnk), lnk_id))

          update(
            list(
              blocks = list(add = new_blk),
              links = list(add = new_lnk)
            )
          )

          removeModal()
        }
      )

      NULL
    },
    id = "prepend_block_action"
  )
}

remove_block_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      observeEvent(
        trigger(),
        update(list(blocks = list(rm = trigger())))
      )
      NULL
    },
    id = "remove_block_action"
  )
}
