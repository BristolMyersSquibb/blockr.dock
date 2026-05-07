add_link_action <- function(trigger, board, update, ...,
                            sidebar_id = "main_sidebar") {

  new_action(
    function(input, output, session) {

      observeEvent(
        trigger(),
        {
          body <- link_sidebar_body(session$ns, board$board, trigger())
          if (is.null(body)) return()

          blockr.ui::show_sidebar(
            sidebar_id,
            title = "Create new link",
            ui = body
          )
        }
      )

      observeEvent(
        input$create_link,
        {
          req(input$create_link)

          res <- block_input_select(
            board_blocks(board$board)[[input$create_link]],
            input$create_link,
            board_links(board$board),
            mode = "update",
            session = session,
            inputId = "add_link_input"
          )

          if (is.null(res)) {
            notify(
              "No inputs are available for the selected block.",
              type = "warning"
            )
            return()
          }
        }
      )

      observeEvent(
        input$add_link_confirm,
        {
          lnk_id <- input$add_link_id

          if (!nchar(lnk_id) || lnk_id %in% board_link_ids(board$board)) {
            notify(
              "Please choose a valid link ID.",
              type = "warning",
              session = session
            )

            return()
          }

          lnk_inp <- input$add_link_input
          trg_blk <- board_blocks(board$board)[[input$create_link]]

          if (!is.na(block_arity(trg_blk))) {

            avail_inps <- block_input_select(
              trg_blk,
              input$create_link,
              board_links(board$board),
              mode = "inputs"
            )

            if (!nchar(lnk_inp) || !lnk_inp %in% avail_inps) {
              notify(
                "Please choose a valid link input.",
                type = "warning",
                session = session
              )

              return()
            }
          }

          new_lnk <- new_link(
            from = trigger(),
            to = input$create_link,
            input = lnk_inp
          )

          new_lnk <- as_links(set_names(list(new_lnk), lnk_id))

          update(
            list(links = list(add = new_lnk))
          )

          blockr.ui::keep_or_hide_sidebar(
            sidebar_id,
            title = "Create new link",
            ui = link_sidebar_body(session$ns, board$board, trigger())
          )
        }
      )

      NULL
    },
    id = "add_link_action"
  )
}

remove_link_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      observeEvent(
        trigger(),
        update(list(links = list(rm = trigger())))
      )
      NULL
    },
    id = "remove_link_action"
  )
}
