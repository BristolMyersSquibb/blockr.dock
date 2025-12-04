add_link_function <- function(trigger, board, update, input, session) {

  observeEvent(
    trigger(),
    showModal(link_modal(session$ns, board$board, trigger()))
  )

  observeEvent(
    input$create_link,
    {
      req(input$create_link)

      res <- block_input_select(
        board_blocks(board$board)[[input$create_link]],
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

      new_lnk <- new_link(
        from = trigger(),
        to = input$create_link,
        input = input$add_link_input
      )

      new_lnk <- as_links(set_names(list(new_lnk), lnk_id))

      update(
        list(links = list(add = new_lnk))
      )

      removeModal()
    }
  )
}

#' @include action-class.R
#' @rdname action
#' @export
add_link_action <- new_action(add_link_function)

remove_link_function <- function(trigger, update) {
  observeEvent(
    trigger(),
    update(list(links = list(rm = trigger())))
  )
}

#' @include action-class.R
#' @rdname action
#' @export
remove_link_action <- new_action(remove_link_function)
