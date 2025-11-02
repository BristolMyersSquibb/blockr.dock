#' @keywords internal
edit_block_ui <- function(x, id, ...) {
  list(
    block_name = popover(
      uiOutput(NS(id, "block_name_out"), inline = TRUE),
      title = "Provide a new title",
      textInput(
        NS(id, "block_name_in"),
        "Block name",
        updateOn = "blur"
      )
    ),
    block_summary = uiOutput(NS(id, "block_summary"))
  )
}

#' @keywords internal
edit_block_server <- function(id, block_id, board, update, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      initial_block <- isolate(
        board_blocks(board$board)[[block_id]]
      )

      cur_name <- reactive({
        req(board_blocks(board$board)[[block_id]])
        block_name(board_blocks(board$board)[[block_id]])
      })

      output$block_name_out <- renderUI({
        h3(
          input$block_name_in,
          tags$sup(icon("pencil-square", class = "fa-2xs"))
        )
      })

      observeEvent(
        cur_name(),
        updateTextInput(
          session,
          "block_name_in",
          "Block name",
          cur_name()
        )
      )

      # Update block data (name)
      observeEvent(
        input$block_name_in,
        {
          req(input$block_name_in)
          if (!identical(cur_name(), input$block_name_in)) {
            new_val <- board_blocks(board$board)[[block_id]]
            block_name(new_val) <- input$block_name_in
            new_val <- as_blocks(set_names(list(new_val), block_id))
            update(list(blocks = list(mod = new_val)))
          }
        }
      )

      output$block_summary <- renderText(
        block_summary(
          initial_block,
          reval_if(board$blocks[[block_id]]$server$result)
        )
      )

      NULL
    }
  )
}
