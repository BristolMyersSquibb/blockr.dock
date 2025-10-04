new_block_extension <- function() {
	new_dock_extension(
    blk_ext_srv,
    blk_ext_ui,
    name = "Blocks",
    class = "block_extension"
  )
}

blk_ext_ui <- function(id, board) {
  tagList(
    fluidRow(
      column(
        4,
        selectInput(
          NS(id, "registry_select"),
          "Select block from registry",
          choices = list_blocks()
        )
      ),
      column(
        4,
        textInput(
          inputId = NS(id, "block_id"),
          label = "Block ID",
          value = rand_names(),
          placeholder = "Enter a block ID."
        )
      ),
      column(
        4,
        actionButton(
          NS(id, "confirm_add"),
          "Add block",
          class = "btn-success"
        )
      )
    ),
    fluidRow(
      column(
        4,
        selectInput(
          NS(id, "block_select"),
          "Select block(s) from board",
          choices = board_block_ids(board),
          multiple = TRUE
        )
      ),
      column(
        4,
        actionButton(
          NS(id, "confirm_rm"),
          "Remove block",
          class = "btn-success"
        )
      )
    )
  )
}

blk_ext_srv <- function(id, board, update, ...) {

  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(
        input$confirm_add,
        {
          sel <- input$registry_select
          bid <- input$block_id

          if (!validate_block_addition(sel, bid, board$board, session)) {
            return()
          }

          blk <- create_block(sel, name = id_to_sentence_case(bid))
          add <- as_blocks(set_names(list(blk), bid))

          update(
            list(blocks = list(add = add))
          )

          removeModal()
        }
      )

      observeEvent(
        input$confirm_rm,
        {
          sel <- input$block_select

          if (!length(sel)) {

            notify(
              "Please choose at least one block.",
              type = "warning",
              session = session
            )

            return()
          }

          if (!all(sel %in% board_block_ids(board$board))) {

            notify(
              "Please choose valid block IDs.",
              type = "warning",
              session = session
            )

            return()
          }

          update(
            list(blocks = list(rm = sel))
          )

          removeModal()
        }
      )

      observeEvent(
        board_block_ids(board$board),
        updateSelectInput(
          session,
          "block_select",
          choices = board_block_ids(board$board)
        )
      )
    }
  )
}

validate_block_addition <- function(block, id, board, session) {

  if (nchar(id) == 0L || !is_string(id)) {

    notify(
      "Please choose a valid block ID.",
      type = "warning",
      session = session
    )

    return(FALSE)
  }

  if (id %in% board_block_ids(board)) {

    notify(
      "Please choose a unique block ID.",
      type = "warning",
      session = session
    )

    return(FALSE)
  }

  if (!is_string(block) || !block %in% list_blocks()) {

    notify(
      "Please choose a valid block type.",
      type = "warning",
      session = session
    )

    return(FALSE)
  }

  TRUE
}
