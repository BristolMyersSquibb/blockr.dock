# "Show code" on a dock board: the board's script on a page of the board
# options sidebar, in place of core's modal. Opening the page
# (options-sidebar.js reports `code_mod`) asks for every block to be built,
# since an unbuilt block has no expression; nothing is evaluated. As in core,
# the script waits until every block is built, and is held back while a block
# is not configured or reports an error, with a button that evaluates the
# board once.
dock_code_server <- function(id, board, update, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(
        input$code_mod,
        update(list(construct = board_block_ids(board$board)))
      )

      observeEvent(
        input$code_eval,
        update(list(evaluate = board_block_ids(board$board)))
      )

      output$code_out <- renderUI(
        {
          state <- code_state(board)

          if (identical(state, "ready")) {
            return(tags$pre(class = "blockr-code-script", board_script(board)))
          }

          if (identical(state, "pending")) {
            return(div(class = "blockr-code-note", "Building the blocks…"))
          }

          tagList(
            div(
              class = "blockr-code-note",
              paste(
                "The board is not ready. Finish configuring all blocks, and",
                "fix any block reporting an error, before exporting code."
              )
            ),
            tags$button(
              id = session$ns("code_eval"),
              type = "button",
              class = "btn btn-default btn-sm action-button",
              "Evaluate blocks"
            )
          )
        }
      )

      NULL
    }
  )
}

dock_code_ui <- function(id, board) {
  div(
    class = "blockr-code-export",
    `data-open-input` = NS(id, "code_mod"),
    div(
      class = "blockr-code-actions",
      tags$button(
        type = "button",
        class = "btn btn-default btn-sm blockr-code-copy",
        onclick = paste0(
          "var pre = this.closest('.blockr-code-export').querySelector('pre');",
          "if (pre) navigator.clipboard.writeText(pre.innerText);"
        ),
        "Copy"
      )
    ),
    uiOutput(NS(id, "code_out"))
  )
}

# "pending" until every block is built, "blocked" while one is not configured
# or reports an error, else "ready", as core's export decides.
code_state <- function(board) {

  ids <- board_block_ids(board$board)

  if (!setequal(names(board$blocks), ids)) {
    return("pending")
  }

  ready <- lgl_ply(
    board$blocks,
    function(blk) isTRUE(reval_if(blk$server$state_ready))
  )

  cnd <- reval_if(board$conditions)

  if (!all(ready) || any(cnd$severity == "error")) {
    return("blocked")
  }

  "ready"
}

# The script, from core's exported export_code(): each block's expression
# with its inputs bound, assigned to the block's id, in link order.
board_script <- function(board) {

  exprs <- lapply(board$blocks, function(blk) reval(blk$server$expr))
  parts <- export_code(exprs, board$board)

  lines <- Map(
    function(id, expr, args, type) {
      paste(deparse(script_line(id, expr, args, type)), collapse = "\n")
    },
    names(parts$exprs),
    parts$exprs,
    parts$args,
    parts$types
  )

  paste(unlist(lines), collapse = "\n\n")
}

script_line <- function(id, expr, args, type) {

  if (identical(type, "bquoted")) {
    expr <- do.call(bquote, list(expr, args))
  }

  if (length(args) && identical(type, "quoted")) {
    expr <- call("with", args, expr)
  } else {
    expr <- call("local", expr)
  }

  bquote(.(nme) <- .(val), list(nme = as.name(id), val = expr))
}
