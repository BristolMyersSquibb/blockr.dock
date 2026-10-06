library(blockr.dock)
library(blockr.core)

# Two items an app appends to the default navbar, each drawing an output its
# server fills under the item's namespace: one renders nothing, as an avatar
# does with nobody signed in, the other the id of the board it was handed.
output_item <- function(id, content) {
  navbar_item(
    id,
    function(id, board) shiny::uiOutput(shiny::NS(id, "out"), inline = TRUE),
    function(id, board) {
      shiny::moduleServer(id, function(input, output, session) {
        output$out <- shiny::renderUI(content(board))
      })
    }
  )
}

serve(
  new_dock_board(
    blocks = c(a = new_dataset_block("iris")),
    views = list("a")
  ),
  "my_board",
  navbar = custom_navbar(
    c(
      output_item("empty", function(board) NULL),
      output_item(
        "filled",
        function(board) {
          shiny::tags$span(class = "navbar-filled", board$board_id)
        }
      )
    )
  )
)
