library(shiny)
library(blockr.core)
library(blockr.dock)

# Two buttons firing shared-panel actions straight off the trigger bundle -
# the path a consumer's context menu takes, and the one that declares nothing
# about which panel the action fills.
fire_ui <- function(id, board) {
  div(
    actionButton(NS(id, "add_link"), "Connect a"),
    actionButton(NS(id, "edit_stack"), "Edit s1")
  )
}

fire_srv <- function(id, board, update, actions, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(input$add_link, actions[["add_link_action"]]("a"))
      observeEvent(input$edit_stack, actions[["edit_stack_action"]]("s1"))

      list(state = list())
    }
  )
}

new_fire_action_extension <- function(...) {
  new_dock_extension(
    fire_srv,
    fire_ui,
    name = "Fire actions",
    class = "fire_action_extension",
    ...
  )
}

serve(
  new_dock_board(
    c(a = new_dataset_block("iris"), b = new_head_block()),
    stacks = stacks(s1 = "a"),
    extensions = list(fire = new_fire_action_extension())
  ),
  "my_board"
)
