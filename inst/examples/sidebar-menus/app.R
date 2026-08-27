library(shiny)
library(blockr.core)
library(blockr.dock)

# Buttons firing the action triggers straight off the bundle, so each menu has
# one deterministic entry point. The e2e suite this feeds drives the menus'
# client JS, not the context-menu gesture that reaches them.
menu_ui <- function(id, board) {
  div(
    actionButton(NS(id, "add_block"), "Add block"),
    actionButton(NS(id, "link_a"), "Connect a"),
    actionButton(NS(id, "link_m"), "Connect m"),
    actionButton(NS(id, "add_stack"), "Add stack"),
    actionButton(NS(id, "edit_stack"), "Edit s1")
  )
}

# A committed link takes a generated id, so orientation rather than identity is
# what an assertion can name. The empty board needs its own branch: `paste0()`
# recycles a zero-length column against the separators and yields ">>".
link_specs <- function(board) {

  lnk <- as.data.frame(board_links(board))

  if (!nrow(lnk)) {
    return(character())
  }

  paste0(lnk[["from"]], ">", lnk[["to"]], ">", lnk[["input"]])
}

menu_srv <- function(id, board, update, actions, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(input$add_block, actions[["add_block_action"]](TRUE))
      observeEvent(input$link_a, actions[["add_link_action"]]("a"))
      observeEvent(input$link_m, actions[["add_link_action"]]("m"))
      observeEvent(input$add_stack, actions[["add_stack_action"]](TRUE))
      observeEvent(input$edit_stack, actions[["edit_stack_action"]]("s1"))

      exportTestValues(
        blocks = names(board_blocks(board$board)),
        links = link_specs(board$board),
        stacks = lapply(board_stacks(board$board), stack_blocks)
      )

      list(state = list())
    }
  )
}

new_menu_action_extension <- function(...) {
  new_dock_extension(
    menu_srv,
    menu_ui,
    name = "Fire menus",
    class = "menu_action_extension",
    ...
  )
}

# Blocks `r` and `s` are stacked so the create-stack pool (which drops stacked
# blocks) still offers three cards while the edit flow has two pre-selected.
# The `m` merge block carries two ports, which is what gives the link menu a
# target-input picker to refresh.
serve(
  new_dock_board(
    c(
      a = new_dataset_block("iris"),
      b = new_head_block(),
      m = new_merge_block(),
      r = new_head_block(),
      s = new_head_block()
    ),
    stacks = stacks(s1 = c("r", "s")),
    extensions = list(menus = new_menu_action_extension())
  ),
  "my_board"
)
