library(shiny)
library(blockr.core)
library(blockr.dock)

# A button firing the action trigger straight off the bundle, so the menu has
# one deterministic entry point -- the same shape as `sidebar-menus`, which
# carries the other four menus. This board exists separately because the rows
# that hold a remove button and a name field are a variadic block's, and
# because their ids have to be fixed up front.
menu_ui <- function(id, board) {
  actionButton(NS(id, "edit_inputs"), "Edit inputs of v")
}

# The menu keys every row by link id and commits that id back, so an assertion
# names a slot by id rather than by position -- which is what tells "the id
# travelled with the click" apart from "the first row was used". The target is
# always `v`, so `to` carries nothing an assertion wants.
slot_specs <- function(board) {

  lnk <- as.data.frame(board_links(board))

  if (!nrow(lnk)) {
    return(character())
  }

  paste0(lnk[["id"]], ":", lnk[["from"]], ">", lnk[["input"]])
}

menu_srv <- function(id, board, update, actions, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(input$edit_inputs, actions[["edit_inputs_action"]]("v"))

      exportTestValues(links = slot_specs(board$board))

      list(state = list())
    }
  )
}

new_menu_action_extension <- function(...) {
  new_dock_extension(
    menu_srv,
    menu_ui,
    name = "Fire inputs menu",
    class = "menu_action_extension",
    ...
  )
}

# Block `v` is variadic, so its rows are positional slots carrying a remove
# button and a name field; a finite block's rows are port pickers instead, and
# reach neither handler. Three slots leave a middle one to commit against.
serve(
  new_dock_board(
    c(
      a = new_dataset_block("iris"),
      b = new_dataset_block("iris"),
      c = new_dataset_block("iris"),
      v = new_rbind_block()
    ),
    links = links(
      l1 = new_link("a", "v"),
      l2 = new_link("b", "v"),
      l3 = new_link("c", "v")
    ),
    extensions = list(menus = new_menu_action_extension())
  ),
  "my_board"
)
