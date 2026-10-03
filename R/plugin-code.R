# "Show code" is the last row of the board options list, so the dock gives
# core's code plugin a row of blockr.ui's menu for its UI. Core's server opens
# the code dialog on `code_mod`, which the row reports as an action button.
show_code_ui <- function(id, board) {
  tags$button(
    id = NS(id, "code_mod"),
    type = "button",
    class = paste(
      "action-button blockr-menu__item blockr-menu__item--quiet",
      "blockr-options-code"
    ),
    tags$span(class = "blockr-menu__icon", blockr.ui::small_icon("code")),
    tags$span(class = "blockr-menu__label", "Show code")
  )
}
