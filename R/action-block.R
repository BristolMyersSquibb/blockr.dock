add_block_function <- function(trigger, board, update, input, session) {
  # Show the slide-out block panel with add mode
  observeEvent(
    trigger(),
    {
      log_debug("showing block panel for add")
      session$sendCustomMessage(
        "blockr-show-block-panel",
        list(mode = "add", source_block = NULL)
      )
    }
  )
}

#' @param trigger A string, function or [shiny::reactive()]
#' @param as_module Logical flag controlling the return type
#' @include action-class.R
#' @rdname action
#' @export
add_block_action <- new_action(add_block_function)

append_block_function <- function(trigger, board, update, input, session) {
  # Show the slide-out block panel with append mode
  observeEvent(
    trigger(),
    {
      log_debug("showing block panel for append from {trigger()}")
      session$sendCustomMessage(
        "blockr-show-block-panel",
        list(mode = "append", source_block = trigger())
      )
    }
  )
}

#' @include action-class.R
#' @rdname action
#' @export
append_block_action <- new_action(append_block_function)

remove_block_function <- function(trigger, update) {
  observeEvent(
    trigger(),
    update(list(blocks = list(rm = trigger())))
  )
}

#' @include action-class.R
#' @rdname action
#' @export
remove_block_action <- new_action(remove_block_function)
