add_block_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      # Show sidebar when trigger fires
      # Card click handling is in sidebar_server (namespace match)
      observeEvent(
        trigger(),
        {
          log_debug("showing add block panel")
          show_sidebar(new_sidebar("add_block", list(mode = "add")))
        }
      )

      NULL
    },
    id = "add_block_action"
  )
}

append_block_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      # Show sidebar when trigger fires
      # Card click handling is in sidebar_server (namespace match)
      observeEvent(
        trigger(),
        {
          show_sidebar(new_sidebar("append_block", list(
            mode = "append",
            source_block = trigger()
          )))
        }
      )

      NULL
    },
    id = "append_block_action"
  )
}

prepend_block_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      # Show sidebar when trigger fires
      # Card click handling is in sidebar_server (namespace match)
      observeEvent(
        trigger(),
        {
          show_sidebar(new_sidebar("prepend_block", list(
            mode = "prepend",
            target_block = trigger()$target,
            target_input = trigger()$input
          )))
        }
      )

      NULL
    },
    id = "prepend_block_action"
  )
}

remove_block_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      observeEvent(
        trigger(),
        update(list(blocks = list(rm = trigger())))
      )
      NULL
    },
    id = "remove_block_action"
  )
}
