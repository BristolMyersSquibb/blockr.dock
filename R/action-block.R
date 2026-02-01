add_block_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      # Show sidebar when trigger fires
      # Card click handling is in sidebar_server (namespace match)
      observeEvent(
        trigger(),
        ignoreInit = TRUE,
        {
          log_debug("showing add block panel")
          show_sidebar("add_block")
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
        ignoreInit = TRUE,
        {
          set_sidebar_context(source_block = trigger())
          show_sidebar("append_block")
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
        ignoreInit = TRUE,
        {
          set_sidebar_context(
            target_block = trigger()$target,
            target_input = trigger()$input
          )
          show_sidebar("prepend_block")
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
        ignoreInit = TRUE,
        update(list(blocks = list(rm = trigger())))
      )
      NULL
    },
    id = "remove_block_action"
  )
}
