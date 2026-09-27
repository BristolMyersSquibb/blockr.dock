# Adding, appending and prepending a block open the "+" menu in place
# (add-block-menu.R); a pick comes back through block_browser_server(),
# which builds the block and, for append and prepend, its link.
add_block_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      added <- block_browser_server(
        "browser",
        board = reactive(board$board)
      )

      observeEvent(trigger(), {
        open_add_block_menu("add", "Add a block", session = session)
      })

      observeEvent(added(), {
        # `added()` is a ready-to-apply `blocks` object (id-keyed, block
        # built and named server-side), so just add it.
        update(list(blocks = list(add = added())))
      })

      NULL
    },
    id = "add_block_action"
  )
}

append_block_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      # The source block is supplied via the `target` reactive and resolved
      # into the link at commit.
      added <- block_browser_server(
        "browser",
        board = reactive(board$board),
        target = reactive(append_to(trigger()))
      )

      observeEvent(trigger(), {
        open_add_block_menu(
          "append",
          paste("Append to", block_label(board$board, trigger())),
          session = session
        )
      })

      observeEvent(added(), {
        # `added()` is `list(blocks, links)` with the link's port already
        # resolved; apply both.
        res <- added()
        update(list(
          blocks = list(add = res$blocks),
          links = list(add = res$links)
        ))
      })

      NULL
    },
    id = "append_block_action"
  )
}

prepend_block_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      added <- block_browser_server(
        "browser",
        board = reactive(board$board),
        target = reactive(prepend_to(trigger()))
      )

      observeEvent(trigger(), {
        open_add_block_menu(
          "prepend",
          paste("Prepend to", block_label(board$board, trigger())),
          session = session
        )
      })

      observeEvent(added(), {
        res <- added()
        update(list(
          blocks = list(add = res$blocks),
          links = list(add = res$links)
        ))
      })

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
