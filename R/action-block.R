add_block_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      # The add browser's catalogue never varies, so its body is
      # pre-rendered once in `board_ui()` (the `add_block_sidebar` mount)
      # under this action's namespace. Opening it is a pure toggle - no
      # re-render, no server round-trip for the markup.
      sidebar_id <- NS(isolate(board$board_id), "add_block_sidebar")
      added <- blockr.ui::block_browser_server(
        "browser",
        board = reactive(board$board)
      )

      observeEvent(trigger(), {
        log_debug("opening pre-rendered add block sidebar")
        blockr.ui::show_sidebar(sidebar_id, title = "Add new block")
      })

      observeEvent(added(), {
        # `added()` is a ready-to-apply `blocks` object (id-keyed, block
        # built and named menu-side), so just add it.
        update(list(blocks = list(add = added())))

        # Re-open without `ui` keeps the pre-rendered body in place (no
        # re-render) when the user pinned the panel; otherwise it hides.
        blockr.ui::keep_or_hide_sidebar(
          sidebar_id, ui = NULL, title = "Add new block"
        )
      })

      NULL
    },
    id = "add_block_action"
  )
}

append_block_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      sidebar_id <- NS(isolate(board$board_id), "actions_sidebar")
      # Append is trigger-specific (the source is the right-clicked
      # block), so the browser is rendered per open. Passing board +
      # target makes it return `list(blocks, links)` with the link's
      # port already resolved - this handler just applies both.
      added <- blockr.ui::block_browser_server(
        "browser",
        board = reactive(board$board),
        target = reactive(blockr.ui::append_to(trigger()))
      )

      browser_ui <- function() {
        blockr.ui::block_browser_ui(
          session$ns("browser"), board$board,
          blockr.ui::append_to(trigger())
        )
      }

      observeEvent(trigger(), {
        blockr.ui::show_sidebar(
          sidebar_id, title = "Append new block", ui = browser_ui()
        )
      })

      observeEvent(added(), {
        res <- added()
        update(list(
          blocks = list(add = res$blocks),
          links = list(add = res$links)
        ))

        blockr.ui::keep_or_hide_sidebar(
          sidebar_id, title = "Append new block", ui = browser_ui()
        )
      })

      NULL
    },
    id = "append_block_action"
  )
}

prepend_block_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      sidebar_id <- NS(isolate(board$board_id), "actions_sidebar")
      added <- blockr.ui::block_browser_server(
        "browser",
        board = reactive(board$board),
        target = reactive(blockr.ui::prepend_to(trigger()))
      )

      browser_ui <- function() {
        blockr.ui::block_browser_ui(
          session$ns("browser"), board$board,
          blockr.ui::prepend_to(trigger())
        )
      }

      observeEvent(trigger(), {
        blockr.ui::show_sidebar(
          sidebar_id, title = "Prepend new block", ui = browser_ui()
        )
      })

      observeEvent(added(), {
        res <- added()
        update(list(
          blocks = list(add = res$blocks),
          links = list(add = res$links)
        ))

        blockr.ui::keep_or_hide_sidebar(
          sidebar_id, title = "Prepend new block", ui = browser_ui()
        )
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
