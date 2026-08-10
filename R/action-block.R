add_block_action <- function(trigger, board, update, ...) {

  action <- "add_block_action"

  new_action(
    function(input, output, session) {
      # The add browser's catalogue never varies, so its body is
      # pre-rendered once in `board_ui()` (the `add_block_sidebar` mount)
      # under this action's namespace. Opening it is a pure toggle - no
      # re-render, no server round-trip for the markup.
      sidebar_id <- NS(isolate(board$board_id), "add_block_sidebar")
      added <- block_browser_server(
        "browser",
        board = reactive(board$board)
      )

      observeEvent(trigger(), {
        log_debug("opening pre-rendered add block sidebar")
        show_sidebar(sidebar_id, title = "Add new block", owner = action)
      })

      observeEvent(added(), {
        # `added()` is a ready-to-apply `blocks` object (id-keyed, block
        # built and named menu-side), so just add it.
        update(list(blocks = list(add = added())))

        # Re-open without `ui` keeps the pre-rendered body in place (no
        # re-render) when the user pinned the panel; otherwise it hides.
        keep_or_hide_sidebar(
          sidebar_id, ui = NULL, title = "Add new block", owner = action
        )
      })

      NULL
    },
    id = action
  )
}

append_block_action <- function(trigger, board, update, ...) {

  action <- "append_block_action"

  new_action(
    function(input, output, session) {
      # The append catalogue (linkable blocks) is registry-based, not
      # source-specific, so its body is pre-rendered once in `board_ui()`
      # (the `append_block_sidebar` mount) and opening is a pure toggle.
      # The source (right-clicked block) is supplied here via the `target`
      # reactive and resolved into the link at commit; the "Append from X"
      # context goes in the sidebar title.
      sidebar_id <- NS(isolate(board$board_id), "append_block_sidebar")
      added <- block_browser_server(
        "browser",
        board = reactive(board$board),
        target = reactive(append_to(trigger()))
      )

      title <- function() paste0("Append from ", trigger())

      observeEvent(trigger(), {
        show_sidebar(sidebar_id, title = title(), owner = action)
      })

      observeEvent(added(), {
        # `added()` is `list(blocks, links)` with the link's port already
        # resolved menu-side; apply both.
        res <- added()
        update(list(
          blocks = list(add = res$blocks),
          links = list(add = res$links)
        ))

        keep_or_hide_sidebar(
          sidebar_id, ui = NULL, title = title(), owner = action
        )
      })

      NULL
    },
    id = action
  )
}

prepend_block_action <- function(trigger, board, update, ...) {

  action <- "prepend_block_action"

  new_action(
    function(input, output, session) {
      sidebar_id <- NS(isolate(board$board_id), "actions_sidebar")
      added <- block_browser_server(
        "browser",
        board = reactive(board$board),
        target = reactive(prepend_to(trigger()))
      )

      browser_ui <- function() {
        block_browser_ui(
          session$ns("browser"), board$board,
          prepend_to(trigger())
        )
      }

      observeEvent(trigger(), {
        show_sidebar(
          sidebar_id, title = "Prepend new block", ui = browser_ui(),
          owner = action
        )
      })

      observeEvent(added(), {
        res <- added()
        update(list(
          blocks = list(add = res$blocks),
          links = list(add = res$links)
        ))

        keep_or_hide_sidebar(
          sidebar_id, title = "Prepend new block", ui = browser_ui(),
          owner = action
        )
      })

      NULL
    },
    id = action
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
