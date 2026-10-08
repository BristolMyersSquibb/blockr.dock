# What the four block actions share: adding, appending, prepending and
# inserting a block. A gesture opens the block actions' menu where it
# happened (add-block-menu.R). Where the action holds the sidebar already, it
# shows the block browser there for the new target instead, which is what
# keeps a pinned browser following the DAG's selection. The menu's tool opens
# the browser on what was typed in the menu. A pick from either commits
# through block_browser_server(), and `apply` takes the value it builds.
#
# Both `target` and `caption` read the trigger's value. Once a pick is
# applied, the browser closes unless it is pinned: a pinned one stays, and is
# shown afresh where `refresh` says the commit changed what the form offers,
# as a prepend takes up one of the target's inputs. That waits for the board
# to take the commit in, which happens after `apply` returns, so the form
# reads the target's inputs as they are then. A target that leaves the board,
# the link an insert split among them, closes the browser, since there is
# nothing left to add to.
serve_block_action <- function(mode, trigger, board, target, caption, apply,
                               refresh = FALSE, session = get_session()) {

  sidebar_id <- NS(isolate(board$board_id), "actions_sidebar")
  input <- session$input

  added <- block_browser_server(
    "browser",
    board = reactive(board$board),
    target = reactive(target(trigger()))
  )

  show_browser <- function(query = "") {
    show_sidebar(
      sidebar_id,
      title = caption(board$board, trigger()),
      ui = block_browser_ui(
        session$ns("browser"), board$board, target(trigger()), query
      ),
      session = session
    )
  }

  observeEvent(trigger(), {
    if (owns_open_sidebar(sidebar_id, session)) {
      show_browser()
    } else {
      open_add_block_menu(
        mode, caption(board$board, trigger()), at = trigger_at(trigger),
        session = session
      )
    }
  })

  observeEvent(input$expand, show_browser(coal(input$expand$query, "")))

  stale <- reactiveVal(FALSE)

  observeEvent(added(), {

    apply(added())

    if (owns_open_sidebar(sidebar_id, session)) {
      if (!isTRUE(sidebar_state(sidebar_id, session)$pinned)) {
        hide_sidebar(sidebar_id, session)
      } else {
        stale(refresh)
      }
    }
  })

  observeEvent(
    board$board,
    {
      if (owns_open_sidebar(sidebar_id, session)) {
        if (!target_on_board(target(trigger()), board$board)) {
          hide_sidebar(sidebar_id, session)
        } else if (stale()) {
          show_browser()
        }
      }
      stale(FALSE)
    },
    ignoreInit = TRUE
  )

  invisible(NULL)
}

# Whether what a block action adds to is still there: the block an append or
# prepend links to, the link an insert splits. Adding a block needs nothing.
target_on_board <- function(target, board) {

  if (is.null(target) || is.null(target$id)) {
    return(TRUE)
  }

  ids <- if (target$mode == "insert") {
    board_link_ids(board)
  } else {
    board_block_ids(board)
  }

  target$id %in% ids
}

add_block_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      serve_block_action(
        "add", trigger, board,
        target = function(x) NULL,
        caption = function(board, x) "Add a block",
        # A ready-to-apply `blocks` object, id-keyed, the block built and
        # named server-side.
        apply = function(res) update(list(blocks = list(add = res)))
      )
      NULL
    },
    id = "add_block_action"
  )
}

append_block_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      serve_block_action(
        "append", trigger, board,
        target = append_to,
        caption = function(board, id) {
          paste("Append to", block_label(board, id))
        },
        # The block and its link, with the link's input already resolved.
        apply = function(res) {
          update(
            list(
              blocks = list(add = res$blocks),
              links = list(add = res$links)
            )
          )
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
      serve_block_action(
        "prepend", trigger, board,
        target = prepend_to,
        caption = function(board, id) {
          paste("Prepend to", block_label(board, id))
        },
        apply = function(res) {
          update(
            list(
              blocks = list(add = res$blocks),
              links = list(add = res$links)
            )
          )
        },
        refresh = TRUE
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
