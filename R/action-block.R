add_block_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      sidebar_id <- NS(isolate(board$board_id), "actions_sidebar")
      added <- blockr.ui::block_browser_server("browser")

      browser_ui <- function() {
        blockr.ui::block_browser_ui(session$ns("browser"), board$board)
      }

      observeEvent(trigger(), {
        log_debug("showing add block action sidebar")
        blockr.ui::show_sidebar(
          sidebar_id, title = "Add new block", ui = browser_ui()
        )
      })

      observeEvent(added(), {
        spec <- added()

        if (!valid_block_id(spec$id, board$board, session)) return()

        new_blk <- build_block_from_spec(spec, board$board, session)
        if (is.null(new_blk)) return()

        update(list(
          blocks = list(
            add = as_blocks(set_names(list(new_blk), spec$id))
          )
        ))

        blockr.ui::keep_or_hide_sidebar(
          sidebar_id, title = "Add new block", ui = browser_ui()
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
      added <- blockr.ui::block_browser_server("browser")

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
        spec <- added()

        if (!valid_block_id(spec$id, board$board, session)) return()
        if (!valid_link_id(spec$link_id, board$board, session)) return()

        new_blk <- build_block_from_spec(spec, board$board, session)
        if (is.null(new_blk)) return()

        # The browser hides the input-port picker when the new block
        # has a single named input or is variadic. `block_input_select`
        # resolves both cases uniformly: it returns the named slot, or
        # a freshly-generated integer slot for variadic blocks.
        block_input <- spec$block_input
        if (is.null(block_input) || !nzchar(block_input)) {
          block_input <- block_input_select(
            new_blk,
            block_id = spec$id,
            links = board_links(board$board),
            mode = "inputs"
          )[1L]
        }

        new_lnk <- new_link(
          from = trigger(),
          to = spec$id,
          input = block_input
        )

        update(list(
          blocks = list(
            add = as_blocks(set_names(list(new_blk), spec$id))
          ),
          links = list(
            add = as_links(set_names(list(new_lnk), spec$link_id))
          )
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
      added <- blockr.ui::block_browser_server("browser")

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
        spec <- added()

        if (!valid_block_id(spec$id, board$board, session)) return()
        if (!valid_link_id(spec$link_id, board$board, session)) return()

        new_blk <- build_block_from_spec(spec, board$board, session)
        if (is.null(new_blk)) return()

        # Target input port: user pick (arity > 1) or default to the
        # first available slot, mirroring the previous prepend behaviour.
        target_input <- spec$target_input
        if (is.null(target_input) || !nzchar(target_input)) {
          inps <- block_input_select(
            board_blocks(board$board)[[trigger()]],
            trigger(),
            board_links(board$board),
            mode = "inputs"
          )
          target_input <- inps[1L]
        }

        new_lnk <- new_link(
          from = spec$id,
          to = trigger(),
          input = target_input
        )

        update(list(
          blocks = list(
            add = as_blocks(set_names(list(new_blk), spec$id))
          ),
          links = list(
            add = as_links(set_names(list(new_lnk), spec$link_id))
          )
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

# ---- helpers shared by add / append / prepend --------------------------

# The block browser pre-seeds unique ids via `rand_names()` against the
# current board, so the typical case here is a no-op pass. We still
# guard against an empty / duplicate id that the user could type into
# the per-card form.
valid_block_id <- function(id, board, session) {
  if (is.null(id) || !nzchar(id) || id %in% board_block_ids(board)) {
    notify(
      "Please choose a valid block ID.",
      type = "warning",
      session = session
    )
    return(FALSE)
  }
  TRUE
}

valid_link_id <- function(id, board, session) {
  if (is.null(id) || !nzchar(id) || id %in% board_link_ids(board)) {
    notify(
      "Please choose a valid link ID.",
      type = "warning",
      session = session
    )
    return(FALSE)
  }
  TRUE
}

# Instantiate via the registry by uid (spec$type). Apply the user's
# title if supplied; otherwise keep `create_block_with_name`'s auto-
# numbered name so "Dataset 1" / "Dataset 2" still works on repeat adds.
build_block_from_spec <- function(spec, board, session) {
  blk <- tryCatch(
    create_block_with_name(
      spec$type,
      chr_ply(board_blocks(board), block_name)
    ),
    error = function(e) {
      notify(
        paste0("Could not create block: ", conditionMessage(e)),
        type = "error",
        session = session
      )
      NULL
    }
  )

  if (is.null(blk)) return(NULL)

  if (!is.null(spec$title) && nzchar(spec$title)) {
    block_name(blk) <- spec$title
  }

  blk
}
