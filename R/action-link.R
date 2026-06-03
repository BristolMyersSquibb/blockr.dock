add_link_action <- function(trigger, board, update, ...) {

  new_action(
    function(input, output, session) {

      sidebar_id <- NS(isolate(board$board_id), "actions_sidebar")
      committed <- blockr.ui::link_menu_server("menu")

      menu_ui <- function() {
        blockr.ui::link_menu_ui(
          session$ns("menu"), board$board, anchor = trigger()
        )
      }

      # The sidebar title carries the anchor so users always see which
      # block they're connecting; the wording reads correctly whichever
      # direction the card lives in (OUTGOING / INCOMING).
      sidebar_title <- function() paste0("Connect ", trigger())

      observeEvent(trigger(), {
        # The link menu owns its own empty-state, so there's no pre-flight
        # NULL-check / notification here anymore: a block that can't be
        # linked still opens the sidebar with an in-place empty message.
        blockr.ui::show_sidebar(
          sidebar_id, title = sidebar_title(), ui = menu_ui()
        )
      })

      observeEvent(committed(), {
        spec <- committed()

        if (!valid_link_id(spec$link_id, board$board, session)) return()

        links <- board_links(board$board)
        trg_blk <- board_blocks(board$board)[[spec$target]]

        # The menu only renders a port picker for finite-arity targets
        # with > 1 free slot, so `spec$block_input` arrives NULL for
        # arity-1 and variadic targets. Resolve the slot ourselves in
        # those cases: the first free named input, or - for variadic
        # targets - a freshly generated slot, both via the same helper.
        chosen_input <- spec$block_input
        if (is.null(chosen_input) || !nzchar(chosen_input)) {
          chosen_input <- block_input_select(
            trg_blk, spec$target, links, mode = "inputs"
          )[1L]
        }

        new_lnk <- new_link(
          from = spec$source,
          to = spec$target,
          input = chosen_input
        )

        update(
          list(
            links = list(
              add = as_links(set_names(list(new_lnk), spec$link_id))
            )
          )
        )

        # Multi-link session: keep the menu open and push a live
        # eligibility update so the just-wired card drops client-side
        # without a full re-render. `update()` only sets a reactiveVal,
        # so the board mutates on the next reactive flush - defer the
        # pool recompute until after the flush, otherwise `board$board`
        # is still the pre-link snapshot. `onFlushed` runs outside a
        # reactive context, hence `isolate()`.
        anchor <- trigger()
        session$onFlushed(
          function() {
            isolate({
              pools <- blockr.ui::link_eligible_pools(board$board, anchor)

              if (!length(pools$outgoing) && !length(pools$incoming)) {
                blockr.ui::hide_sidebar(sidebar_id)
                return()
              }

              session$sendInputMessage(
                session$ns("menu-commit"),
                list(
                  type = "pool-update",
                  eligible = list(
                    outgoing = pools$outgoing,
                    incoming = pools$incoming
                  ),
                  free_inputs = pools$free_inputs,
                  link_id_seed = rand_names(board_link_ids(board$board))
                )
              )
            })
          },
          once = TRUE
        )
      })

      NULL
    },
    id = "add_link_action"
  )
}

remove_link_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      observeEvent(
        trigger(),
        update(list(links = list(rm = trigger())))
      )
      NULL
    },
    id = "remove_link_action"
  )
}

# ---- shared validators -------------------------------------------------

# blockr.ui's link_menu_ui seeds a fresh link id via `rand_names()`
# against the current board, so the typical case is a no-op pass. We
# still guard against an empty / duplicate id the user could type in.
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
