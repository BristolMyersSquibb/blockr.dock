add_stack_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      sidebar_id <- NS(isolate(board$board_id), "actions_sidebar")
      # Pass the board as a reactive: the menu validates the committed
      # spec (id / name / colour) itself and keeps an open panel in sync
      # with the board (cards added / removed live), so this handler is a
      # thin adapter - no dock-side validators, no manual refresh.
      committed <- blockr.ui::stack_menu_server(
        "menu", board = reactive(board$board)
      )

      menu_ui <- function() {
        blockr.ui::stack_menu_ui(session$ns("menu"), board$board)
      }

      observeEvent(trigger(), {
        blockr.ui::show_sidebar(
          sidebar_id, title = "Create new stack", ui = menu_ui()
        )
      })

      observeEvent(committed(), {
        # The menu returns a `blockr.core` stacks object (id-keyed, colour
        # carried as an attribute); `as_dock_stacks()` re-classes it to
        # `dock_stack` preserving that colour. No reshaping here.
        update(list(stacks = list(add = as_dock_stacks(committed()))))

        # `update()` only sets a reactiveVal; the board state is mutated
        # on the next reactive flush, so defer the sidebar rebuild until
        # after the flush. Otherwise `menu_ui()` reads `board$board` from
        # the pre-add snapshot and re-suggests the just-used stack id.
        # `onFlushed` runs outside a reactive context, hence `isolate()`.
        session$onFlushed(
          function() {
            isolate(
              blockr.ui::keep_or_hide_sidebar(
                sidebar_id, title = "Create new stack", ui = menu_ui()
              )
            )
          },
          once = TRUE
        )
      })

      NULL
    },
    id = "add_stack_action"
  )
}

edit_stack_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      sidebar_id <- NS(isolate(board$board_id), "actions_sidebar")
      committed <- blockr.ui::stack_menu_server(
        "menu",
        board = reactive(board$board),
        target = reactive(trigger())
      )

      menu_ui <- function() {
        blockr.ui::stack_menu_ui(
          session$ns("menu"), board$board, target = trigger()
        )
      }

      # The sidebar title carries the stack identifier so users always
      # see which stack they're editing (the menu body no longer prints
      # a subtitle of its own).
      sidebar_title <- function() paste0("Edit stack ", trigger())

      observeEvent(trigger(), {
        blockr.ui::show_sidebar(
          sidebar_id, title = sidebar_title(), ui = menu_ui()
        )
      })

      # Close the sidebar the moment the edited stack leaves the board
      # (removed elsewhere): editing a stack that no longer exists makes
      # no sense, so don't wait for an "Update" click. Guarded on the
      # sidebar being open. (If another action had since taken over the
      # shared slot this could close that form too - a rare
      # edit-then-switch-then-remove sequence; revisit with sidebar
      # ownership if it ever bites.)
      observeEvent(board$board, {
        id <- trigger()
        # No-op unless an edit is actually in progress (a valid stack id);
        # otherwise `trigger()` is NULL / empty (e.g. while another action
        # mutates the board) and the membership test would error.
        if (length(id) == 1L && !is.na(id) && nzchar(id) &&
              !id %in% board_stack_ids(board$board) &&
              isTRUE(blockr.ui::sidebar_state(sidebar_id)$open)) {
          blockr.ui::hide_sidebar(sidebar_id)
        }
      }, ignoreInit = TRUE)

      observeEvent(committed(), {
        id <- trigger()

        # Safety net for a race (board change not yet observed when the
        # user clicks): committing for a stack that's gone would error
        # (the `mod` update and the post-commit `menu_ui()` rebuild both
        # look it up). Bail and close unless `id` is a present stack.
        if (!(length(id) == 1L && !is.na(id) && nzchar(id) &&
                id %in% board_stack_ids(board$board))) {
          blockr.ui::hide_sidebar(sidebar_id)
          return()
        }

        # Re-class to `dock_stack` first (preserving the carried colour),
        # so the `stack_*()` accessors read the real values - on a bare
        # core `stack`, `stack_color()` returns a placeholder.
        stk <- as_dock_stacks(committed())[[id]]

        # `mod` entries are partial-arg deltas applied via `update_stack()`
        # on the blockr.core side (a full `stacks` object trips
        # `board_update_stacks_mod_entry_invalid`), so unpack the stack
        # into its fields. Reserved key `blocks` replaces the member ids;
        # the rest update the named attributes.
        update(list(
          stacks = list(
            mod = set_names(
              list(list(
                blocks = stack_blocks(stk),
                name = stack_name(stk),
                color = stack_color(stk)
              )),
              id
            )
          )
        ))

        # Defer the sidebar rebuild to the next reactive flush so
        # `menu_ui()` reads the merged board state. Without this, the
        # rebuilt form shows the pre-edit selection (e.g. a block the
        # user just removed still appears selected).
        session$onFlushed(
          function() {
            isolate(
              blockr.ui::keep_or_hide_sidebar(
                sidebar_id, title = sidebar_title(), ui = menu_ui()
              )
            )
          },
          once = TRUE
        )
      })

      NULL
    },
    id = "edit_stack_action"
  )
}

remove_stack_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      observeEvent(
        trigger(),
        update(list(stacks = list(rm = trigger())))
      )
      NULL
    },
    id = "remove_stack_action"
  )
}
