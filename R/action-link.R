# "Connect to…": the blocks this block can send its output to, and the blocks
# it can take an input from, in one menu. A block with more than one free
# input asks which in a second menu. The link id is generated.
add_link_action <- function(trigger, board, update, ...) {

  new_action(
    function(input, output, session) {

      first <- NULL

      connect_menu <- function() {

        brd <- board$board
        anchor <- trigger()
        pools <- link_eligible_pools(brd, anchor)

        items <- c(
          if (length(pools$outgoing)) {
            c(
              list(menu_title("Send to")),
              block_menu_rows(
                brd, pools$outgoing, value = function(id) paste0("to:", id)
              )
            )
          },
          if (length(pools$incoming)) {
            c(
              list(menu_title("Take from")),
              block_menu_rows(
                brd, pools$incoming, value = function(id) paste0("from:", id)
              )
            )
          }
        )

        if (!length(items)) {
          items <- list(
            menu_row("No block can be linked to it", NULL, disabled = TRUE)
          )
        }

        first <<- open_action_menu(
          at = trigger_at(trigger),
          caption = paste("Connect", block_label(brd, anchor)),
          items = items,
          session = session
        )
      }

      observeEvent(trigger(), connect_menu())

      observeEvent(input$pick, {

        brd <- board$board
        anchor <- trigger()
        val <- pick_value(input$pick)
        req(is_string(val), anchor %in% board_block_ids(brd))

        parts <- strsplit(val, ":", fixed = TRUE)[[1L]]
        dir <- parts[1L]
        other <- parts[2L]
        req(other %in% board_block_ids(brd))

        from <- if (dir == "to") anchor else other
        to <- if (dir == "to") other else anchor

        slot <- if (length(parts) > 2L) parts[3L]

        if (is.null(slot)) {

          free <- free_named_inputs(
            board_blocks(brd)[[to]], to, as.data.frame(board_links(brd))
          )

          # A choice to make: which input of the target takes it.
          if (length(free) > 1L) {
            open_action_menu(
              at = trigger_at(trigger),
              caption = paste0("Into which input of ", block_label(brd, to), "?"),
              items = lapply(
                free,
                function(x) menu_row(x, paste(dir, other, x, sep = ":"))
              ),
              back = first,
              session = session
            )
            return()
          }

          slot <- resolve_free_input(board_blocks(brd)[[to]], to, board_links(brd))
        }

        lnk <- new_link(from = from, to = to, input = slot)
        update(
          list(links = list(add = as_links(set_names(list(lnk), seed_link_id(brd)))))
        )
      })

      NULL
    },
    id = "add_link_action"
  )
}

# The link's menu: what can be done to one link. Insert a block into it;
# rename its input (a target that takes any number of inputs) or move it to
# another free input (a target with named inputs); change the block it comes
# from; remove it. Rename, Move and Change source open a second menu, from
# which Escape comes back here.
edit_link_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {

      first <- NULL

      inserted <- block_browser_server(
        "browser",
        board = reactive(board$board),
        target = reactive(insert_into(trigger()))
      )

      link_menu <- function() {

        brd <- board$board
        lid <- trigger()
        row <- edit_link_row(brd, lid)
        req(row)

        to_blk <- board_blocks(brd)[[row$to]]
        variadic <- is.na(block_arity(to_blk))
        others <- if (!variadic) {
          free_named_inputs(
            to_blk, row$to, as.data.frame(links_without(brd, lid))
          )
        }
        others <- setdiff(others, row$input)

        items <- c(
          list(menu_row("Insert a block here", "insert", icon = "plus")),
          if (variadic) list(menu_row("Rename input", "rename")),
          if (length(others) == 1L) {
            list(menu_row("Move to input", paste0("move:", others), meta = others))
          } else if (length(others) > 1L) {
            list(menu_row("Move to input", "move"))
          },
          list(
            menu_row("Change source", "source"),
            menu_divider(),
            menu_row("Remove link", "remove", icon = "trash", danger = TRUE)
          )
        )

        first <<- open_action_menu(
          at = trigger_at(trigger),
          head = list(
            title = paste(
              block_label(brd, row$from), "→", block_label(brd, row$to)
            ),
            text = link_input_text(row$input)
          ),
          items = items,
          session = session
        )
      }

      observeEvent(trigger(), link_menu())

      # A step that needs a link still on the board; one removed in the
      # meantime ends the menu.
      current_row <- function() {
        row <- edit_link_row(board$board, trigger())
        req(row)
        row
      }

      observeEvent(input$pick, {

        brd <- board$board
        lid <- trigger()
        row <- current_row()
        pick <- input$pick

        if (!is.null(pick_field(pick))) {
          rename_link_input(brd, lid, row, pick_field(pick), update, session)
          return()
        }

        val <- pick_value(pick)
        req(is_string(val))
        step <- pick_step(val)
        arg <- pick_arg(val)

        switch(
          step,
          insert = open_add_block_menu(
            "insert",
            insert_caption(brd, lid),
            at = trigger_at(trigger),
            board = brd,
            session = session
          ),
          remove = update(list(links = list(rm = lid))),
          rename = open_action_menu(
            at = trigger_at(trigger),
            caption = paste0("Rename input of ", block_label(brd, row$to)),
            field = menu_field(
              row$input,
              placeholder = "unnamed",
              taken = setdiff(taken_input_names(brd, row$to), row$input),
              taken_msg = "Another input is called that",
              empty_ok = TRUE
            ),
            back = first,
            session = session
          ),
          move = if (is.null(arg)) {
            others <- setdiff(
              free_named_inputs(
                board_blocks(brd)[[row$to]], row$to,
                as.data.frame(links_without(brd, lid))
              ),
              row$input
            )
            open_action_menu(
              at = trigger_at(trigger),
              caption = paste0("Move to which input of ", block_label(brd, row$to), "?"),
              items = lapply(others, function(x) menu_row(x, paste0("move:", x))),
              back = first,
              session = session
            )
          } else {
            apply_link_change(lid, list(input = arg), brd, update, session)
          },
          source = if (is.null(arg)) {
            open_action_menu(
              at = trigger_at(trigger),
              caption = paste0("Feed ", block_label(brd, row$to), " from"),
              items = block_menu_rows(
                brd, link_source_ids(brd, lid, row),
                value = function(id) paste0("source:", id)
              ),
              back = first,
              session = session
            )
          } else {
            apply_link_change(lid, list(from = arg), brd, update, session)
          }
        )
      })

      observeEvent(inserted(), {
        upd <- insert_block_update(inserted(), trigger(), session)
        if (!is.null(upd)) update(upd)
      })

      NULL
    },
    id = "edit_link_action"
  )
}

link_input_text <- function(input) {
  if (length(input) && nzchar(input)) {
    paste("into", input)
  } else {
    "into an unnamed input"
  }
}

taken_input_names <- function(board, block_id) {
  lnks <- board_links(board)
  inp <- lnks[lnks$to == block_id]$input
  inp[nzchar(inp)]
}

# The blocks that can feed the link's input instead of its source: any but
# the target, its descendants (a cycle) and the current source.
link_source_ids <- function(board, link_id, row) {
  links_df <- as.data.frame(links_without(board, link_id))
  setdiff(
    board_block_ids(board),
    c(row$to, row$from, descendants_of(row$to, links_df))
  )
}

# Apply a change to one link after the same checks the edit form made.
apply_link_change <- function(link_id, delta, board, update, session) {
  row <- edit_link_row(board, link_id)
  spec <- utils::modifyList(row, delta)
  validate_edit_link_spec(spec, board, link_id, session)
  delta <- edit_link_delta(spec, board, link_id)
  if (length(delta)) {
    update(list(links = list(mod = set_names(list(delta), link_id))))
  }
  invisible()
}

rename_link_input <- function(board, link_id, row, name, update, session) {
  if (name %in% setdiff(taken_input_names(board, row$to), row$input)) {
    notify(
      "Another input on this block is called that.",
      type = "warning", session = session
    )
    return(invisible())
  }
  apply_link_change(link_id, list(input = name), board, update, session)
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

# Triggered with a link id like the other actions in this file, though what
# it commits is a block: an insert is scoped to the wire it splits, and the
# wire is what the user gestures at.
insert_block_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {

      # The catalogue is registry-based (any block that can receive from the
      # link's source); the menu's caption names the wire's two ends.
      added <- block_browser_server(
        "browser",
        board = reactive(board$board),
        target = reactive(insert_into(trigger()))
      )

      observeEvent(trigger(), {
        open_add_block_menu(
          "insert",
          insert_caption(board$board, trigger()),
          at = trigger_at(trigger),
          board = board$board,
          session = session
        )
      })

      observeEvent(added(), {
        upd <- insert_block_update(added(), trigger(), session)
        if (!is.null(upd)) update(upd)
      })

      NULL
    },
    id = "insert_block_action"
  )
}

# The update that puts a picked block into a link, or NULL (with a warning)
# when the link has gone between opening the menu and the pick.
#
# One update. `modify_board_links()` drops the split link before it adds, so
# the far end's slot is free by the time the second new link claims it, and
# `before` puts that link where the split one sat: an entry's argument
# position follows the board's link order, so a link merely appended would
# slide every sibling after it up a place. The anchor is resolved before `rm`
# is applied, which is what lets it name the link this same payload removes.
insert_block_update <- function(res, link_id, session) {

  # Applying the block alone would leave it stranded off the graph.
  if (!length(res$links)) {
    notify(
      "That link is no longer on the board.",
      type = "warning", session = session
    )
    return(NULL)
  }

  list(
    blocks = list(add = res$blocks),
    links = list(add = res$links, rm = link_id, before = res$before)
  )
}
