# Putting blocks into a stack. Triggered with block ids (a block's "Add to
# stack", a selection's "Stack selected"), it lists the board's stacks and
# "New stack"; a block leaves the stack it was in. Triggered with anything
# else (an older DAG's canvas "Create stack"), it lists the blocks in no
# stack to tick, and makes a stack of them.
add_stack_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {

      observeEvent(trigger(), {

        brd <- board$board
        ids <- trigger_block_ids(trigger(), brd)

        if (length(ids)) {
          open_action_menu(
            at = trigger_at(trigger),
            caption = paste("Add", blocks_label(brd, ids), "to"),
            items = c(
              stack_menu_rows(brd, setdiff(board_stack_ids(brd), stacks_holding_all(brd, ids))),
              if (length(board_stack_ids(brd))) list(menu_divider()),
              list(menu_row("New stack", "new", icon = "plus", quiet = TRUE))
            ),
            session = session
          )
          return()
        }

        open_action_menu(
          at = trigger_at(trigger),
          caption = "New stack of",
          items = block_menu_rows(brd, stack_eligible_blocks(brd)),
          multi = TRUE,
          session = session
        )
      })

      observeEvent(input$pick, {

        brd <- board$board
        ids <- trigger_block_ids(trigger(), brd)

        if (!is.null(input$pick$values)) {
          ids <- intersect(pick_values(input$pick), board_block_ids(brd))
          req(length(ids))
          update(new_stack_update(brd, ids))
          return()
        }

        val <- pick_value(input$pick)
        req(is_string(val), length(ids))

        if (identical(val, "new")) {
          update(new_stack_update(brd, ids))
        } else {
          stk <- pick_arg(val)
          req(stk %in% board_stack_ids(brd))
          update(join_stack_update(brd, stk, ids))
        }
      })

      NULL
    },
    id = "add_stack_action"
  )
}

# The stack's menu: Rename… and Colour… open a second menu, Blocks… lists
# the blocks to tick in or out of it, Dissolve stack keeps the blocks.
edit_stack_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {

      first <- NULL

      current <- function() {
        id <- trigger()
        req(is_string(id), id %in% board_stack_ids(board$board))
        id
      }

      observeEvent(trigger(), {

        brd <- board$board
        id <- current()
        stk <- board_stacks(brd)[[id]]

        first <<- open_action_menu(
          at = trigger_at(trigger),
          head = list(
            title = stack_name(stk),
            text = n_blocks(length(stack_blocks(stk)))
          ),
          items = list(
            menu_row("Rename…", "rename"),
            menu_row("Colour…", "colour"),
            menu_row("Blocks…", "blocks"),
            menu_divider(),
            menu_row("Dissolve stack", "dissolve", icon = "trash", danger = TRUE)
          ),
          session = session
        )
      })

      observeEvent(input$pick, {

        brd <- board$board
        id <- current()
        stk <- board_stacks(brd)[[id]]
        pick <- input$pick

        if (!is.null(pick_field(pick))) {
          name <- trimws(pick_field(pick))
          req(nzchar(name))
          update(stack_mod(id, list(name = name)))
          return()
        }

        if (!is.null(pick$values)) {
          ids <- intersect(pick_values(pick), board_block_ids(brd))
          update(stack_mod(id, list(blocks = ids)))
          return()
        }

        val <- pick_value(pick)
        req(is_string(val))

        switch(
          pick_step(val),
          rename = open_action_menu(
            at = trigger_at(trigger),
            caption = "Rename stack",
            field = menu_field(
              stack_name(stk), empty_msg = "A stack needs a name"
            ),
            back = first,
            session = session
          ),
          colour = if (is.null(pick_arg(val))) {
            open_action_menu(
              at = trigger_at(trigger),
              caption = paste("Colour of", stack_name(stk)),
              items = colour_menu_rows(brd, id),
              back = first,
              session = session
            )
          } else {
            col <- pick_arg(val)
            req(is_hex_color(col))
            update(stack_mod(id, list(color = col)))
          },
          blocks = open_action_menu(
            at = trigger_at(trigger),
            caption = paste("Blocks in", stack_name(stk)),
            items = block_menu_rows(
              brd,
              union(stack_blocks(stk), stack_eligible_blocks(brd)),
              checked = stack_blocks(stk)
            ),
            multi = TRUE,
            back = first,
            session = session
          ),
          dissolve = update(list(stacks = list(rm = id)))
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

# ---- helpers --------------------------------------------------------------

# The block ids a trigger names, or none: an older DAG fires add_stack with
# TRUE from the canvas. Ids sent from the browser as an array arrive as a
# list.
trigger_block_ids <- function(value, board) {
  value <- unlist(value)
  if (!is.character(value)) {
    return(character())
  }
  intersect(value, board_block_ids(board))
}

blocks_label <- function(board, ids) {
  if (length(ids) == 1L) block_label(board, ids) else paste(length(ids), "blocks")
}

n_blocks <- function(n) if (n == 1L) "1 block" else paste(n, "blocks")

# The stacks that already hold every one of `ids`: adding them there would
# change nothing.
stacks_holding_all <- function(board, ids) {
  stks <- board_stacks(board)
  names(stks)[lgl_ply(stks, function(s) all(ids %in% stack_blocks(s)))]
}

stack_square <- function() {
  paste0(
    "<svg viewBox=\"0 0 16 16\" fill=\"currentColor\" aria-hidden=\"true\">",
    "<rect x=\"2\" y=\"2\" width=\"12\" height=\"12\" rx=\"3\"/></svg>"
  )
}

stack_menu_rows <- function(board, ids) {
  stks <- board_stacks(board)
  lapply(
    ids,
    function(id) {
      menu_row(
        label = stack_name(stks[[id]]),
        value = paste0("stack:", id),
        meta = n_blocks(length(stack_blocks(stks[[id]]))),
        mark = list(icon = stack_square(), color = stack_color(stks[[id]])),
        keywords = id
      )
    }
  )
}

# The current colour, then colours the dock suggests next to the board's
# other stacks, then the browser's picker.
colour_menu_rows <- function(board, id) {
  stks <- board_stacks(board)
  cur <- stack_color(stks[[id]])
  others <- chr_ply(stks[setdiff(names(stks), id)], stack_color)
  sugg <- setdiff(toupper(suggest_new_colors(c(others, cur), n = 6L)), toupper(cur))

  swatch <- function(col, label, value) {
    menu_row(
      label, value,
      mark = list(icon = stack_square(), color = col),
      meta = if (!identical(label, col)) toupper(col)
    )
  }

  c(
    list(swatch(cur, "Current", paste0("colour:", cur)), menu_divider()),
    lapply(sugg, function(col) swatch(col, col, paste0("colour:", col))),
    list(
      menu_divider(),
      menu_row(
        "Custom…", "colour", quiet = TRUE,
        colour_picker = TRUE, colour = cur
      )
    )
  )
}

stack_mod <- function(id, delta) {
  list(stacks = list(mod = set_names(list(delta), id)))
}

# A block sits in one stack at most: the stacks the blocks leave, as mod
# entries.
leave_stacks <- function(board, ids, keep = NULL) {
  stks <- board_stacks(board)
  out <- list()
  for (sid in setdiff(names(stks), keep)) {
    mem <- stack_blocks(stks[[sid]])
    if (any(ids %in% mem)) {
      out[[sid]] <- list(blocks = setdiff(mem, ids))
    }
  }
  out
}

join_stack_update <- function(board, stack_id, ids) {
  mem <- stack_blocks(board_stacks(board)[[stack_id]])
  mod <- leave_stacks(board, ids, keep = stack_id)
  mod[[stack_id]] <- list(blocks = union(mem, ids))
  list(stacks = list(mod = mod))
}

# "Stack 1", "Stack 2", …: the first number no stack on the board is called.
next_stack_name <- function(board) {
  taken <- chr_ply(board_stacks(board), function(s) stack_name(s) %||% "")
  n <- 1L
  while (paste("Stack", n) %in% taken) n <- n + 1L
  paste("Stack", n)
}

new_stack_update <- function(board, ids) {
  stks <- board_stacks(board)
  col <- suggest_new_colors(chr_ply(stks, stack_color))
  stk <- new_dock_stack(blocks = ids, name = next_stack_name(board), color = col)
  upd <- list(stacks = list(add = as_stacks(set_names(list(stk), seed_stack_id(board)))))
  mod <- leave_stacks(board, ids)
  if (length(mod)) upd$stacks$mod <- mod
  upd
}
