link_sidebar_body <- function(ns, board, block_id) {
  board_blocks <- board_blocks(board)

  stopifnot(is_string(block_id), block_id %in% names(board_blocks))

  board_blocks <- board_blocks[names(board_blocks) != block_id]

  selection_id <- "create_link"

  avail <- map(
    block_input_select,
    board_blocks,
    names(board_blocks),
    MoreArgs = list(links = board_links(board), mode = "inputs")
  )

  if (sum(lengths(avail)) == 0L) {
    # Pure UI builder: signal "nothing to render" with NULL and let the
    # caller decide whether to warn (initial open) or close silently
    # (post-confirm chain path).
    return(NULL)
  }

  visible_fields <- list(
    board_select(
      id = ns(selection_id),
      blocks = board_blocks[lgl_ply(avail, has_length)],
      label = "Select target block",
      choices = NULL
    )
  )

  toggle <- toggle_button(
    ns("link-advanced-options"),
    ns("link-advanced-toggle")
  )

  advanced_fields <- list(
    block_input_select(
      inputId = ns("add_link_input"),
      label = "Block input"
    ),
    textInput(
      ns("add_link_id"),
      label = "Link ID",
      value = rand_names(board_link_ids(board))
    )
  )

  advanced_section <- div(
    id = ns("link-advanced-options"),
    tagList(advanced_fields)
  )

  tagList(
    css_modal_advanced(ns("link-advanced-options")),
    visible_fields,
    toggle,
    advanced_section,
    confirm_button(
      inputId = ns("add_link_confirm"),
      label = "Add Link"
    )
  )
}
