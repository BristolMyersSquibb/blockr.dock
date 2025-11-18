#' @export
block_ui.dock_board <- function(id, x, blocks = NULL, edit_ui = NULL, ...) {
  stopifnot(is_string(id))

  if (is.null(blocks)) {
    blocks <- board_blocks(x)
  }

  stopifnot(is_blocks(blocks))

  map(
    block_card,
    blocks,
    names(blocks),
    MoreArgs = list(
      plugin = edit_ui,
      board = x,
      board_ns = NS(id)
    )
  )
}

block_card <- function(blk, blk_id, plugin, board, board_ns) {
  blk_srv_id <- board_ns(paste0("block_", blk_id))

  edit_ui <- plugin_ui(plugin)

  card_tag <- div(
    class = "card",
    width = "100%",
    id = board_ns(as_block_handle_id(blk_id)),
    edit_ui(
      NS(blk_srv_id, plugin_id(plugin)),
      blk,
      blk_id,
      expr_ui(blk_srv_id, blk),
      block_ui(blk_srv_id, blk)
    )
  )

  tagAppendAttributes(card_tag, class = "border border-0 shadow-none")
}

show_hide_block_dep <- function() {
  htmltools::htmlDependency(
    "show-hide-block",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "show-hide-block.js"
  )
}

#' @export
remove_block_ui.dock_board <- function(
  id,
  x,
  blocks,
  dock,
  ...,
  session = get_session()
) {
  stopifnot(is.character(blocks), all(blocks %in% board_block_ids(x)))

  for (blk in blocks) {
    if (as_block_panel_id(blk) %in% block_panel_ids(dock$proxy)) {
      hide_block_panel(blk, proxy = dock$proxy)
    }

    removeUI(
      as_block_handle_id(id),
      immediate = TRUE,
      session = session
    )
  }

  invisible(x)
}

#' @export
insert_block_ui.dock_board <- function(
  id,
  x,
  blocks,
  dock,
  ...,
  session = get_session()
) {
  stopifnot(is_blocks(blocks))

  for (i in names(blocks)) {
    insertUI(
      paste0("#", id, "-blocks_offcanvas"),
      "beforeEnd",
      block_ui(id, x, blocks[i], ...)[[1L]],
      immediate = TRUE,
      session = session
    )

    show_block_panel(blocks[i], determine_panel_pos(dock), dock$proxy)
  }

  invisible(x)
}

show_block_panel <- function(block, add_panel = TRUE, proxy = dock_proxy()) {
  if (isTRUE(add_panel)) {
    add_block_panel(block, proxy = proxy)
  } else if (isFALSE(add_panel)) {
    select_block_panel(block, proxy)
  } else {
    add_block_panel(block, position = add_panel, proxy = proxy)
  }

  show_block_ui(block, proxy$session)

  invisible(NULL)
}

hide_block_panel <- function(id, rm_panel = TRUE, proxy = dock_proxy()) {
  hide_block_ui(id, proxy$session)

  if (isTRUE(rm_panel)) {
    remove_block_panel(id, proxy)
  }

  invisible(NULL)
}

hide_block_ui <- function(id, session) {
  ns <- session$ns

  bid <- ns(as_block_handle_id(id))
  oid <- paste0(ns("blocks_offcanvas"), " .offcanvas-body")

  log_debug("hiding block {bid} in {oid}")

  move_dom_element(paste0("#", bid), paste0("#", oid), session)
}

show_block_ui <- function(id, session) {
  ns <- session$ns

  bid <- ns(as_block_handle_id(id))
  pid <- paste(dock_id(ns), as_block_panel_id(id), sep = "-")

  log_debug("showing block {bid} in panel {pid}")

  move_dom_element(paste0("#", bid), paste0("#", pid), session)
}
