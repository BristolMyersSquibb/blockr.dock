#' Open the menu for adding a panel to the dock.
#'
#' The "+" menu (Blockr.menu, add-block-menu.js), listing the blocks and
#' extensions not yet shown in the dock: mark, title and the block type as
#' meta text. A pick is `add_dock_panel_pick`. If none are available,
#' either triggers `suggest_new` or notifies the user.
#'
#' @param dock Dock proxy.
#' @param board Reactive board state.
#' @param suggest_new If truthy, called when no panels are available
#'   (used to prompt adding a new block).
#' @param panels Currently visible panels (auto-detected if `NULL`).
#' @param at Where the menu opens (see [new_action()]).
#' @param session Shiny session.
#'
#' @noRd
suggest_panels_to_add <- function(
  dock,
  board,
  suggest_new = FALSE,
  panels = NULL,
  at = NULL,
  session = get_session()
) {
  ns <- session$ns

  if (is.null(panels)) {
    panels <- dock_panel_ids(dock$proxy)
  }

  stopifnot(is.list(panels), all(lgl_ply(panels, is_dock_panel_id)))

  blk_opts <- setdiff(
    board_block_ids(board$board),
    as_obj_id(panels[lgl_ply(panels, is_block_panel_id)])
  )

  ext_opts <- setdiff(
    dock_ext_ids(board$board),
    as_obj_id(panels[lgl_ply(panels, is_ext_panel_id)])
  )

  items <- add_panel_menu_items(board$board, blk_opts, ext_opts)

  if (length(items)) {
    session$sendCustomMessage(
      "blockr-add-panel-menu",
      list(
        pick = ns("add_dock_panel_pick"),
        caption = "Show in this view",
        at = at,
        items = items
      )
    )
  } else if (!isFALSE(suggest_new)) {
    suggest_new(TRUE)
  } else if (
    length(board_block_ids(board$board)) == 0L &&
      length(dock_ext_ids(board$board)) == 0L
  ) {
    notify("The board has no blocks yet. Add a new block to get started.")
  } else {
    notify(
      paste(
        "All blocks and extensions are already in this view.",
        "Add a new block to the board first."
      )
    )
  }
}

add_panel_menu_items <- function(board, blk_ids, ext_ids) {

  blocks <- list()

  if (length(blk_ids)) {
    blks <- board_blocks(board)[blk_ids]
    meta <- blks_metadata(blks)
    blocks <- lapply(seq_along(blk_ids), function(i) {
      list(
        label = block_name(blks[[i]]),
        value = as.character(as_block_panel_id(blk_ids[[i]])),
        meta = meta$name[i],
        keywords = blk_ids[[i]],
        mark = list(icon = meta$icon[i], category = meta$category[i])
      )
    })
  }

  exts <- list()

  if (length(ext_ids)) {
    all_exts <- as.list(dock_extensions(board))
    exts <- lapply(ext_ids, function(ext_id) {
      list(
        label = extension_name(all_exts[[ext_id]]),
        value = as.character(as_ext_panel_id(ext_id)),
        keywords = ext_id,
        mark = list(
          icon = extension_default_icon(),
          category = "uncategorized"
        )
      )
    })
  }

  if (length(blocks) && length(exts)) {
    return(c(
      list(list(title = "Blocks")), blocks,
      list(list(title = "Extensions")), exts
    ))
  }

  c(blocks, exts)
}

#' Default icon for extensions in the panel picker.
#' @noRd
extension_default_icon <- function() {
  as.character(bsicons::bs_icon("gear"))
}
