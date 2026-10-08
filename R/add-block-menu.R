# The menu the block actions open: adding, appending, prepending and inserting
# a block. A pick sends the commit with the block type alone, so
# block_browser_server() builds the block, generates its id and resolves the
# link's input. Its tool opens the same list as the block browser in the
# sidebar, where the ids and the input are set before adding.

# The rows for one flow: every registered block type for add and prepend,
# only the ones that can receive a link for append and insert (the filter
# browser_block_metas() applies).
add_block_menu_items <- function(mode) {

  groups <- category_groups(block_metas(mode))

  unlst(
    Map(
      function(category, metas) {
        c(
          list(list(title = category)),
          lapply(metas, add_block_menu_item)
        )
      },
      names(groups),
      groups
    )
  )
}

add_block_menu_item <- function(meta) {
  list(
    label = meta$name,
    type = meta$type,
    badge = meta$package,
    # The type id ("filter_block"), not the description: a description
    # mentions other blocks' words and would match half the list.
    keywords = meta$type,
    mark = list(icon = meta$icon, category = meta$category)
  )
}

# Open the menu for one flow, at what the gesture named (`at`, see
# new_action()). The session is the action module's, so the commit lands on
# its `browser` module's input, and the tool on the action's `expand`.
open_add_block_menu <- function(mode, caption, at = NULL,
                                session = get_session()) {
  session$sendCustomMessage(
    "blockr-add-block-menu",
    list(
      commit = session$ns(NS("browser", "commit")),
      expand = session$ns("expand"),
      caption = caption,
      at = at,
      items = add_block_menu_items(mode)
    )
  )
}

add_block_menu_dep <- function() {
  htmltools::htmlDependency(
    "blockr-add-block-menu",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "add-block-menu.js"
  )
}

insert_caption <- function(board, link_id) {

  ends <- link_ends(board, link_id)

  if (is.null(ends)) {
    return("Insert a block")
  }

  paste(
    "Insert between", block_label(board, ends$from),
    "and", block_label(board, ends$to)
  )
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
