# The "+" menu (design system, "Picking a block"): adding, appending,
# prepending and inserting a block all open one menu, in place, instead of the
# block browser sidebar. Blockr.menu (blockr.ui) draws it: a caption saying
# what the pick does, a filter box, category titles, one row per block type
# with its mark, name and package badge. A pick sends the same commit the
# browser's cards sent (`<action>-browser-commit`, the block type and no ids),
# so block_browser_server() builds the block, generates its id and resolves
# the link's port exactly as before. add-block-menu.js opens the menu where
# the gesture happened.

# The rows for one flow: every registered block type for add and prepend,
# only the ones that can receive a link for append and insert (the filter
# browser_block_metas() applies). Built once per flow and registry, since
# for append and insert it instantiates each block to read its inputs; the
# key hashes the registry's entries, so registering a block anew rebuilds.
add_block_menu_items <- function(mode) {

  key <- paste(mode, rlang::hash(available_blocks()))

  if (!is.null(add_block_menu_cache[[key]])) {
    return(add_block_menu_cache[[key]])
  }

  groups <- category_groups(browser_block_metas(mode))

  items <- unlst(
    Map(
      function(category, metas) {
        c(
          list(list(title = upper_first(category))),
          lapply(metas, add_block_menu_item)
        )
      },
      names(groups),
      groups
    )
  )

  assign(key, items, envir = add_block_menu_cache)

  items
}

add_block_menu_cache <- new.env(parent = emptyenv())

upper_first <- function(x) {
  paste0(toupper(substr(x, 1L, 1L)), substring(x, 2L))
}

add_block_menu_item <- function(meta) {
  list(
    label = meta$name,
    type = meta$type,
    badge = meta$package,
    # The type id ("filter_block"), not the description: a description
    # mentions other blocks' words and would match half the list.
    keywords = meta$type,
    mark = list(
      icon = meta$icon,
      color = unname(blk_color(meta$category))
    )
  )
}

# Open the menu for one flow, at what the gesture named (`at`, see
# new_action()). `session` is the action module's session, so the commit
# lands on its `browser` module's input.
open_add_block_menu <- function(mode, caption, at = NULL,
                                session = get_session()) {
  session$sendCustomMessage(
    "blockr-add-block-menu",
    list(
      commit = session$ns(NS("browser", "commit")),
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

# "Insert between A and B", naming the wire's ends the way the user sees
# them; a link that has left the board gets the plain caption.
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

# The rows of the add-panel menu: the board's blocks not on the page (mark,
# title, the block type as meta text) and its extensions, each group under a
# title when both are there.
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
        mark = list(icon = meta$icon[i], color = meta$color[i])
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
          color = unname(blk_color("uncategorized"))
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
