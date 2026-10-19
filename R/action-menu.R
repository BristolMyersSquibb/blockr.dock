# The "+" menu that adding, appending, prepending and inserting a block open.
# A pick sends the block browser's commit with the block type alone, so
# block_browser_server() builds the block, generates its id and resolves the
# link's port.

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
          list(list(title = category)),
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
# its `browser` module's input.
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
