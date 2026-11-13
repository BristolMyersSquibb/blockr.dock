#' Dock board
#'
#' An app script builds a board for the dock with `new_dock_board()`, which
#' returns the board that [blockr.core::new_board()] builds.
#' The board holds the blocks, links, stacks, board option values and plugins
#' that the script gives.
#' It also holds the dock's state, as the state of the dock plugin (see
#' [new_dock_plugin()]): the views, written with [dock_view()], the folders,
#' written with [dock_folder()], the active view and the sections that each
#' block's card shows.
#'
#' The function takes the board's plugins in `plugins`, each made by its
#' package's constructor, as `new_dag_plugin()` makes the DAG.
#' The script may give a plugin a name there, as
#' `plugins = list(analysis = new_dag_plugin())`.
#' A grid then names the plugin's panel by that name, as `plg("analysis")`.
#' The board holds the plugins without those names, under the id of each
#' plugin's class, and so does the dock's state.
#'
#' A bare id in a grid names the panel of a block where the board holds a
#' block of that id, and otherwise the panel of the plugin of that name in
#' `plugins`.
#' The function refuses, naming each, a grid that names a block that the board
#' lacks, and an entry of `visible` for such a block.
#' It refuses no reference to a plugin, since a grid may name the class of a
#' plugin that [blockr.core::serve()] adds to the app.
#' A stack has no colour unless the script gives it one, and the dock then
#' draws it in a colour of its own.
#' The board holds the value of a board option only where the script gives
#' one, as `options = new_compact_option(TRUE)` does.
#'
#' The readers take a board, such as one that `new_dock_board()` builds.
#' The function `board_views()` reads its views, each as [dock_view()] writes
#' it, with its grid.
#' Each grid names its panels with [blk()] and [plg()], the latter by the id of
#' the plugin's class.
#' So a script can build one board from another, as
#' `new_dock_board(views = board_views(board)["main"])` does.
#' The function `active_view()` reads the id of the active view.
#' The function `dock_plugins()` reads the board's plugins other than the dock,
#' by the id of each plugin's class, which `new_dock_board()` takes in
#' `plugins`.
#'
#' @param blocks,links,stacks,options,id,name The board's blocks, links,
#'   stacks, board option values, id and name, as [blockr.core::new_board()]
#'   takes them.
#' @param plugins The board's plugins besides the dock, each made by its
#'   package's constructor, under the name by which a grid names its panel,
#'   if any.
#' @param views,folders,active,visible The dock's views and folders, the view
#'   that the board opens on, and the sections that each card shows, as
#'   [new_dock_plugin()] takes them.
#' @param x A board.
#'
#' @return The function `new_dock_board()` returns a `board`.
#'   The function `board_views()` returns a list of views, by id, each a
#'   `dock_view`, and `active_view()` returns the id of the active view, or
#'   `NULL` where the board holds no view.
#'   The function `dock_plugins()` returns a list of plugins, by the id of
#'   each plugin's class.
#'
#' @examples
#' board <- new_dock_board(
#'   blocks = c(
#'     a = blockr.core::new_dataset_block("iris"),
#'     b = blockr.core::new_head_block()
#'   ),
#'   links = list(from = "a", to = "b"),
#'   plugins = list(edit = blockr.core::new_edit_board_plugin()),
#'   views = list(
#'     main = dock_view(dock_grid("a", "b", rail(plg("edit"))), name = "Main"),
#'     side = dock_view("b")
#'   ),
#'   active = "side",
#'   visible = list(b = "outputs")
#' )
#'
#' names(board_views(board))
#' active_view(board)
#' view_name(board_views(board)[["side"]])
#' names(dock_plugins(board))
#'
#' @rdname dock
#' @export
new_dock_board <- function(blocks = list(), links = list(), stacks = list(),
                           plugins = list(), views = NULL, folders = list(),
                           active = NULL, visible = list(), options = list(),
                           id = NULL, name = NULL) {

  blocks <- as_blocks(blocks)
  plugins <- named_plugins(plugins)

  dock <- new_dock_plugin(
    resolve_views(check_nav_list(views, "view"), names(blocks), plugins),
    folders,
    active,
    visible
  )

  check_board_blocks(plugin_state(dock), names(blocks))

  new_board(
    blocks = blocks,
    links = links,
    stacks = stacks,
    options = options,
    plugins = c(list(dock), unname(plugins)),
    id = id,
    name = name
  )
}

#' @rdname dock
#' @export
board_views <- function(x) {
  lapply(
    board_dock_state(x)[["views"]],
    function(view) {
      new_dock_view(
        grid = grid_from_state(view[["grid"]]),
        name = view[["name"]],
        folder = view[["folder"]]
      )
    }
  )
}

#' @rdname dock
#' @export
active_view <- function(x) {
  board_dock_state(x)[["active"]]
}

#' @rdname dock
#' @export
dock_plugins <- function(x) {

  plugins <- board_plugins(x)

  plugins[names(plugins) != dock_class_id]
}

# The dock's state that a board holds, or an empty list where it holds none.
board_dock_state <- function(x) {

  if (!is_board(x)) {
    blockr_abort("Expecting a board.", class = "dock_board_invalid")
  }

  dock <- board_plugins(x)[[dock_class_id]]

  if (is.null(dock)) {
    return(list())
  }

  plugin_state(dock)
}

# The plugins that a script gives `new_dock_board()`, a plugin alone or a list
# of them, by the name that a grid gives each, if any.
# A name names one plugin.
named_plugins <- function(x) {

  if (is_plugin(x)) {
    x <- list(x)
  }

  if (!is.list(x) || is.object(x) || !all(lgl_ply(x, is_plugin))) {
    blockr_abort(
      "The argument `plugins` is a list of plugins, each made by its ",
      "package's constructor.",
      class = "dock_board_plugins_invalid"
    )
  }

  nms <- coal(names(x), rep("", length(x)), fail_all = FALSE)
  nms[is.na(nms)] <- ""

  named <- nms[nzchar(nms)]
  twice <- unique(named[duplicated(named)])

  if (length(twice)) {
    blockr_abort(
      "A name in `plugins` names one plugin, while {twice} name{?s/} more ",
      "than one.",
      class = "dock_board_plugins_invalid"
    )
  }

  set_names(x, nms)
}

# The views with each panel reference resolved against the board's blocks,
# `block_ids`, and the names of its plugins, as `new_dock_plugin()` takes them.
# A bare id names a block where the board holds one, and otherwise the plugin
# of that name in `plugins`, while `plg()` takes the name of a plugin there,
# or else the id of a plugin's class.
# What names neither a plugin nor a block that the board holds names a block,
# which `check_board_blocks()` then refuses.
resolve_views <- function(views, block_ids, plugins) {

  named <- nzchar(names(plugins))

  class_ids <- set_names(
    chr_ply(plugins[named], plugin_class_id),
    names(plugins)[named]
  )

  resolve <- function(ref) {

    id <- ref[["id"]]
    type <- ref[["type"]]

    if (identical(type, "bare") && id %in% block_ids) {
      return(blk(id))
    }

    if (!identical(type, "block") && id %in% names(class_ids)) {
      return(plg(class_ids[[id]]))
    }

    if (identical(type, "plugin")) {
      return(ref)
    }

    blk(id)
  }

  lapply(
    views,
    function(view) {

      if (not_null(view[["grid"]])) {
        view[["grid"]] <- map_grid_panels(view[["grid"]], resolve)
      } else {
        view[["panels"]] <- lapply(view[["panels"]], resolve)
      }

      view
    }
  )
}

# The dock's state names blocks in the grids of its views and in `visible`,
# each of which the board holds, so the function refuses, naming each, a block
# that the board lacks.
check_board_blocks <- function(state, block_ids) {

  lost <- function(ids) unique(setdiff(ids, block_ids))

  lines <- chr_mply(
    function(view, id) {

      blocks <- lost(unlst(lst_xtr(grid_panels(view[["grid"]]), "block")))

      if (length(blocks)) {
        paste0(
          "The grid of the view ", id, " names the block",
          if (length(blocks) > 1L) "s", " ",
          paste_enum(blocks, quotes = ""), ", which the board lacks."
        )
      } else {
        NA_character_
      }
    },
    state[["views"]],
    names(state[["views"]])
  )

  blocks <- lost(names(state[["visible"]]))

  lines <- c(
    lines[!is.na(lines)],
    if (length(blocks)) {
      paste0(
        "The argument `visible` names the block",
        if (length(blocks) > 1L) "s", " ",
        paste_enum(blocks, quotes = ""), ", which the board lacks."
      )
    }
  )

  abort_each(lines, class = "dock_board_block_unknown")
}
