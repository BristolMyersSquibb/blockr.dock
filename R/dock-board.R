#' The dock's state on a board
#'
#' An app script builds its board with [blockr.core::new_board()], and gives
#' the dock's state among the board's `plugins`, as the dock plugin that
#' [new_dock_plugin()] makes.
#' The script may name the board's other plugins there, as
#' `plugins = list(new_dock_plugin(...), dag = new_dag_plugin())`, and a grid
#' then names a plugin's panel by that name, as `plg("dag")`.
#' A grid may also name a plugin by the id of its class, as
#' `plg("blockr.dag-dag_plugin")`, and so name a plugin that the board does
#' not hold, such as one that keeps no state.
#'
#' As `new_board()` builds the board, the dock completes its state on it,
#' through its method of [blockr.core::complete_plugin()].
#' Where a grid names a plugin by such a name, the dock names it by the id of
#' the plugin's class instead, as the board holds the plugin.
#' It then refuses, naming each, a grid that places a panel twice, as a grid
#' does that names one plugin both by its name and by the id of its class.
#' It refuses a `plg()` whose id is neither the name of one of the board's
#' plugins nor the id of a plugin class, which has the form `{package}-{id}`.
#' It refuses no id of that form, since a grid may name a plugin that
#' [blockr.core::serve()] adds to the app.
#' Last, it refuses a grid or an entry of `visible` that names a block that
#' the board lacks.
#'
#' The readers take a board.
#' The function `board_views()` reads its views, each as [dock_view()] writes
#' it, with its grid.
#' Each grid names its panels with [blk()] and [plg()], the latter by the id
#' of the plugin's class.
#' So a script can build one board from another, as
#' `new_dock_plugin(views = board_views(board)["main"])` does.
#' The function `active_view()` reads the id of the active view.
#'
#' @param x A board.
#'
#' @return The function `board_views()` returns a list of views, by id, each a
#'   `dock_view`, and `active_view()` returns the id of the active view, or
#'   `NULL` where the board holds no view.
#'
#' @examples
#' board <- blockr.core::new_board(
#'   blocks = c(
#'     a = blockr.core::new_dataset_block("iris"),
#'     b = blockr.core::new_head_block()
#'   ),
#'   links = list(from = "a", to = "b"),
#'   plugins = list(
#'     new_dock_plugin(
#'       views = list(
#'         main = dock_view(
#'           dock_grid("a", "b", rail(plg("edit"))),
#'           name = "Main"
#'         ),
#'         side = dock_view("b")
#'       ),
#'       active = "side",
#'       visible = list(b = "outputs")
#'     ),
#'     edit = blockr.core::new_edit_board_plugin()
#'   )
#' )
#'
#' names(board_views(board))
#' active_view(board)
#' view_name(board_views(board)[["side"]])
#'
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

# The dock's state that a board holds, or an empty list where it holds none.
# The board holds the dock under the id of the dock plugin's class.
board_dock_state <- function(x) {

  if (!is_board(x)) {
    blockr_abort("Expecting a board.", class = "dock_board_invalid")
  }

  dock <- board_plugins(x)[[plugin_class_id(new_dock_plugin())]]

  if (is.null(dock)) {
    return(list())
  }

  plugin_state(dock)
}

# The dock completes its state on the board that `new_board()` builds, which
# hands it the names that the script gives the board's plugins in `aliases`,
# each mapped to the id of its plugin's class.
# Each `plg()` that names a plugin by such a name comes to name it by the id
# of its class, as the board holds the plugin.
# Then the dock refuses, naming each, a panel that a grid places twice, as a
# name and the id of a class for one plugin do, a `plg()` that names no plugin
# of the board and no plugin class, and a block that the board lacks.
#' @export
complete_plugin.dock_plugin <- function(x, board, aliases, ...) {

  if (!length(plugin_state(x))) {
    return(x)
  }

  resolve <- function(panel) {

    id <- panel[["plugin"]]

    if (not_null(id) && id %in% names(aliases)) {
      panel[["plugin"]] <- aliases[[id]]
    }

    panel
  }

  x[["views"]][] <- lapply(
    x[["views"]],
    function(view) {
      view[["grid"]] <- map_grid_panels(view[["grid"]], resolve)
      view
    }
  )

  check_placed_once(lst_xtr(x[["views"]], "grid"))
  check_plugin_names(x[["views"]])
  check_board_blocks(x[["views"]], x[["visible"]], board_block_ids(board))

  x
}

# A `plg()` names a plugin of the board by its name, which the dock has turned
# into the id of the plugin's class, or a plugin class by its id, which has
# the form `{package}-{id}`, so the dock refuses, naming each, an id without a
# package.
check_plugin_names <- function(views) {

  lines <- chr_mply(
    function(view, id) {

      ids <- unlst(lst_xtr(grid_panels(view[["grid"]]), "plugin"))
      lost <- unique(ids[!grepl("-", ids, fixed = TRUE)])

      if (length(lost)) {
        paste0(
          "The grid of the view ", id, " names the plugin",
          if (length(lost) > 1L) "s", " ", paste_enum(lost, quotes = ""),
          ", which the board's plugins do not name."
        )
      } else {
        NA_character_
      }
    },
    views,
    names(views)
  )

  abort_each(lines[!is.na(lines)], class = "dock_plugin_name_unknown")
}

# The dock's state names blocks in the grids of its views and in `visible`,
# each of which the board holds, so the dock refuses, naming each, a block
# that the board lacks.
check_board_blocks <- function(views, visible, block_ids) {

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
    views,
    names(views)
  )

  blocks <- lost(names(visible))

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
