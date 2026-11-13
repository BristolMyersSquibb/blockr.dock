#' The dock's state on a board
#'
#' An app script builds its board with [blockr.core::new_board()], and gives
#' the dock's state among the board's `plugins`, as the dock plugin that
#' [new_dock_plugin()] makes.
#' A grid names a plugin's panel by the id of the plugin's class, as
#' `plg("blockr.dag-dag_plugin")`, and may name a plugin that the board does
#' not hold, such as one that keeps no state.
#'
#' The readers take a board.
#' The function `board_views()` reads its views, each as [dock_view()] writes
#' it, with its grid.
#' Each grid names its panels with [blk()] and [plg()].
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
#'           dock_grid("a", "b", rail(plg("blockr.core-edit_board_plugin"))),
#'           name = "Main"
#'         ),
#'         side = dock_view("b")
#'       ),
#'       active = "side",
#'       visible = list(b = "outputs")
#'     )
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
