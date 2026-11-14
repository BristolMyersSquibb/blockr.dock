#' The dock plugin
#'
#' The dock is a frontend, the plugin that draws the page.
#' Its client draws the page as a React app, with dockview for the layout.
#' The constructor `new_dock_plugin()` makes the dock plugin, of class
#' `c("dock_plugin", "frontend_plugin", "plugin")`, whose plugin class has the
#' id `blockr.dock-dock_plugin` (see [blockr.core::new_plugin()]).
#' The dock keeps its state in the board: its views, the folders that group
#' them, the active view and the sections that each block's card shows.
#' The schema of that state ships beside the class's module, as
#' `js/dock.schema.json`, and R writes the state by it.
#'
#' The constructor writes the state that an app script gives, once, as the
#' script runs, and the script gives the plugin to [blockr.core::new_board()]
#' in `plugins` (see [board_views()]):
#'
#' * Each view in `views`, written with [dock_view()], gets its grid, or the
#'   default grid over the panels that it is given.
#'   A view's id is its name in the list, and R draws the id of each view that
#'   the list leaves unnamed, in the shape that the blockr option `rand_id`
#'   names (see [blockr.core::rand_names()]).
#'   A view without a name is named after its id.
#' * Each folder in `folders`, written with [dock_folder()], has its name in
#'   the list as its id, or an id that R draws.
#' * Each view and folder gets an `index`, which orders it among the views and
#'   folders of the folder that holds it.
#'   In each folder, and at the top, the views come first and the folders
#'   after them, each in the order that the script gives.
#' * The view that `active` names is the one that the board opens on.
#'   Where `active` names none, it is the first view of the views menu.
#' * The argument `visible` gives the sections that a block's card shows, by
#'   the block's id: `"inputs"`, `"outputs"` or neither.
#'   A card without an entry shows both, so R drops an entry that names both.
#'
#' Where the script gives no views, folders or `visible`, the plugin holds no
#' state, and a board that holds it holds none for the dock.
#' The client then gives the board the class's default, which holds no view,
#' and the dock adds a view as it starts.
#' In a grid, a bare id names a block, and [plg()] names a plugin by the
#' name that the script gives it in the board's `plugins`, or by the id of its
#' class.
#'
#' The constructor refuses, naming each, a grid that places a panel twice.
#' A panel on a rail and in a tab group counts as twice.
#' It also refuses a view or folder that names a folder that `folders` lacks,
#' and a folder that sits in itself, through the folders that hold it.
#' Last, it refuses an `active` that names a view that `views` lacks, and a
#' section other than `"inputs"` and `"outputs"`.
#' It knows nothing of the board that holds it, so the dock completes its
#' state as `new_board()` builds the board, and refuses there what only the
#' board can tell, such as a grid that names a block that the board lacks
#' (see [board_views()]).
#'
#' The package lists the class in the `._BLOCKR_MODULES_` that it exports,
#' with its name, "Dock", and its module, `js/dock.js` (see
#' [blockr.core::read_blocks_yml()]).
#' So [blockr.core::available_plugins()] gives the class wherever blockr.dock
#' is available, and [blockr.core::serve()] runs the dock in place of core's
#' basic frontend.
#' An app script gives the items of the dock's navbar to `serve()` in
#' `navbar`.
#' The dock takes them through its method of [blockr.core::frontend_args()]
#' (see [default_navbar()]).
#'
#' The class requires five board options: the dock's own, `compact`,
#' `view_tabs` and `sync_rails` (see [new_compact_option()]), and core's
#' `show_conditions` and `dark_mode`.
#' So the client gives every board that lacks a value of one of them its
#' default as the board opens, before the dock draws it.
#' The package lists each of its own board options in `._BLOCKR_MODULES_`
#' too, with its module, as `js/compact-option.js`, beside which the schema of
#' its value ships, as `js/compact-option.schema.json`.
#'
#' @param views The views, each written with [dock_view()], by id, or `NULL`
#'   for none.
#' @param folders The folders, each written with [dock_folder()], by id.
#' @param active The id of the view that the board opens on, or `NULL` for the
#'   first view of the views menu.
#' @param visible The sections that each block's card shows, by the block's
#'   id, as `list(a = "outputs")`.
#'
#' @return A `plugin` object, as [blockr.core::new_plugin()] returns it.
#'
#' @examples
#' new_dock_plugin()
#'
#' dock <- new_dock_plugin(
#'   views = list(
#'     overview = dock_view(dock_grid("a", "b"), name = "Overview"),
#'     ae = dock_view("c", folder = "safety")
#'   ),
#'   folders = list(safety = dock_folder("Safety")),
#'   visible = list(b = "outputs")
#' )
#'
#' names(blockr.core::plugin_state(dock))
#'
#' @export
new_dock_plugin <- function(views = NULL, folders = list(), active = NULL,
                            visible = list()) {
  new_plugin(
    dock_state(views, folders, active, visible),
    "dock_plugin",
    c("dock_plugin", "frontend_plugin")
  )
}

#' @rdname new_dock_plugin
#' @format NULL
#' @usage NULL
#' @export
._BLOCKR_MODULES_ <- list( # nolint: object_name_linter.
  plugins = list(
    new_dock_plugin = list(
      id = "dock_plugin",
      name = "Dock",
      module = "js/dock.js"
    )
  ),
  options = list(
    compact = list(module = "js/compact-option.js"),
    view_tabs = list(module = "js/view-tabs-option.js"),
    sync_rails = list(module = "js/sync-rails-option.js")
  )
)
