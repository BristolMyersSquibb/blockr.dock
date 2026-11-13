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
#' `js/dock.schema.json`.
#' The constructor takes no args, so a board that an app script builds holds
#' no state for the dock, and the client gives it the class's default, which
#' holds no view.
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
#' @return A `plugin` object, as [blockr.core::new_plugin()] returns it.
#'
#' @examples
#' new_dock_plugin()
#'
#' @export
new_dock_plugin <- function() {
  new_plugin(list(), "dock_plugin", c("dock_plugin", "frontend_plugin"))
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
