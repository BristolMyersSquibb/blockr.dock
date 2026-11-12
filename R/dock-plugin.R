#' The dock plugin
#'
#' The dock is a frontend, the plugin that draws the page.
#' Its client draws the page as a React app, with dockview for the layout.
#' The constructor `new_dock_plugin()` makes the dock plugin, of class
#' `c("dock_plugin", "frontend_plugin", "plugin")`, whose plugin class has the
#' id `blockr.dock-dock_plugin` (see [blockr.core::new_plugin()]).
#' The dock keeps no state in the board, so the constructor takes no args.
#'
#' The package lists the class in the `._BLOCKR_MODULES_` that it exports,
#' with its name, "Dock", and its module, `js/dock.js` (see
#' [blockr.core::read_blocks_yml()]).
#' So [blockr.core::available_plugins()] gives the class wherever blockr.dock
#' is available, and [blockr.core::serve()] runs the dock in place of core's
#' basic frontend.
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
  )
)
