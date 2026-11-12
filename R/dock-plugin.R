#' The dock plugin
#'
#' The dock is a frontend, the plugin that draws the page.
#' Its client draws the page as a React app, with dockview for the layout.
#' The constructor `new_dock_plugin()` makes the dock plugin, of class
#' `c("dock_plugin", "frontend_plugin", "plugin")`, whose plugin class has the
#' id `blockr.dock-dock_plugin` (see [blockr.core::new_plugin()]).
#' The dock keeps no state in the board, so the constructor takes no args.
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
