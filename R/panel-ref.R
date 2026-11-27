#' Panel references
#'
#' A grid places panels, each the card of a block or the UI of a plugin, and
#' names each panel by a panel reference (see [dock_grid()]).
#' The function `blk()` names the panel of a block, by the block's id.
#' The function `plg()` names the panel of a plugin, by the name that the app
#' script gives the plugin in the `plugins` of [blockr.core::new_board()], or
#' else by the plugin's class: the class's id, with its package in `package`,
#' as `plg("edit_board_plugin", package = "blockr.core")`.
#' A bare id names a panel too, that of the block of that id.
#'
#' The dock refuses no plugin class, since a class may be that of a plugin
#' that [blockr.core::serve()] adds to the app.
#' As `new_board()` builds the board, it refuses a `plg()` without a package
#' that names no plugin in the board's `plugins` (see [board_views()]).
#' As the dock starts, it drops the panel of a plugin that the app does not
#' run.
#'
#' @param id For `blk()`, the id of a block.
#'   For `plg()`, the name of a plugin in the `plugins` of
#'   [blockr.core::new_board()], or the id of a plugin's class within its
#'   package.
#' @param package For `plg()`, the package of the plugin's class, as
#'   [blockr.core::available_plugins()] gives it in `package`, or `NULL` where
#'   `id` is the name of a plugin.
#' @param x An object.
#'
#' @return The functions `blk()` and `plg()` return a `panel_ref`, and
#'   `is_panel_ref()` returns a boolean.
#'
#' @examples
#' blk("a")
#' plg("edit")
#' plg("edit_board_plugin", package = "blockr.core")
#'
#' dock_grid(blk("a"), "b", rail(plg("edit")))
#'
#' @rdname panel-ref
#' @export
blk <- function(id) {
  new_panel_ref(id, "block")
}

#' @rdname panel-ref
#' @export
plg <- function(id, package = NULL) {
  new_panel_ref(id, "plugin", package)
}

# A panel reference holds an id and what the id names: "block" for `blk()`
# and a bare id, and "plugin" for `plg()`.
# A reference to a plugin's class holds the class's package in `package` too,
# while a reference to a plugin by its name holds no package.
new_panel_ref <- function(id, type, package = NULL) {

  if (!is_nonempty_string(id)) {
    blockr_abort(
      "A panel reference names a panel by an id, a string that is not empty.",
      class = "dock_panel_ref_invalid"
    )
  }

  if (not_null(package) && !is_nonempty_string(package)) {
    blockr_abort(
      "A panel reference names the package of a plugin's class by a string ",
      "that is not empty, or names none for a plugin by its name.",
      class = "dock_panel_ref_invalid"
    )
  }

  structure(
    c(
      list(id = id, type = type),
      if (not_null(package)) list(package = package)
    ),
    class = "panel_ref"
  )
}

#' @rdname panel-ref
#' @export
is_panel_ref <- function(x) {
  inherits(x, "panel_ref")
}

#' @export
format.panel_ref <- function(x, ...) {
  switch(
    x[["type"]],
    block = paste0("blk(\"", x[["id"]], "\")"),
    plugin = paste0(
      "plg(\"", x[["id"]], "\"",
      if (not_null(x[["package"]])) {
        paste0(", package = \"", x[["package"]], "\"")
      },
      ")"
    )
  )
}

#' @export
print.panel_ref <- function(x, ...) {
  cat("<panel_ref>", format(x), "\n")
  invisible(x)
}

# The panel references that `x` gives, as a list: a reference alone, that of
# a block's panel for each string of a character vector, those of each element
# of a list, and none for `NULL`.
as_panel_refs <- function(x) {

  if (is.null(x)) {
    return(list())
  }

  if (is_panel_ref(x)) {
    return(list(x))
  }

  if (is.character(x) && !is.object(x)) {
    return(lapply(unname(x), blk))
  }

  if (is.list(x) && !is.object(x)) {
    return(do.call(c, c(list(list()), lapply(unname(x), as_panel_refs))))
  }

  blockr_abort(
    "A panel is named by `blk()`, `plg()` or a bare id, and not by an object ",
    "of class {class(x)[1L]}.",
    class = "dock_panel_ref_invalid"
  )
}

# The reference among `refs` that `active` names, as `panels()` and `rail()`
# take it, or the first where `active` is `NULL`.
# An `active` that names none of them is refused.
active_panel_ref <- function(active, refs) {

  if (!length(refs)) {
    return(NULL)
  }

  if (is.null(active)) {
    return(refs[[1L]])
  }

  active <- as_panel_refs(active)

  hit <- if (length(active) == 1L) {
    Position(function(ref) identical(ref, active[[1L]]), refs)
  }

  if (is.null(hit) || is.na(hit)) {
    blockr_abort(
      "The `active` panel must be one of the panels given.",
      class = "dock_panels_active_invalid"
    )
  }

  refs[[hit]]
}

# The panel that a reference names, as the dock's state holds it: a block by
# its id, as `list(block = "a")`, a plugin's class by its package and its id,
# as `list(plugin = list(package = "blockr.core", id = "edit_board_plugin"))`,
# and a plugin by its name, as `list(plugin = "edit")`, until the dock
# completes its state on the board.
state_panel <- function(ref) {

  if (identical(ref[["type"]], "block")) {
    return(list(block = ref[["id"]]))
  }

  if (is.null(ref[["package"]])) {
    return(list(plugin = ref[["id"]]))
  }

  list(plugin = list(package = ref[["package"]], id = ref[["id"]]))
}

# A reference to the panel that the dock's state holds, as `blk()` or `plg()`
# write it.
panel_ref_of <- function(panel) {

  if (not_null(panel[["block"]])) {
    return(blk(panel[["block"]]))
  }

  plugin <- panel[["plugin"]]

  if (is.list(plugin)) {
    return(plg(plugin[["id"]], package = plugin[["package"]]))
  }

  plg(plugin)
}

# How a sentence names a panel of the dock's state, a plugin's class as core's
# messages name an id, as `the plugin edit_board_plugin of blockr.core`.
panel_label <- function(panel) {

  if (not_null(panel[["block"]])) {
    return(paste("the block", panel[["block"]]))
  }

  plugin <- panel[["plugin"]]

  paste("the plugin", if (is.list(plugin)) id_label(plugin) else plugin)
}

# How a message names what `x` names by its `package` and its `id`, as core's
# messages name an id: `dock_plugin of blockr.dock`, or the id alone where `x`
# holds no package, as a panel reference to a block or to a plugin by its
# name does.
# Core's `pkg_id_label()` writes the same text, but core does not export it.
id_label <- function(x) {

  if (is.null(x[["package"]])) {
    return(x[["id"]])
  }

  paste(x[["id"]], "of", x[["package"]])
}
