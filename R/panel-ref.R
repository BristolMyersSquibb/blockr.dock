#' Panel references
#'
#' A grid places panels, each the card of a block or the UI of a plugin, and
#' names each panel by a panel reference (see [dock_grid()]).
#' The function `blk()` names the panel of a block, by the block's id.
#' The function `plg()` names the panel of a plugin, by the name that the app
#' script gives the plugin in the `plugins` of [new_dock_board()], or else by
#' the id of the plugin's class, as `"blockr.dag-dag_plugin"`.
#' A bare id names a panel too.
#' It names the panel of the block of that id, where the board holds one, and
#' otherwise the panel of the plugin of that name in `plugins`.
#'
#' R refuses no reference to a plugin, since it may name the class of a plugin
#' that [blockr.core::serve()] adds to the app.
#' As the dock starts, it drops the panel of a plugin that the app does not
#' run.
#'
#' @param id For `blk()`, the id of a block.
#'   For `plg()`, the name of a plugin in the `plugins` of [new_dock_board()],
#'   or the id of a plugin's class.
#' @param x An object.
#'
#' @return The functions `blk()` and `plg()` return a `panel_ref`, and
#'   `is_panel_ref()` returns a boolean.
#'
#' @examples
#' blk("a")
#' plg("blockr.dag-dag_plugin")
#'
#' dock_grid(blk("a"), "b", rail(plg("dag")))
#'
#' @rdname panel-ref
#' @export
blk <- function(id) {
  new_panel_ref(id, "block")
}

#' @rdname panel-ref
#' @export
plg <- function(id) {
  new_panel_ref(id, "plugin")
}

# A panel reference holds an id and what the id names: "block" for `blk()`,
# "plugin" for `plg()`, and "bare" for a bare id, which names a block or a
# plugin, as `new_dock_board()` resolves it.
new_panel_ref <- function(id, type) {

  if (!is_string(id) || is.na(id) || !nzchar(id)) {
    blockr_abort(
      "A panel reference names a panel by an id, a string that is not empty.",
      class = "dock_panel_ref_invalid"
    )
  }

  structure(list(id = id, type = type), class = "panel_ref")
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
    plugin = paste0("plg(\"", x[["id"]], "\")"),
    paste0("\"", x[["id"]], "\"")
  )
}

#' @export
print.panel_ref <- function(x, ...) {
  cat("<panel_ref>", format(x), "\n")
  invisible(x)
}

# The panel references that `x` gives, as a list: a reference alone, a bare id
# for each string of a character vector, those of each element of a list, and
# none for `NULL`.
as_panel_refs <- function(x) {

  if (is.null(x)) {
    return(list())
  }

  if (is_panel_ref(x)) {
    return(list(x))
  }

  if (is.character(x) && !is.object(x)) {
    return(lapply(unname(x), new_panel_ref, "bare"))
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

# Whether two references may name the same panel: they have the same id, and
# the same type, unless one of them is a bare id, which may name either.
same_panel_ref <- function(a, b) {

  types <- c(a[["type"]], b[["type"]])

  identical(a[["id"]], b[["id"]]) &&
    (identical(types[1L], types[2L]) || "bare" %in% types)
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
    Position(function(ref) same_panel_ref(ref, active[[1L]]), refs)
  }

  if (is.null(hit) || is.na(hit)) {
    blockr_abort(
      "The `active` panel must be one of the panels given.",
      class = "dock_panels_active_invalid"
    )
  }

  refs[[hit]]
}

# The panel that a reference names, as the dock's state holds it, where
# `type` is the reference's resolved type, "block" or "plugin".
state_panel <- function(id, type) {
  set_names(list(id), type)
}

# A reference to the panel that the dock's state holds, as `blk()` or `plg()`
# write it.
panel_ref_of <- function(panel) {

  if (not_null(panel[["block"]])) {
    return(blk(panel[["block"]]))
  }

  plg(panel[["plugin"]])
}

# How a sentence names a panel of the dock's state.
panel_label <- function(panel) {
  if (not_null(panel[["block"]])) {
    paste("the block", panel[["block"]])
  } else {
    paste("the plugin", panel[["plugin"]])
  }
}
