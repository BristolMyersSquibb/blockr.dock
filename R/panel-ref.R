#' Panel references
#'
#' A grid places panels, each the card of a block or the UI of a plugin, and
#' names each panel by a panel reference (see [dock_grid()]).
#' The function `blk()` names the panel of a block, by the block's id.
#' The function `plg()` names the panel of a plugin, by the id of the
#' plugin's class, as `"blockr.dag-dag_plugin"`.
#' A bare id names a panel too, that of the block of that id.
#'
#' The dock plugin's constructor refuses a `plg()` whose id lacks the form
#' `{package}-{id}` of a plugin class's id (see [new_dock_plugin()]).
#' As the dock starts, it drops the panel of a plugin that the app does not
#' run.
#'
#' @param id For `blk()`, the id of a block.
#'   For `plg()`, the id of a plugin's class.
#' @param x An object.
#'
#' @return The functions `blk()` and `plg()` return a `panel_ref`, and
#'   `is_panel_ref()` returns a boolean.
#'
#' @examples
#' blk("a")
#' plg("blockr.dag-dag_plugin")
#'
#' dock_grid(blk("a"), "b", rail(plg("blockr.dag-dag_plugin")))
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

# A panel reference holds an id and what the id names: "block" for `blk()`
# and a bare id, and "plugin" for `plg()`.
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
    plugin = paste0("plg(\"", x[["id"]], "\")")
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

# The panel that a reference names, as the dock's state holds it, where
# `type` is the reference's type, "block" or "plugin".
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
