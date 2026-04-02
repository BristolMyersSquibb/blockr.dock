dock_input <- function(input) {
  paste(dock_id(), input, sep = "_")
}

dock_panel_ids <- function(proxy = dock_proxy()) {
  ids <- dockViewR::get_panels_ids(proxy)
  if (!length(ids)) return(list())
  res <- as_dock_panel_id(ids)
  # Normalise to list: as_dock_panel_id returns a list for length > 1
  # but a single classed vector for length <= 1.
  if (!is.list(res)) list(res) else res
}

dock_panel_named_ids <- function(proxy = dock_proxy()) {
  panels <- dockViewR::get_panels(proxy)
  set_names(chr_xtr(panels, "id"), chr_xtr(panels, "title"))
}

block_panel_ids <- function(proxy = dock_proxy()) {
  res <- dock_panel_ids(proxy)

  as_block_panel_id(
    res[lgl_ply(res, is_block_panel_id)]
  )
}

remove_block_panel <- function(id, proxy = dock_proxy()) {
  pid <- as_block_panel_id(id)

  log_debug("removing block panel {pid}")

  dockViewR::remove_panel(proxy, pid)

  invisible(NULL)
}

add_block_panel <- function(block, ..., proxy = dock_proxy()) {
  log_debug("adding block panel {as_block_panel_id(block)}")

  dockViewR::add_panel(proxy, panel = block_panel(block, ...))

  invisible(NULL)
}

select_block_panel <- function(id, proxy = dock_proxy()) {
  bid <- as_block_panel_id(id)

  log_debug("selecting block panel {bid}")

  dockViewR::select_panel(proxy, bid)

  invisible(NULL)
}

block_panel <- function(block, ...) {
  pid <- as_block_panel_id(block)
  name <- block_name(block[[1]])

  log_debug("creating block panel {pid}")

  dock_panel(id = pid, title = name, ...)
}

remove_ext_panel <- function(id, proxy = dock_proxy()) {
  pid <- as_ext_panel_id(id)

  log_debug("removing extension panel {pid}")

  dockViewR::remove_panel(proxy, pid)

  invisible(NULL)
}

add_ext_panel <- function(ext, ..., proxy = dock_proxy()) {
  stopifnot(is_dock_extension(ext))

  log_debug("adding extension panel {as_ext_panel_id(ext)}")

  dockViewR::add_panel(proxy, panel = ext_panel(ext, ...))

  invisible(NULL)
}

select_ext_panel <- function(id, proxy = dock_proxy()) {
  eid <- as_ext_panel_id(id)

  log_debug("selecting extension panel {eid}")

  dockViewR::select_panel(proxy, eid)

  invisible(NULL)
}

ext_panel <- function(ext, ...) {
  eid <- as_ext_panel_id(ext)

  log_debug("creating extension panel {eid}")

  dock_panel(id = eid, title = extension_name(ext), ...)
}

ext_panel_ids <- function(proxy = dock_proxy()) {
  res <- dock_panel_ids(proxy)

  as_ext_panel_id(
    res[lgl_ply(res, is_ext_panel_id)]
  )
}

restore_dock <- function(layout, proxy = dock_proxy()) {
  log_debug("restoring dockview layout")
  dockViewR::restore_dock(proxy, unclass(layout))
}

dock_proxy <- function(session = get_session()) {
  dockViewR::dock_view_proxy(dock_id(), session = session)
}

proxy_board_ns <- function(proxy) {
  proxy$board_ns %||% proxy$session$ns
}

dock_panel <- function(...) {
  dockViewR::panel(
    ...,
    content = tagList(),
    remove = dockViewR::new_remove_tab_plugin(
      !is_dock_locked(),
      mode = "manual"
    ),
    style = list(
      overflow = "auto",
      height = "100%"
    )
  )
}

set_dock_view_output <- function(..., session = get_session()) {
  args <- c(
    list(...),
    if (is_dock_locked()) list(locked = TRUE, disableDnd = TRUE),
    list(
      defaultRenderer = "always",
      add_tab = dockViewR::new_add_tab_plugin(!is_dock_locked())
    )
  )

  session$output[[dock_id()]] <- dockViewR::render_dock_view(
    {
      log_debug("initializing empty dock {dock_id(session$ns)}")
      do.call(dockViewR::dock_view, args)
    }
  )

  dock_proxy(session)
}

is_dock_locked <- function() {
  isTRUE(blockr_option("dock_is_locked", FALSE))
}

dock_panel_groups <- function(session = get_session()) {
  xtr_leaf_id <- function(x) {
    if (x$type == "leaf") {
      return(x$data$id)
    }

    lapply(x$data, xtr_leaf_id)
  }

  unlist(
    xtr_leaf_id(session$input[[dock_input("state")]][["grid"]][["root"]])
  )
}

#' Swap the contents of `dock_mgr$active_dock` to mirror a different dock.
#'
#' Extensions hold a stable reference to `dock_mgr$active_dock` (a
#' `reactiveValues`). When the user switches workspaces, this function
#' copies the new workspace's dock module result into it so extensions
#' transparently see the new dock without needing to re-bind.
#'
#' @param rv The `reactiveValues` to update (`dock_mgr$active_dock`).
#' @param dock A dock module result (list with `layout`, `proxy`, etc.).
#'
#' @noRd
update_active_dock <- function(rv, dock) {
  rv$layout <- dock$layout
  rv$proxy <- dock$proxy
  rv$prev_active_group <- dock$prev_active_group
  rv$n_panels <- dock$n_panels
  rv$active_group_trail <- dock$active_group_trail
}

get_dock_panel <- function(id, proxy = dock_proxy()) {
  dockViewR::get_panels(proxy)[[id]]
}
