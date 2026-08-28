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

# Maintain the authoritative server-side panel-membership set. Every panel add /
# remove flows through add_*_panel() / remove_*_panel(), so updating here keeps
# `live_panels` in lockstep with the live dock without waiting for the browser's
# settled state echo -- which is what lets reconcile tell a just-added panel
# from a stale layout. `live_panels` is NULL for a dock with no tracker (a bare
# test stub), in which case tracking is a no-op.
track_panel_added <- function(live_panels, id) {

  if (is.null(live_panels)) {
    return(invisible())
  }

  pid <- as.character(id)
  cur <- isolate(live_panels())

  if (!pid %in% cur) {
    live_panels(c(cur, pid))
  }

  invisible()
}

track_panel_removed <- function(live_panels, id) {

  if (is.null(live_panels)) {
    return(invisible())
  }

  live_panels(setdiff(isolate(live_panels()), as.character(id)))

  invisible()
}

remove_block_panel <- function(id, dock) {
  pid <- as_block_panel_id(id)

  log_debug("removing block panel {pid}")

  dockViewR::remove_panel(dock$proxy, pid)
  track_panel_removed(dock$live_panels, pid)

  invisible(NULL)
}

add_block_panel <- function(block, ..., dock) {
  pid <- as_block_panel_id(block)

  log_debug("adding block panel {pid}")

  dockViewR::add_panel(dock$proxy, panel = block_panel(block, ...))
  track_panel_added(dock$live_panels, pid)

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

remove_ext_panel <- function(id, dock) {
  pid <- as_ext_panel_id(id)

  log_debug("removing extension panel {pid}")

  dockViewR::remove_panel(dock$proxy, pid)
  track_panel_removed(dock$live_panels, pid)

  invisible(NULL)
}

add_ext_panel <- function(ext, ..., dock) {
  stopifnot(is_dock_extensions(ext))

  pid <- as_ext_panel_id(ext)

  log_debug("adding extension panel {pid}")

  dockViewR::add_panel(dock$proxy, panel = ext_panel(ext, ...))
  track_panel_added(dock$live_panels, pid)

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

  dock_panel(id = eid, title = extension_name(ext[[1L]]), ...)
}

ext_panel_ids <- function(proxy = dock_proxy()) {
  res <- dock_panel_ids(proxy)

  as_ext_panel_id(
    res[lgl_ply(res, is_ext_panel_id)]
  )
}

move_dock_panel <- function(id, position, proxy = dock_proxy()) {
  pid <- as_dock_panel_id(id)

  log_debug("moving panel {pid}")

  dockViewR::move_panel(proxy, pid, position)

  invisible(NULL)
}

resize_dock_panel <- function(id, size, proxy = dock_proxy()) {
  pid <- as_dock_panel_id(id)

  log_debug("resizing panel {pid}")

  dockViewR::set_size(proxy, pid, size)

  invisible(NULL)
}

restore_layout <- function(layout, proxy, blocks = list(),
                           extensions = list()) {
  log_debug("restoring dockview layout")

  payload <- as_dock_layout(as_dock_grid(layout), blocks, extensions)

  # Coerce each panel's closability to match the *current* lock state.
  # Without this, a board saved unlocked (panels have tabComponent = "manual")
  # remains closable when restored in a locked app, and vice versa. The
  # dock-level locked / disableDnd / add_tab config is already set from the
  # current state at `set_dock_view_output()`; we just need the per-panel
  # tabComponent to follow suit (#124).
  payload <- lock_panels(payload, locked = is_dock_locked())

  dockViewR::restore_dock(proxy, unclass(payload))
  invisible(NULL)
}

# Rewrite every panel's `tabComponent` and `removeCallback` so closability
# reflects the current lock state rather than what was persisted at save
# time. Symmetric:
#   * locked   = TRUE  -> tabComponent "custom" + removeCallback NULL
#     (no close X is rendered; any saved callback is dropped).
#   * locked   = FALSE -> tabComponent "manual" + the canonical default
#     `removeCallback` (matches what `create_layout_panel()` would
#     serialise for a freshly built panel). Without this, panels saved
#     while locked have `removeCallback = NULL`; restoring them unlocked
#     would render the close X but clicking it would throw a TypeError
#     in `DefaultTab` because the callback is missing (#TBD).
lock_panels <- function(layout, locked) {
  if (isTRUE(locked)) {
    tab_component <- "custom"
    remove_callback <- NULL
  } else {
    tab_component <- "manual"
    remove_callback <- list(
      `__IS_FUNCTION__` = TRUE,
      source = unclass(dockViewR::default_remove_tab_callback())
    )
  }
  layout[["panels"]] <- lapply(layout[["panels"]], function(p) {
    p[["tabComponent"]] <- tab_component
    p[["params"]][["removeCallback"]] <- remove_callback
    p
  })
  layout
}

dock_proxy <- function(session = get_session()) {
  dockViewR::dock_view_proxy(dock_id(), session = session)
}

# Hand the client the edges this dock carries a rail on, once the restore that
# creates them has gone out. The client owns two things the server cannot see in
# time: revealing a hidden rail mid-drag, which has no server round-trip to
# spare, and re-asserting the derived visibility rule as panels are added,
# removed and moved. Both need to know which edges are rails and how wide the
# collapsed strip is, which is all this carries.
send_rail_config <- function(rails, session = get_session()) {

  if (!length(rails)) {
    return(invisible(NULL))
  }

  session$sendCustomMessage(
    "blockr-dock-rails",
    list(
      id = session$ns(dock_id()),
      rails = unname(lapply(rails, rail_client_config))
    )
  )

  invisible(NULL)
}

rail_client_config <- function(rail) {
  list(
    position = rail[["position"]],
    collapsedSize = coal(rail[["collapsed_size"]], 35, fail_all = FALSE)
  )
}

rail_dep <- function() {
  htmltools::htmlDependency(
    "blockr-dock-rail",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "dock-rail.js"
  )
}

dock_board_ns <- function(dock) {
  coal(dock$board_ns, dock$proxy$session$ns)
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

  # Read blockr.core's `blockr.locked`, the same flag the server-side gate
  # consults via is_board_locked(), so one deployment option drives both core's
  # update / option gate and dock's UI hides.
  isTRUE(blockr_option("locked", FALSE))
}

# The narrow-viewport decision, taken once per session from the width the
# client reports at input initialisation (see `viewport_probe_ui()`). Below the
# breakpoint a view's groups stack into one scrolling column rather than a
# nested grid, so a board on a phone reads top to bottom instead of running
# columns off-screen. A width that never arrived -- a `board_ui()` built after
# Shiny bound its inputs, so the probe missed the initial batch -- reads as
# wide, the desktop render, as does an unset or unusable breakpoint.
is_narrow_viewport <- function(width) {

  breakpoint <- narrow_breakpoint()

  is_number(width) && not_null(breakpoint) && width < breakpoint
}

# A breakpoint under this is a typo rather than a layout: no viewport is 50 px
# wide, and the value that reads as "never collapse" is an absent option.
narrow_breakpoint_floor <- 50

# The viewport width below which a view collapses, in CSS pixels, or NULL when
# the collapse is off. Opt-in: a board renders its authored grid at every width
# until a deployment sets `options(blockr.narrow_breakpoint = ...)`. `Inf` is
# meaningful and allowed -- it collapses at every width, which is how a board
# meant only for phones asks to always stack. Anything else unusable aborts
# rather than falling back to a default: a deployment that sets a breakpoint
# has an intent, and silently rendering the other layout hides the typo until
# someone opens the board on a phone.
narrow_breakpoint <- function() {

  opt <- blockr_option("narrow_breakpoint", NULL)

  if (is.null(opt)) {
    return(NULL)
  }

  px <- suppressWarnings(as.numeric(opt))

  if (!is_scalar(px) || is.na(px) || px < narrow_breakpoint_floor) {
    blockr_abort(
      paste(
        "`blockr.narrow_breakpoint` must be a single number of at least",
        "{narrow_breakpoint_floor} px, or `Inf` to always collapse;",
        "got {opt}."
      ),
      class = "narrow_breakpoint_invalid"
    )
  }

  px
}

# The most of the viewport one row of the collapsed stack may take, as a
# fraction in (0, 1]. At the 0.8 default a tall panel fills most of the screen
# and the page scrolls from one to the next; lower it to fit more rows at once.
# Tune with `options(blockr.narrow_group_fraction = ...)`; an unusable value
# aborts, for the same reason the breakpoint does.
narrow_group_fraction <- function() {

  opt <- blockr_option("narrow_group_fraction", NULL)

  if (is.null(opt)) {
    return(0.8)
  }

  frac <- suppressWarnings(as.numeric(opt))

  if (!is_number(frac) || frac <= 0 || frac > 1) {
    blockr_abort(
      "`blockr.narrow_group_fraction` must be a fraction in (0, 1]; got {opt}.",
      class = "narrow_group_fraction_invalid"
    )
  }

  frac
}

# The stacked container's height: the sum of the rows' viewport shares, so the
# page scrolls the stack rather than dockView dividing a viewport-height box
# among the rows and squeezing every one of them. Takes the *wide* grid, the
# one `stack_row_heights()` reads the authored heights off; the stacked grid
# carries those normalised to ratios, which no longer say how tall the stack
# is. Written as the custom property the stylesheet reads, so a wide viewport
# (which stamps nothing) keeps the viewport-height default rather than needing
# it cleared. Height only -- the container keeps its clip, or the parked
# background-tab overlays trail a blank screenful past the last panel.
narrow_stack_attrs <- function(grid) {

  total <- sum(stack_row_heights(grid))

  if (!isTRUE(total > 0)) {
    total <- narrow_group_fraction()
  }

  list(style = paste0("--blockr-stack-height: ", total * 100, "vh;"))
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

#' Swap the contents of the `active_dock` mirror to a different dock.
#'
#' `active_dock` is an internal `reactiveValues` mirror of whichever view is
#' currently active. The board-level block insert / remove plugin places and
#' removes panels through it, so it must always point at the active view. When
#' the user switches views, this function copies the new view's dock module
#' result into it so that handle follows the active dock without re-binding.
#'
#' @param rv The `active_dock` `reactiveValues` to update.
#' @param dock A dock module result (list with `proxy`, `board_ns`,
#'   `live_panels`, `layout`, etc.).
#'
#' @noRd
update_active_dock <- function(rv, dock) {
  rv$proxy <- dock$proxy
  rv$board_ns <- dock$board_ns
  rv$live_panels <- dock$live_panels
  rv$layout <- dock$layout
  rv$prev_active_group <- dock$prev_active_group
  rv$n_panels <- dock$n_panels
  rv$active_group_trail <- dock$active_group_trail
}

get_dock_panel <- function(id, proxy = dock_proxy()) {
  dockViewR::get_panels(proxy)[[id]]
}
