off_canvas <- function(
  id,
  title,
  ...,
  width = "w-25",
  position = c("start", "top", "bottom", "end")
) {
  label <- paste0(id, "-title")

  div(
    class = glue("offcanvas offcanvas-{match.arg(position)} {width}"),
    tabindex = "-1",
    id = id,
    `aria-labelledby` = label,
    `data-bs-scroll` = "true",
    div(
      class = "offcanvas-header",
      h5(class = "offcanvas-title", id = label, title),
      tags$button(
        type = "button",
        class = "btn-close",
        `data-bs-dismiss` = "offcanvas",
        `aria-label` = "Close"
      )
    ),
    div(
      class = "offcanvas-body",
      ...
    )
  )
}

collapse_container <- function(id, ...) {
  tags$div(class = "collapse", id = id, ...)
}

# One message per sweep, not one per card: `sendCustomMessage()` writes a
# websocket frame per call, so N cards would arrive as N frames in N client
# tasks, each relocating DOM that re-fires the moved outputs' resize and
# intersection observers, whose clientdata writes then go up in their own input
# batch. Batched, a sweep is one frame and one batch however many cards it
# moves. `to` is recycled: a sweep into the offcanvas shares one target.
move_dom_elements <- function(from, to, session = get_session()) {

  if (!length(from)) {
    return(invisible(NULL))
  }

  session$sendCustomMessage(
    "move-element",
    map(list, from = from, to = to)
  )
}

determine_active_views <- function(layout, active_panel = NULL) {

  if (is.null(layout)) {
    return(character())
  }

  # The dockView tree, keyed by group id: a compact `dock_grid` is expanded
  # (assigning ids), while a raw dockView `_state` echo already carries its
  # tree at `$grid`. A group's active view is its open tab -- but a bare tab
  # switch does not always refresh the echo's `activeView`, so the client's
  # live `active_panel` overrides the front of whichever group lists it.
  #
  # A rail is a group too, serialised beside the grid rather than inside it, so
  # its open tab folds in here. Without that, a block a user parks on an edge
  # reads as off screen and its card never paints. A bare `dock_grid` carries
  # no rails, so only an echo contributes any.
  compact <- is_dock_grid(layout)
  tree <- if (compact) grid_to_tree(layout) else layout[["grid"]]

  rails <- if (compact) {
    character()
  } else {
    rail_active_views(layout[["edgeGroups"]], active_panel)
  }

  root <- tree[["root"]]

  if (is.null(root)) {
    return(rails)
  }

  xtr_leaf <- function(x) {

    if (identical(x[["type"]], "leaf")) {

      front <- if (not_null(active_panel) &&
                     active_panel %in% x[["data"]][["views"]]) {
        active_panel
      } else {
        coal(x[["data"]][["activeView"]], "", fail_all = FALSE)
      }

      return(set_names(front, x[["data"]][["id"]]))
    }

    lapply(x[["data"]], xtr_leaf)
  }

  c(rapply(xtr_leaf(root), identity, "character"), rails)
}

# The open tab of each rail that is actually on screen, keyed by group id like
# the grid's fronts. A rail counts only while it is both shown and expanded: a
# collapsed rail is a bare strip with no content pane, exactly as a background
# tab has none.
rail_active_views <- function(edges, active_panel = NULL) {

  shown <- Filter(rail_on_screen, edges)

  if (!length(shown)) {
    return(character())
  }

  groups <- lapply(shown, `[[`, "group")

  set_names(
    chr_ply(groups, rail_front_panel, active_panel = active_panel),
    chr_xtr(groups, "id")
  )
}

rail_on_screen <- function(edge) {
  isTRUE(edge[["visible"]]) && !isTRUE(edge[["collapsed"]])
}

rail_front_panel <- function(group, active_panel) {

  views <- as.character(unlst(group[["views"]]))

  if (not_null(active_panel) && active_panel %in% views) {
    return(active_panel)
  }

  coal(group[["activeView"]], "", fail_all = FALSE)
}

visible_block_ids <- function(layout, active_panel = NULL) {

  front_panels <- as.character(determine_active_views(layout, active_panel))
  block_panels <- front_panels[maybe_block_panel_id(front_panels)]

  as_obj_id(new_block_panel_id(block_panels))
}

# The block object ids of a view's whole membership (front and tabbed alike).
# `visible_block_ids` reads a grid for front panels only; this reads the
# structural member set, for building all of a view's cards.
view_block_ids <- function(view) {

  members <- view_members(view)

  as_obj_id(new_block_panel_id(members[maybe_block_panel_id(members)]))
}

active_view_block_ids <- function(x) {

  views <- board_views(x)

  view_block_ids(views[[active_view(views)]])
}

visible_exts <- function() {
  blockr_option("visible_extensions", "dag")
}

determine_panel_pos <- function(dock) {

  active <- determine_active_views(dock$layout())

  # A narrow view is one group, so it is always the target. The reservation
  # below -- which keeps an add off the group fronting a kept-visible extension
  # -- would leave no candidate there and split a second group off, undoing the
  # collapse on the first block a user adds.
  cands <- if (isTRUE(dock$narrow)) {
    names(active)
  } else {
    names(active)[!active %in% as_ext_panel_id(visible_exts())]
  }

  if (!length(cands)) {
    return(list(direction = "right"))
  }

  prev <- dock$prev_active_group()

  if (isTRUE(prev %in% cands)) {
    grp <- prev
  } else {
    grp <- last(cands)
  }

  list(referenceGroup = grp, direction = "within")
}

# The front (active) panel of a dockview group. The add-panel modal resolves the
# `referenceGroup` the user clicked `+` on to a member panel here, because the
# grammar addresses a group through a member panel (`near`), not a group id.
# NULL when the group is absent from the settled layout.
group_front_panel <- function(dock, group_id) {
  fronts <- determine_active_views(dock$layout())
  if (group_id %in% names(fronts)) fronts[[group_id]] else NULL
}

#' @noRd
empty_dock_prompt <- function(ns) {

  if (is_dock_locked()) {
    return(
      div(
        class = "blockr-empty-dock-prompt",
        bsicons::bs_icon("lock-fill", size = "2em"),
        tags$p("This view is empty")
      )
    )
  }

  div(
    class = "blockr-empty-dock-prompt",
    bsicons::bs_icon("plus-circle", size = "2em"),
    tags$p("Start by adding a panel"),
    actionButton(
      ns("empty_dock_add"),
      "Add panel",
      class = "btn-outline-primary btn-sm"
    )
  )
}

drop_nulls <- function(x) {
  x[!lgl_ply(x, is.null)]
}
