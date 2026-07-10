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

move_dom_element <- function(from, to, session = get_session()) {
  session$sendCustomMessage(
    "move-element",
    list(
      from = from,
      to = to
    )
  )
}

determine_active_views <- function(layout) {

  if (is.null(layout)) {
    return(character())
  }

  # The dockView tree, keyed by group id: a compact `dock_grid` is expanded
  # (assigning ids), while a raw dockView `_state` echo already carries its
  # tree at `$grid`. A group's active view is its open tab.
  tree <- if (is_dock_grid(layout)) grid_to_tree(layout) else layout[["grid"]]

  root <- tree[["root"]]

  if (is.null(root)) {
    return(character())
  }

  xtr_leaf <- function(x) {

    if (identical(x[["type"]], "leaf")) {
      return(
        set_names(coal(x[["data"]][["activeView"]], "", fail_all = FALSE),
                  x[["data"]][["id"]])
      )
    }

    lapply(x[["data"]], xtr_leaf)
  }

  rapply(xtr_leaf(root), identity, "character")
}

visible_block_ids <- function(layout) {

  front_panels <- as.character(determine_active_views(layout))
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

  keep_visible <- as_ext_panel_id(visible_exts())

  cands <- names(active)[!active %in% keep_visible]

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
