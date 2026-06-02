#' View navigation dropdown UI and dependency.
#' @noRd
view_binding_dep <- function() {
  htmltools::htmlDependency(
    "blockr-view",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "view-binding.js"
  )
}

#' @noRd
view_nav_ui <- function(id, views) {

  ns <- NS(id)
  nav_id <- ns("view_nav")
  active <- active_view(views)
  active_nm <- coal(view_name(views[[active]]), active, fail_all = FALSE)
  can_crud <- views_can_crud(views)

  items <- map(
    view_item_ui,
    names(views),
    view_names(views),
    MoreArgs = list(active_id = active, can_crud = can_crud)
  )

  add_btn <- NULL
  if (can_crud) {
    add_btn <- tags$button(
      class = "dropdown-item blockr-view-add",
      bsicons::bs_icon("plus-lg"),
      "New page"
    )
  }

  div(
    class = "blockr-view-dropdown dropdown",
    view_binding_dep(),
    tags$button(
      class = "blockr-navbar-icon-btn dropdown-toggle blockr-view-toggle",
      type = "button",
      `data-bs-toggle` = "dropdown",
      `aria-expanded` = "false",
      bsicons::bs_icon("journals"),
      tags$span(class = "blockr-view-toggle-label", active_nm)
    ),
    div(
      class = "dropdown-menu blockr-view-nav",
      id = nav_id,
      items,
      if (can_crud) tags$hr(class = "dropdown-divider"),
      add_btn
    )
  )
}

#' @noRd
view_item_ui <- function(view_id, view_name, active_id = NULL,
                         can_crud = FALSE) {

  cls <- paste("dropdown-item blockr-view-item",
               if (identical(view_id, active_id)) "active" else "")

  actions <- NULL
  if (can_crud) {
    actions <- tags$span(
      class = "blockr-view-item-actions",
      tags$span(
        class = "blockr-view-action blockr-view-edit",
        role = "button",
        title = "Rename",
        bsicons::bs_icon("pencil")
      ),
      tags$span(
        class = "blockr-view-action blockr-view-remove",
        role = "button",
        title = "Remove",
        bsicons::bs_icon("x-lg")
      )
    )
  }

  tags$div(
    class = cls,
    `data-view-id` = view_id,
    tags$span(class = "blockr-view-item-name", view_name),
    actions
  )
}
