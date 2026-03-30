# Workspace navigation dropdown UI and dependency

workspace_binding_dep <- function() {
  htmltools::htmlDependency(
    "blockr-workspace",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "workspace-binding.js"
  )
}

workspace_nav_ui <- function(id, workspaces) {

  ns <- NS(id)
  nav_id <- ns("ws_nav")
  active <- active_workspace(workspaces)
  can_crud <- ws_can_crud(workspaces)

  items <- lapply(names(workspaces), function(ws_name) {
    ws_item_ui(ws_name, active = identical(ws_name, active),
               can_crud = can_crud)
  })

  add_btn <- NULL
  if (can_crud) {
    add_btn <- tags$button(
      class = "dropdown-item blockr-ws-add",
      bsicons::bs_icon("plus-lg"),
      "New page"
    )
  }

  div(
    class = "blockr-ws-dropdown dropdown",
    workspace_binding_dep(),
    tags$button(
      class = "blockr-navbar-icon-btn dropdown-toggle blockr-ws-toggle",
      type = "button",
      `data-bs-toggle` = "dropdown",
      `aria-expanded` = "false",
      bsicons::bs_icon("journals"),
      tags$span(class = "blockr-ws-toggle-label", active)
    ),
    div(
      class = "dropdown-menu blockr-ws-nav",
      id = nav_id,
      items,
      if (can_crud) tags$hr(class = "dropdown-divider"),
      add_btn
    )
  )
}

ws_item_ui <- function(ws_name, active = FALSE, can_crud = FALSE) {

  cls <- paste("dropdown-item blockr-ws-item",
               if (active) "active" else "")

  actions <- NULL
  if (can_crud) {
    actions <- tags$span(
      class = "blockr-ws-item-actions",
      tags$button(
        class = "blockr-ws-action blockr-ws-edit",
        title = "Rename",
        bsicons::bs_icon("pencil")
      ),
      tags$button(
        class = "blockr-ws-action blockr-ws-remove",
        title = "Remove",
        bsicons::bs_icon("x-lg")
      )
    )
  }

  tags$button(
    class = cls,
    `data-ws-name` = ws_name,
    tags$span(class = "blockr-ws-item-name", ws_name),
    actions
  )
}
