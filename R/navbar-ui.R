navbar_ui <- function(id) {
  ns <- NS(id)

  tags$nav(
    class = "blockr-navbar",
    style = "display: flex !important; flex-direction: row !important; align-items: center !important; justify-content: space-between !important;",
    # Left section
    tags$div(
      class = "blockr-navbar-left",
      style = "display: flex !important; flex-direction: row !important; align-items: center !important; gap: 12px;",
      # Hamburger menu with dropdown
      tags$div(
        class = "dropdown",
        tags$button(
          class = "blockr-navbar-icon-btn",
          type = "button",
          `data-bs-toggle` = "dropdown",
          `aria-expanded` = "false",
          bsicons::bs_icon("list")
        ),
        # Dropdown content
        tags$div(
          class = "dropdown-menu blockr-workflows-dropdown",
          tags$div(
            class = "blockr-workflows-header",
            "Workflows"
          ),
          tags$div(
            class = "blockr-workflows-search",
            tags$input(
              type = "text",
              class = "form-control",
              placeholder = "Search workflows..."
            )
          ),
          tags$div(
            class = "blockr-workflows-section",
            "RECENT"
          ),
          tags$div(
            class = "blockr-workflows-list",
            tags$div(
              class = "blockr-workflow-item active",
              tags$div(class = "blockr-workflow-name", "My Workflow"),
              tags$div(class = "blockr-workflow-meta", "Modified 2 hours ago")
            ),
            tags$div(
              class = "blockr-workflow-item",
              tags$div(class = "blockr-workflow-name", "Data Processing Flow"),
              tags$div(class = "blockr-workflow-meta", "Modified yesterday")
            ),
            tags$div(
              class = "blockr-workflow-item",
              tags$div(class = "blockr-workflow-name", "API Integration"),
              tags$div(class = "blockr-workflow-meta", "Modified 3 days ago")
            )
          ),
          tags$a(
            href = "#",
            class = "blockr-workflows-link",
            "View all workflows ",
            bsicons::bs_icon("arrow-right")
          )
        )
      ),
      # Workflow title
      tags$span(class = "blockr-navbar-title", "My Workflow"),
      # Divider
      tags$span(class = "blockr-navbar-divider"),
      # Save status section
      tags$div(
        class = "blockr-navbar-save-section",
        tags$span(class = "blockr-navbar-meta", "Saved 2 min ago"),
        tags$button(
          class = "blockr-navbar-save-btn",
          type = "button",
          bsicons::bs_icon("floppy")
        )
      ),
      # New button
      tags$button(
        class = "blockr-navbar-btn-outline",
        type = "button",
        bsicons::bs_icon("plus"),
        "New"
      )
    ),
    # Right section
    tags$div(
      class = "blockr-navbar-right",
      style = "display: flex !important; flex-direction: row !important; align-items: center !important; gap: 8px;",
      tags$button(
        class = "blockr-navbar-settings-btn",
        type = "button",
        bsicons::bs_icon("gear")
      ),
      tags$button(
        class = "blockr-navbar-btn-outline",
        type = "button",
        bsicons::bs_icon("share"),
        "Share"
      ),
      tags$button(
        class = "blockr-navbar-btn-primary",
        type = "button",
        "Publish"
      ),
      tags$span(class = "blockr-navbar-avatar", "JD")
    )
  )
}
