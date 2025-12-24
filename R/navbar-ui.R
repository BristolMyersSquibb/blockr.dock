#' Navbar UI Module
#'
#' Creates the top navbar with workflow management controls.
#'
#' @param id Namespace ID
#' @return Shiny UI element
#'
#' @keywords internal
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
          bsicons::bs_icon("list", size = "1.4em")
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
          # Dynamic workflow list from server
          tags$div(
            class = "blockr-workflows-list",
            uiOutput(ns("recent_workflows"))
          ),
          tags$a(
            href = "#",
            class = "blockr-workflows-link",
            "View all workflows ",
            bsicons::bs_icon("arrow-right")
          )
        )
      ),
      # Editable workflow title
      tags$div(
        id = ns("title_wrapper"),
        class = "blockr-navbar-title-wrapper",
        # Display mode
        tags$span(
          id = ns("title_display"),
          class = "blockr-navbar-title",
          onclick = sprintf(
            "document.getElementById('%s').classList.add('editing'); document.getElementById('%s').focus(); document.getElementById('%s').select();",
            ns("title_wrapper"),
            ns("title_input"),
            ns("title_input")
          ),
          ""
        ),
        # Edit mode input
        tags$input(
          id = ns("title_input"),
          class = "blockr-navbar-title-input shiny-bound-input",
          type = "text",
          value = "",
          onblur = sprintf(
            "Shiny.setInputValue('%s', this.value, {priority: 'event'}); document.getElementById('%s').classList.remove('editing'); document.getElementById('%s').textContent = this.value;",
            ns("title_edit"),
            ns("title_wrapper"),
            ns("title_display")
          ),
          onkeydown = sprintf(
            "if(event.key === 'Enter') { this.blur(); } if(event.key === 'Escape') { document.getElementById('%s').classList.remove('editing'); this.value = document.getElementById('%s').textContent; }",
            ns("title_wrapper"),
            ns("title_display")
          )
        )
      ),
      # Divider
      tags$span(class = "blockr-navbar-divider"),
      # Save status section
      tags$div(
        class = "blockr-navbar-save-section",
        tags$span(
          id = ns("save_status"),
          class = "blockr-navbar-meta",
          "Not saved"
        ),
        tags$button(
          id = ns("save_btn"),
          class = "blockr-navbar-save-btn shiny-bound-input",
          type = "button",
          onclick = sprintf(
            "Shiny.setInputValue('%s', Date.now(), {priority: 'event'})",
            ns("save_btn")
          ),
          bsicons::bs_icon("floppy")
        )
      ),
      # New button
      tags$button(
        id = ns("new_btn"),
        class = "blockr-navbar-btn-outline shiny-bound-input",
        type = "button",
        onclick = sprintf(
          "Shiny.setInputValue('%s', Date.now(), {priority: 'event'})",
          ns("new_btn")
        ),
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
        bsicons::bs_icon("gear", size = "1.4em")
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
    ),
    # JavaScript for handling title and save status updates from server
    tags$script(HTML(sprintf("
      $(document).ready(function() {
        Shiny.addCustomMessageHandler('blockr-update-navbar-title', function(title) {
          var displayEl = document.getElementById('%s');
          var inputEl = document.getElementById('%s');
          if (displayEl) displayEl.textContent = title;
          if (inputEl) inputEl.value = title;
        });

        Shiny.addCustomMessageHandler('blockr-update-save-status', function(status) {
          var statusEl = document.getElementById('%s');
          if (statusEl) statusEl.textContent = status;
        });
      });
    ", ns("title_display"), ns("title_input"), ns("save_status"))))
  )
}
