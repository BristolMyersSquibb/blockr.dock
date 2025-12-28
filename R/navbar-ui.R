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
      # Hamburger menu with tabbed dropdown (Workflows + History)
      tags$div(
        class = "dropdown",
        tags$button(
          class = "blockr-navbar-icon-btn",
          type = "button",
          `data-bs-toggle` = "dropdown",
          `aria-expanded` = "false",
          bsicons::bs_icon("layers", size = "1.4em")
        ),
        # Tabbed dropdown content
        tags$div(
          class = "dropdown-menu blockr-tabbed-dropdown",
          # Tab bar
          tags$div(
            class = "blockr-tab-bar",
            tags$button(
              id = ns("tab_workflows"),
              class = "blockr-tab active",
              type = "button",
              onclick = sprintf(
                "event.stopPropagation(); document.querySelectorAll('#%s .blockr-tab').forEach(t => t.classList.remove('active')); this.classList.add('active'); document.getElementById('%s').classList.remove('blockr-tab-panel-hidden'); document.getElementById('%s').classList.add('blockr-tab-panel-hidden');",
                ns("tabbed_dropdown"),
                ns("panel_workflows"),
                ns("panel_history")
              ),
              bsicons::bs_icon("layers"),
              "Workflows"
            ),
            tags$button(
              id = ns("tab_history"),
              class = "blockr-tab",
              type = "button",
              onclick = sprintf(
                "event.stopPropagation(); document.querySelectorAll('#%s .blockr-tab').forEach(t => t.classList.remove('active')); this.classList.add('active'); document.getElementById('%s').classList.add('blockr-tab-panel-hidden'); document.getElementById('%s').classList.remove('blockr-tab-panel-hidden');",
                ns("tabbed_dropdown"),
                ns("panel_workflows"),
                ns("panel_history")
              ),
              bsicons::bs_icon("clock-history"),
              "History"
            )
          ),
          # Workflows panel
          tags$div(
            id = ns("panel_workflows"),
            class = "blockr-tab-panel",
            tags$div(
              class = "blockr-workflows-section",
              "RECENT"
            ),
            tags$div(
              class = "blockr-workflows-list",
              uiOutput(ns("recent_workflows"))
            ),
            tags$div(
              class = "blockr-tab-footer",
              tags$a(
                href = "#",
                class = "blockr-workflows-link",
                onclick = sprintf(
                  "Shiny.setInputValue('%s', Date.now(), {priority: 'event'}); return false;",
                  ns("view_all_workflows")
                ),
                "View all workflows ",
                bsicons::bs_icon("arrow-right")
              )
            )
          ),
          # History panel
          tags$div(
            id = ns("panel_history"),
            class = "blockr-tab-panel blockr-tab-panel-hidden",
            tags$div(
              class = "blockr-history-title",
              uiOutput(ns("history_title"), inline = TRUE)
            ),
            uiOutput(ns("version_history"))
          ),
          id = ns("tabbed_dropdown")
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
      # Save status section (simplified - history is now in hamburger menu)
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
      # Dark mode toggle
      tags$button(
        id = ns("dark_mode_toggle"),
        class = "blockr-navbar-icon-btn blockr-dark-mode-toggle",
        type = "button",
        title = "Toggle dark mode",
        onclick = "document.body.classList.toggle('dark-mode'); this.querySelector('.icon-light').classList.toggle('d-none'); this.querySelector('.icon-dark').classList.toggle('d-none'); var dv = document.querySelector('[class*=\"dockview-theme-\"]'); console.log('dockview element:', dv); console.log('dockview className before:', dv ? dv.className : 'not found'); if(dv) { if(document.body.classList.contains('dark-mode')) { dv.className = dv.className.replace(/dockview-theme-[\\w-]+/, 'dockview-theme-abyss-spaced'); } else { dv.className = dv.className.replace(/dockview-theme-[\\w-]+/, 'dockview-theme-light-spaced'); } console.log('dockview className after:', dv.className); }",
        tags$span(class = "icon-light", bsicons::bs_icon("sun", size = "1.2em")),
        tags$span(class = "icon-dark d-none", bsicons::bs_icon("moon-stars", size = "1.2em"))
      ),
      tags$button(
        class = "blockr-navbar-settings-btn",
        type = "button",
        `data-bs-toggle` = "offcanvas",
        `data-bs-target` = paste0("#", ns("options_offcanvas")),
        `aria-controls` = ns("options_offcanvas"),
        bsicons::bs_icon("gear", size = "1.4em")
      ),
      tags$button(
        class = "blockr-navbar-btn-outline",
        type = "button",
        onclick = sprintf(
          "Shiny.setInputValue('%s', Date.now(), {priority: 'event'})",
          NS(id, "generate_code-code_mod")
        ),
        bsicons::bs_icon("code-slash"),
        "Code"
      ),
      uiOutput(ns("user_avatar"), inline = TRUE)
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
