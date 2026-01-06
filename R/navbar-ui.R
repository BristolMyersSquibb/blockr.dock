#' Navbar UI
#'
#' Creates the navbar shell with left and right customization slots.
#' Slot content comes from navbar_provider objects attached to the board.
#'
#' @param id Namespace ID
#' @param board A dock_board object
#' @param show_settings Show settings gear button (default TRUE)
#' @param show_code Show code button (default TRUE)
#'
#' @return Shiny UI tagList
#'
#' @keywords internal
navbar_ui <- function(id, board, show_settings = TRUE, show_code = TRUE) {
  ns <- NS(id)

  left_content <- get_navbar_left(ns, board)
  right_content <- get_navbar_right(ns, board)

  tagList(
    # JS handler for triggering clicks
    tags$script(HTML("
      Shiny.addCustomMessageHandler('blockr-trigger-click', function(msg) {
        var el = document.getElementById(msg.id);
        if (el) el.click();
      });
    ")),
    tags$nav(
      class = "blockr-navbar",
      tags$div(
        class = "blockr-navbar-left",
        left_content
      ),
      tags$div(
        class = "blockr-navbar-right",
        right_content,
        if (show_code) code_button_ui(ns),
        if (show_settings) settings_button_ui(ns)
      )
    )
  )
}

#' Get navbar left slot content
#'
#' Collects left slot UI from all navbar_provider objects.
#'
#' @param ns Namespace function
#' @param board A dock_board object
#'
#' @return tagList of UI elements
#' @keywords internal
get_navbar_left <- function(ns, board) {
  providers <- dock_navbar_providers(board)
  if (length(providers) == 0L) return(NULL)

  tagList(lapply(providers, function(p) {
    ui_fn <- navbar_provider_left_ui(p)
    if (is.null(ui_fn)) return(NULL)
    ui_fn(ns(navbar_provider_id(p)))
  }))
}

#' Get navbar right slot content
#'
#' Collects right slot UI from all navbar_provider objects.
#'
#' @param ns Namespace function
#' @param board A dock_board object
#'
#' @return tagList of UI elements
#' @keywords internal
get_navbar_right <- function(ns, board) {
  providers <- dock_navbar_providers(board)
  if (length(providers) == 0L) return(NULL)

  tagList(lapply(providers, function(p) {
    ui_fn <- navbar_provider_right_ui(p)
    if (is.null(ui_fn)) return(NULL)
    ui_fn(ns(navbar_provider_id(p)))
  }))
}

#' Settings button UI
#'
#' Button that opens the settings offcanvas sidebar.
#'
#' @param ns Namespace function
#' @return Shiny tag
#' @keywords internal
settings_button_ui <- function(ns) {
  tags$button(
    id = ns("settings_btn"),
    class = "blockr-navbar-icon-btn",
    type = "button",
    `data-bs-toggle` = "offcanvas",
    `data-bs-target` = paste0("#", ns("settings_offcanvas")),
    bsicons::bs_icon("gear", size = "1.2em")
  )
}

#' Code button UI
#'
#' Button that triggers code generation.
#'
#' @param ns Namespace function
#' @return Shiny tag
#' @keywords internal
code_button_ui <- function(ns) {
  tags$button(
    id = ns("code_btn"),
    class = "blockr-navbar-icon-btn",
    type = "button",
    onclick = sprintf(
      "Shiny.setInputValue('%s', Date.now(), {priority: 'event'})",
      ns("code_btn")
    ),
    bsicons::bs_icon("code-slash", size = "1.2em")
  )
}

#' Settings offcanvas UI
#'
#' Offcanvas sidebar for board settings and extension options.
#'
#' @param id Namespace ID
#' @param board A dock_board object
#' @param options_ui UI for board options
#' @param extensions_ui UI for extension options
#'
#' @return Shiny tag
#' @keywords internal
options_offcanvas_ui <- function(id, board, options_ui, extensions_ui) {
  ns <- NS(id)
  tags$div(
    id = ns("settings_offcanvas"),
    class = "offcanvas offcanvas-end blockr-settings-offcanvas",
    tabindex = "-1",
    tags$div(
      class = "offcanvas-header",
      tags$h5(class = "offcanvas-title", "Settings"),
      tags$button(
        type = "button",
        class = "btn-close",
        `data-bs-dismiss` = "offcanvas"
      )
    ),
    tags$div(
      class = "offcanvas-body",
      options_ui,
      extensions_ui
    )
  )
}
