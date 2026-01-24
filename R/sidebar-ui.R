# =============================================================================
# Sidebar UI Components
# =============================================================================

#' Sidebar Containers UI
#'
#' Creates all sidebar containers for a board.
#'
#' @param ns Namespace function
#' @param board The board object
#'
#' @return Shiny tags containing all sidebar containers
#'
#' @keywords internal
sidebars_ui <- function(ns, board) {

  tagList(
    # Add Block panel
    sidebar_container_ui(
      ns = ns,
      id = "add_block",
      title = "Add Block",
      subtitle = "Select a block type",
      show_search = TRUE,
      content = uiOutput(ns("add_block_content"))
    ),

    # Append Block panel
    sidebar_container_ui(
      ns = ns,
      id = "append_block",
      title = "Append Block",
      subtitle = "Select a block to append",
      show_search = TRUE,
      content = uiOutput(ns("append_block_content"))
    ),

    # Prepend Block panel
    sidebar_container_ui(
      ns = ns,
      id = "prepend_block",
      title = "Prepend Block",
      subtitle = "Select a block to prepend",
      show_search = TRUE,
      content = uiOutput(ns("prepend_block_content"))
    ),

    # Add Link panel
    sidebar_container_ui(
      ns = ns,
      id = "add_link",
      title = "Add Link",
      subtitle = "Select target block",
      show_search = TRUE,
      content = uiOutput(ns("add_link_content"))
    ),

    # Create Stack panel
    sidebar_container_ui(
      ns = ns,
      id = "create_stack",
      title = "Create Stack",
      subtitle = "Select blocks for stack",
      show_search = TRUE,
      content = uiOutput(ns("create_stack_content")),
      footer_content = uiOutput(ns("create_stack_footer"))
    ),

    # Edit Stack panel
    sidebar_container_ui(
      ns = ns,
      id = "edit_stack",
      title = "Edit Stack",
      subtitle = "Select blocks for stack",
      show_search = TRUE,
      content = uiOutput(ns("edit_stack_content")),
      footer_content = uiOutput(ns("edit_stack_footer"))
    ),

    # Add Panel (dock panel) panel
    sidebar_container_ui(
      ns = ns,
      id = "add_panel",
      title = "Add Panel",
      subtitle = "Show block or extension",
      show_search = TRUE,
      content = uiOutput(ns("add_panel_content"))
    )
  )
}


#' Sidebar Container UI
#'
#' Creates a slide-out sidebar container.
#'
#' @param ns Namespace function
#' @param id Sidebar ID (e.g., "add_block")
#' @param title Sidebar title
#' @param subtitle Sidebar subtitle (optional)
#' @param show_search Whether to show search box
#' @param footer_content Custom footer content (optional)
#' @param content Sidebar content (typically uiOutput)
#'
#' @return Shiny tags
#'
#' @keywords internal
sidebar_container_ui <- function(ns, id, title, subtitle = NULL,
                                 show_search = TRUE, footer_content = NULL,
                                 content = NULL) {

  sidebar_id <- paste0(id, "_sidebar")
  search_id <- paste0(id, "_search")

  footer <- if (!is.null(footer_content)) {
    tags$div(class = "blockr-sidebar-footer", footer_content)
  }

  tags$div(
    id = ns(sidebar_id),
    class = "blockr-sidebar blockr-sidebar-hidden",
    `data-sidebar-id` = id,

    # Header
    tags$div(
      class = "blockr-sidebar-header",
      tags$button(
        type = "button",
        class = "blockr-sidebar-close",
        onclick = sprintf(
          "Shiny.setInputValue('%s', Date.now(), {priority: 'event'})",
          ns("close_sidebar")
        ),
        bsicons::bs_icon("x-lg")
      ),
      tags$h3(class = "blockr-sidebar-title", title),
      if (!is.null(subtitle)) {
        tags$span(class = "blockr-sidebar-subtitle", subtitle)
      }
    ),

    # Search
    if (show_search) {
      tags$div(
        class = "blockr-sidebar-search",
        tags$div(
          class = "blockr-sidebar-search-wrapper",
          tags$span(
            class = "blockr-sidebar-search-icon",
            bsicons::bs_icon("search")
          ),
          tags$input(
            type = "text",
            id = ns(search_id),
            class = "blockr-sidebar-search-input shiny-input-text",
            placeholder = "Search...",
            autocomplete = "off"
          )
        )
      )
    },

    # Content
    tags$div(
      class = "blockr-sidebar-content",
      content
    ),

    # Footer
    footer
  )
}


#' Render Block Card
#'
#' Renders a single block card with optional accordion for advanced options.
#'
#' @param ns Namespace function
#' @param id Card ID
#' @param title Card title
#' @param description Card description
#' @param icon Block icon (HTML)
#' @param category Block category
#' @param badge Optional badge text
#' @param accordion_content Content for expandable accordion
#' @param on_click_input Input ID to set on click
#' @param on_click_data Data to send on click
#'
#' @return Shiny tags
#'
#' @keywords internal
render_block_card <- function(ns, id, title, description, icon, category,
                              badge = NULL, accordion_content = NULL,
                              on_click_input = NULL, on_click_data = NULL) {

  has_accordion <- !is.null(accordion_content)
  card_id <- paste0("card_", id)


  # Build onclick for main card area
  # Note: We build the JS object manually to include Date.now()
  main_onclick <- if (!is.null(on_click_input) && !is.null(on_click_data)) {
    # Convert data to JS object notation with Date.now() for timestamp
    js_pairs <- mapply(function(k, v) {
      if (is.character(v)) {
        sprintf("'%s':'%s'", k, gsub("'", "\\\\'", v))
      } else {
        sprintf("'%s':%s", k, v)
      }
    }, names(on_click_data), on_click_data, SIMPLIFY = TRUE)
    js_obj <- paste0(
      "{", paste(c(js_pairs, "timestamp:Date.now()"), collapse = ","), "}"
    )
    sprintf(
      "Shiny.setInputValue('%s', %s, {priority: 'event'})",
      ns(on_click_input),
      js_obj
    )
  }

  # Chevron onclick toggles accordion
  chevron_onclick <- if (has_accordion) {
    # nolint start: line_length_linter.
    sprintf(
      "event.stopPropagation(); document.getElementById('%s').classList.toggle('is-open');",
      ns(card_id)
    )
    # nolint end.
  }

  tags$div(
    id = ns(card_id),
    class = paste("blockr-block-card", if (has_accordion) "has-accordion"),
    `data-block-id` = id,

    # Main clickable area
    tags$div(
      class = "blockr-block-card-main",
      onclick = main_onclick,

      # Icon
      tags$div(
        class = paste0("blockr-block-card-icon category-", category),
        HTML(icon)
      ),

      # Content
      tags$div(
        class = "blockr-block-card-content",
        tags$p(class = "blockr-block-card-title", title),
        tags$p(class = "blockr-block-card-description", description)
      ),

      # Right section: badge + chevron side by side
      tags$div(
        class = "blockr-block-card-right",
        if (!is.null(badge)) {
          tags$span(class = "badge-two-tone", badge)
        },
        if (has_accordion) {
          tags$button(
            type = "button",
            class = "blockr-block-card-chevron",
            onclick = chevron_onclick,
            bsicons::bs_icon("chevron-down")
          )
        }
      )
    ),

    # Accordion content
    if (has_accordion) {
      tags$div(
        class = "blockr-block-card-accordion",
        tags$div(
          class = "blockr-block-card-accordion-inner",
          accordion_content
        )
      )
    }
  )
}


#' Render Category Header
#'
#' @param title Category title
#'
#' @return Shiny tags
#'
#' @keywords internal
render_category_header <- function(title) {
  tags$div(
    class = "blockr-category-header",
    tags$span(title)
  )
}


#' Group Items by Category
#'
#' Groups a data frame by category for rendering with headers.
#'
#' @param df Data frame with 'category' column
#'
#' @return Named list of data frames, one per category
#'
#' @keywords internal
group_by_category <- function(df) {
  if (!"category" %in% names(df) || nrow(df) == 0) {
    return(list(default = df))
  }
  split(df, df$category)
}
