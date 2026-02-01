# =============================================================================
# Sidebar UI Components
# =============================================================================

# =============================================================================
# Sidebar Panel Helper
# =============================================================================

#' Sidebar Panel
#'
#' Creates a standardized sidebar panel structure with body and optional footer.
#' Use this in `sidebar_content_ui` methods to ensure consistent layout.
#'
#' @param body Main content for the sidebar (Shiny tags)
#' @param footer Optional footer content (Shiny tags), rendered sticky at bottom
#'
#' @return Shiny tagList with proper structure
#'
#' @examples
#' \dontrun{
#' sidebar_content_ui.my_sidebar <- function(type, ns, board, ...) {
#'   list(
#'     title = "My Sidebar",
#'     content = sidebar_panel(
#'       body = tagList(
#'         selectInput(ns("choice"), "Choose:", c("A", "B", "C"))
#'       ),
#'       footer = actionButton(ns("confirm"), "Confirm")
#'     )
#'   )
#' }
#' }
#'
#' @export
sidebar_panel <- function(body, footer = NULL) {
  tagList(
    tags$div(class = "blockr-sidebar-body", body),
    if (!is.null(footer)) {
      tags$div(class = "blockr-sidebar-sticky-footer", footer)
    }
  )
}

# =============================================================================
# S3 Generic for Sidebar Content
# =============================================================================

#' Sidebar Content UI
#'
#' S3 generic for sidebar content. Extensions define methods to add custom
#' sidebar types. Each method should return a list with metadata and content.
#'
#' @param type Sidebar type object (S3 class = type name, e.g., "add_block")
#' @param ns Namespace function
#' @param board The board object
#' @param ... Additional arguments (e.g., search_term, input)
#'
#' @return List with:
#' \describe{
#'   \item{title}{Character, sidebar title}
#'   \item{subtitle}{Character or NULL, optional subtitle}
#'   \item{show_search}{Logical, whether to show search input}
#'   \item{content}{Shiny UI tags, typically created with [sidebar_panel()]}
#' }
#'
#' @examples
#' \dontrun{
#' # Define a custom sidebar type in your package:
#' sidebar_content_ui.my_custom <- function(type, ns, board, ...) {
#'   list(
#'     title = "My Custom Sidebar",
#'     subtitle = "Do something custom",
#'     show_search = FALSE,
#'     content = sidebar_panel(
#'       body = tagList(
#'         textInput(ns("my_input"), "Enter value")
#'       ),
#'       footer = actionButton(ns("my_submit"), "Submit")
#'     )
#'   )
#' }
#'
#' # Then trigger it:
#' show_sidebar("my_custom")
#' }
#'
#' @export
sidebar_content_ui <- function(type, ...) {
  UseMethod("sidebar_content_ui")
}

#' @export
sidebar_content_ui.default <- function(type, ns, board, ...) {
  id <- if (is.list(type)) type$id else class(type)[1]
  list(
    title = "Unknown Sidebar",
    subtitle = paste0("Type: ", id),
    show_search = FALSE,
    content = sidebar_panel(
      body = tags$div(
        class = "blockr-sidebar-empty",
        tags$p(paste0("No sidebar_content_ui.", id, "() method found."))
      )
    )
  )
}


# =============================================================================
# Built-in Sidebar Content Methods
# =============================================================================

#' @export
sidebar_content_ui.add_block <- function(type, ns, board,
                                         search_term = NULL, ...) {
  list(
    title = "Add Block",
    subtitle = "Select a block type",
    show_search = TRUE,
    content = sidebar_panel(
      body = render_block_sidebar_content(ns, board, mode = "add",
                                          search_term = search_term)
    )
  )
}

#' @export
sidebar_content_ui.append_block <- function(type, ns, board,
                                            search_term = NULL, ...) {
  list(
    title = "Append Block",
    subtitle = "Select a block to append",
    show_search = TRUE,
    content = sidebar_panel(
      body = render_block_sidebar_content(ns, board, mode = "append",
                                          search_term = search_term)
    )
  )
}

#' @export
sidebar_content_ui.prepend_block <- function(type, ns, board,
                                             search_term = NULL, ...) {
  list(
    title = "Prepend Block",
    subtitle = "Select a block to prepend",
    show_search = TRUE,
    content = sidebar_panel(
      body = render_block_sidebar_content(ns, board, mode = "prepend",
                                          search_term = search_term)
    )
  )
}

#' @export
sidebar_content_ui.add_link <- function(type, ns, board,
                                        search_term = NULL, ...) {
  ctx <- get_sidebar_context()
  source_block <- ctx$source_block

  body <- if (is.null(source_block)) {
    tags$div(
      class = "blockr-sidebar-empty",
      tags$p("No source block specified")
    )
  } else {
    render_link_sidebar_content(ns, board,
                                source_block = source_block,
                                search_term = search_term)
  }

  list(
    title = "Add Link",
    subtitle = "Select target block",
    show_search = TRUE,
    content = sidebar_panel(body = body)
  )
}

#' @export
sidebar_content_ui.create_stack <- function(type, ns, board, search_term = NULL,
                                            current_selection = NULL, ...) {
  list(
    title = "Create Stack",
    subtitle = "Select blocks for stack",
    show_search = TRUE,
    content = sidebar_panel(
      body = render_stack_sidebar_content(ns, board, mode = "create",
                                          search_term = search_term,
                                          current_selection = current_selection),
      footer = render_stack_sidebar_footer(ns, board, mode = "create")
    )
  )
}

#' @export
sidebar_content_ui.edit_stack <- function(type, ns, board, search_term = NULL,
                                          current_selection = NULL, ...) {
  ctx <- get_sidebar_context()
  stack_id <- ctx$stack_id

  if (is.null(stack_id)) {
    return(list(
      title = "Edit Stack",
      subtitle = NULL,
      show_search = FALSE,
      content = sidebar_panel(
        body = tags$div(
          class = "blockr-sidebar-empty",
          tags$p("No stack specified")
        )
      )
    ))
  }

  stack <- board_stacks(board)[[stack_id]]

  list(
    title = "Edit Stack",
    subtitle = "Select blocks for stack",
    show_search = TRUE,
    content = sidebar_panel(
      body = render_stack_sidebar_content(ns, board, mode = "edit",
                                          stack = stack, stack_id = stack_id,
                                          search_term = search_term,
                                          current_selection = current_selection),
      footer = render_stack_sidebar_footer(ns, board, mode = "edit",
                                           stack = stack, stack_id = stack_id)
    )
  )
}

#' @export
sidebar_content_ui.add_panel <- function(type, ns, board,
                                         search_term = NULL, ...) {
  ctx <- get_sidebar_context()

  list(
    title = "Add Panel",
    subtitle = "Show block or extension",
    show_search = TRUE,
    content = sidebar_panel(
      body = render_dock_sidebar_content(
        ns, board,
        visible_block_ids = ctx$visible_block_ids %||% character(),
        visible_ext_ids = ctx$visible_ext_ids %||% character(),
        search_term = search_term
      )
    )
  )
}

#' @export
sidebar_content_ui.settings <- function(type, ns, board,
                                        generate_code_ui = NULL, ...) {
  list(
    title = "Board Options",
    subtitle = NULL,
    show_search = FALSE,
    content = sidebar_panel(
      body = tagList(
        if (!is.null(generate_code_ui)) {
          tags$div(
            class = "blockr-settings-generate-code",
            generate_code_ui
          )
        },
        render_settings_sidebar_content(ns, board)
      )
    )
  )
}


# =============================================================================
# Sidebar Container UI (outer shell)
# =============================================================================

#' Sidebar Container UI
#'
#' Creates the sidebar container with header, search, and dynamic content area.
#' Content is rendered dynamically via S3 dispatch based on the current
#' sidebar type.
#'
#' @param ns Namespace function
#'
#' @return Shiny tags containing the sidebar container
#'
#' @keywords internal
sidebar_container_ui <- function(ns) {
  tags$div(
    id = ns("sidebar"),
    class = "blockr-sidebar-container",
    role = "complementary",
    `aria-label` = "Sidebar",
    blockr_sidebar_dep(),

    # Single sidebar panel
    tags$div(
      id = ns("sidebar_panel"),
      class = "blockr-sidebar blockr-sidebar-hidden",
      role = "dialog",
      `aria-modal` = "false",
      `aria-labelledby` = ns("sidebar_title"),

      # Header
      tags$div(
        class = "blockr-sidebar-header",
        tags$button(
          type = "button",
          class = "blockr-sidebar-close",
          onclick = "blockrSidebar.hide()",
          `aria-label` = "Close sidebar",
          bsicons::bs_icon("x-lg")
        ),
        tags$h3(id = ns("sidebar_title"), class = "blockr-sidebar-title"),
        tags$span(id = ns("sidebar_subtitle"), class = "blockr-sidebar-subtitle")
      ),

      # Search (shown/hidden via JS based on sidebar type)
      tags$div(
        id = ns("sidebar_search_container"),
        class = "blockr-sidebar-search",
        style = "display: none;",
        tags$div(
          class = "blockr-sidebar-search-wrapper",
          tags$span(
            class = "blockr-sidebar-search-icon",
            `aria-hidden` = "true",
            bsicons::bs_icon("search")
          ),
          tags$input(
            type = "text",
            id = ns("sidebar_search"),
            class = "blockr-sidebar-search-input shiny-input-text",
            placeholder = "Search...",
            `aria-label` = "Search blocks",
            autocomplete = "off"
          )
        )
      ),

      # Dynamic content area - single uiOutput for S3-dispatched content
      tags$div(
        class = "blockr-sidebar-content",
        role = "list",
        uiOutput(ns("sidebar_dynamic_content"))
      )
    )
  )
}


#' Sidebar UI
#'
#' Creates the sidebar container. This is an alias for `sidebar_container_ui()`
#' for backwards compatibility.
#'
#' @param ns Namespace function
#' @param board The board object (unused, kept for compatibility)
#' @param generate_code_ui Optional UI for the generate code plugin (stored in
#'   session for settings sidebar)
#'
#' @return Shiny tags containing the sidebar
#'
#' @keywords internal
sidebar_ui <- function(ns, board = NULL, generate_code_ui = NULL) {
  # Store generate_code_ui in an attribute for the settings sidebar to use
  ui <- sidebar_container_ui(ns)
  attr(ui, "generate_code_ui") <- generate_code_ui
  ui
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
                              color = NULL, badge = NULL,
                              accordion_content = NULL, on_click_input = NULL,
                              on_click_data = NULL) {

  has_accordion <- !is.null(accordion_content)
  card_id <- paste0("card_", id)

  # Build data attributes for main card click
  main_data_attrs <- if (!is.null(on_click_input) && !is.null(on_click_data)) {
    attrs <- list(
      `data-action` = "card-click",
      `data-input-id` = ns(on_click_input)
    )
    # Add all on_click_data as data attributes
    for (key in names(on_click_data)) {
      attrs[[paste0("data-", key)]] <- on_click_data[[key]]
    }
    attrs
  } else {
    list()
  }

  accordion_id <- if (has_accordion) paste0(card_id, "_accordion") else NULL

  tags$div(
    id = ns(card_id),
    class = paste("blockr-block-card", if (has_accordion) "has-accordion"),
    `data-block-id` = id,
    role = "listitem",

    # Main clickable area
    do.call(tags$div, c(
      list(class = "blockr-block-card-main", tabindex = "0"),
      main_data_attrs,
      list(
        # Icon (color is embedded in SVG when color param is provided)
        tags$div(
          class = if (is.null(color)) {
            paste0("blockr-block-card-icon category-", category)
          } else {
            "blockr-block-card-icon"
          },
          `aria-hidden` = "true",
          if (!is.null(color)) {
            blk_icon_data_uri(icon, color, 40, "inline")
          } else {
            HTML(icon)
          }
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
              `data-action` = "toggle-accordion",
              `data-card-id` = ns(card_id),
              `aria-expanded` = "false",
              `aria-controls` = ns(accordion_id),
              `aria-label` = "Show options",
              bsicons::bs_icon("chevron-down")
            )
          }
        )
      )
    )),

    # Accordion content
    if (has_accordion) {
      tags$div(
        id = ns(accordion_id),
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


#' Sidebar JS Dependency
#'
#' Creates HTML dependency for the blockr sidebar JavaScript module.
#'
#' @return htmltools::htmlDependency object
#'
#' @keywords internal
blockr_sidebar_dep <- function() {
  htmltools::htmlDependency(
    "blockr-sidebar",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "blockr-sidebar.js"
  )
}
