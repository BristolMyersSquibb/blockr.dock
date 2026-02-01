# =============================================================================
# Sidebar API
# =============================================================================
#
#' Sidebar API
#'
#' @description
#' A slide-out sidebar system for blockr.dock. The sidebar dynamically renders
#' content based on S3 dispatch, allowing extensions to add custom sidebar types
#' by defining `sidebar_content_ui.*` methods.
#'
#' @section Functions:
#'
#' - [show_sidebar()] - Show sidebar with specific content section
#' - [hide_sidebar()] - Hide the sidebar
#' - [set_sidebar_context()] - Set context data for sidebar renderers
#' - [get_sidebar_context()] - Retrieve context data
#'
#' @section Usage:
#' ```r
#' # Show sidebar with the "add_block" content type
#' show_sidebar("add_block")
#'
#' # With context data
#' set_sidebar_context(source_block = "blk_1")
#' show_sidebar("add_link")
#'
#' # Hide
#' hide_sidebar()
#' ```
#'
#' @section For Extensions:
#' Extensions can add custom sidebar types by defining S3 methods:
#' ```r
#' sidebar_content_ui.my_sidebar <- function(type, ns, board, ...) {
#'   list(
#'     title = "My Sidebar",
#'     subtitle = "Optional subtitle",
#'     show_search = FALSE,
#'     content = sidebar_panel(
#'       body = tagList(...),
#'       footer = actionButton(ns("confirm"), "Confirm")  # optional
#'     )
#'   )
#' }
#' ```
#' Then trigger with `show_sidebar("my_sidebar")`.
#'
#' @param session Shiny session (default: current reactive domain)
#'
#' @name sidebar
#' @aliases sidebar-api
NULL


#' @param id The sidebar type ID (e.g., "add_block"). Used for S3 dispatch.
#' @param ... Named context values to store (passed to `set_sidebar_context`)
#' @rdname sidebar
#' @export
show_sidebar <- function(id, ..., session = shiny::getDefaultReactiveDomain()) {
  # Store context if provided
  ctx <- list(...)
  if (length(ctx)) {
    do.call(set_sidebar_context, c(ctx, list(session = session)))
  }

  # Create type object with class = id (for S3 dispatch)
  type <- structure(list(id = id), class = c(id, "sidebar_type"))

  # Initialize reactive if needed (may be called before sidebar_server)
  if (is.null(session$userData$blockr_sidebar_type)) {
    session$userData$blockr_sidebar_type <- reactiveVal(NULL)
  }

  # Store type in reactive for renderUI to pick up
  session$userData$blockr_sidebar_type(type)

  # Show sidebar via JS
  session$sendCustomMessage("blockr-sidebar", list(action = "show"))
  invisible()
}


#' @rdname sidebar
#' @export
hide_sidebar <- function(session = shiny::getDefaultReactiveDomain()) {
  # Clear context and type when hiding
  if (!is.null(session$userData$blockr_sidebar_context)) {
    session$userData$blockr_sidebar_context(list())
  }
  if (!is.null(session$userData$blockr_sidebar_type)) {
    session$userData$blockr_sidebar_type(NULL)
  }
  session$sendCustomMessage("blockr-sidebar", list(action = "hide"))
  invisible()
}


#' Get Current Sidebar Type
#'
#' Returns the current sidebar type object (for S3 dispatch).
#'
#' @param session Shiny session
#'
#' @return Sidebar type object or NULL if sidebar is hidden
#'
#' @keywords internal
get_sidebar_type <- function(session = shiny::getDefaultReactiveDomain()) {
  rv <- session$userData$blockr_sidebar_type
  if (is.null(rv)) NULL else rv()
}


#' Set Sidebar Context
#'
#' Sets context data that sidebar content renderers can access.
#' Call this before `show_sidebar()` when the content needs additional data.
#'
#' @param ... Named values to store in context
#' @param session Shiny session
#'
#' @examples
#' \dontrun{
#' # Set context, then show sidebar
#' set_sidebar_context(source_block = "blk_1")
#' show_sidebar("add_link")
#' }
#'
#' @export
set_sidebar_context <- function(..., session = shiny::getDefaultReactiveDomain()) {
  ctx <- list(...)
  if (is.null(session$userData$blockr_sidebar_context)) {
    session$userData$blockr_sidebar_context <- reactiveVal(list())
  }
  session$userData$blockr_sidebar_context(ctx)
  invisible()
}


#' Get Sidebar Context
#'
#' Retrieves context data set by `set_sidebar_context()`.
#'
#' @param session Shiny session
#'
#' @return List of context values
#'
#' @export
get_sidebar_context <- function(session = shiny::getDefaultReactiveDomain()) {
  rv <- session$userData$blockr_sidebar_context
  if (is.null(rv)) list() else rv()
}
