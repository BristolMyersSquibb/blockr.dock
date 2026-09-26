#' Where the board's pages are listed
#'
#' A board option for the page navigation: `"path"` shows the current page as
#' the last step of the navbar's path (blockr / workflow / page), with the
#' other pages in its menu; `"tabs"` lists every page as a tab in a row under
#' the navbar, one click to switch, and keeps the page menu at the end of that
#' row for adding, renaming and reordering pages.
#'
#' @param value Either `"path"` or `"tabs"`.
#' @param category Board options category.
#' @param ... Passed to [blockr.core::new_board_option()].
#'
#' @return A board option.
#' @export
new_page_nav_option <- function(value = blockr_option("page_nav", "path"),
                                 category = "Board options", ...) {

  new_board_option(
    id = "page_nav",
    default = value,
    ui = function(id) {
      radioButtons(
        NS(id, "page_nav"),
        "Pages",
        choices = c(
          "In the navbar path" = "path",
          "As tabs under the navbar" = "tabs"
        ),
        selected = value
      )
    },
    server = function(..., session) {
      observeEvent(
        get_board_option_or_null("page_nav", session),
        session$sendCustomMessage(
          "blockr-page-nav",
          list(mode = get_board_option_value("page_nav", session))
        )
      )
    },
    category = category,
    ...
  )
}

#' @export
validate_board_option.page_nav_option <- function(x) {

  val <- board_option_value(NextMethod())

  if (!(is_string(val) && val %in% c("path", "tabs"))) {
    blockr_abort(
      "Expecting `page_nav` to be either \"path\" or \"tabs\".",
      class = "board_options_page_nav_invalid"
    )
  }

  invisible(x)
}

#' @rdname option_summary
#' @export
option_summary.page_nav_option <- function(x, value, ...) {
  switch(coal(value, "path"), tabs = "Pages as tabs", "Pages in the path")
}

# The mode the navbar is first drawn in, so a board saved with tabs does not
# flash the path before the server's first push.
page_nav_mode <- function(options) {

  if (!"page_nav" %in% names(options)) {
    return("path")
  }

  val <- tryCatch(
    board_option_value(options[["page_nav"]]),
    error = function(e) NULL
  )

  if (is_string(val) && val %in% c("path", "tabs")) val else "path"
}

# The tab row: filled and kept in step with the page menu by page-tabs.js,
# which forwards a tab's click to the menu's own item, so a switch takes the
# same path as one made in the menu.
page_tabs_ui <- function() {
  tagList(
    htmltools::htmlDependency(
      "blockr-page-tabs",
      pkg_version(),
      src = pkg_file("assets", "js"),
      script = "page-tabs.js"
    ),
    tags$span(class = "blockr-navbar-break", `data-navbar-slot` = "break"),
    tags$div(
      class = "blockr-page-tabs",
      role = "tablist",
      `aria-label` = "Pages",
      `data-navbar-slot` = "pagetabs"
    )
  )
}
