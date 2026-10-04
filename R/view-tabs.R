#' Views as tabs
#'
#' A board option that shows the board's views as a line of tabs under the
#' navbar, one click to switch, in addition to the views menu. It is saved
#' with the workflow, like the light/dark switch and "Compact" beside it under
#' "Theme options", and "Show views as tabs" in the views menu sets the same
#' option.
#'
#' @param value Logical, whether the views show as tabs. Defaults to the
#'   `view_tabs` blockr option, else `FALSE`.
#' @param category Options sidebar category.
#' @param ... Passed to [blockr.core::new_board_option()].
#'
#' @return A `board_option` object.
#'
#' @examples
#' new_view_tabs_option(TRUE)
#'
#' @export
new_view_tabs_option <- function(value = blockr_option("view_tabs", FALSE),
                                 category = "Theme options", ...) {

  value <- isTRUE(as.logical(value))

  new_board_option(
    id = "view_tabs",
    default = value,
    ui = function(id) {
      tags$div(
        class = "blockr-view-tabs-option",
        bslib::input_switch(NS(id, "view_tabs"), "Views as tabs", value)
      )
    },
    server = function(..., session) {
      observeEvent(
        get_board_option_or_null("view_tabs", session),
        {
          on <- isTRUE(get_board_option_value("view_tabs", session))
          bslib::toggle_switch("view_tabs", value = on, session = session)
          session$sendCustomMessage("blockr-view-tabs", on)
        }
      )
    },
    category = category,
    ...
  )
}

#' @export
validate_board_option.view_tabs_option <- function(x) {

  val <- board_option_value(NextMethod())

  if (!is_bool(val)) {
    blockr_abort(
      "Expecting `view_tabs` to be a boolean.",
      class = "board_options_view_tabs_invalid"
    )
  }

  invisible(x)
}

# Whether the navbar is first drawn with tabs, so a board saved with them does
# not flash the bar without them before the server's first push. A board saved
# before the option existed has none, and takes the blockr option.
view_tabs_on <- function(options) {

  if (!"view_tabs" %in% names(options)) {
    return(isTRUE(as.logical(blockr_option("view_tabs", FALSE))))
  }

  isTRUE(
    tryCatch(
      board_option_value(options[["view_tabs"]]),
      error = function(e) FALSE
    )
  )
}

# Puts `.blockr-view-tabs` on <html> as the page is parsed, ahead of the
# navbar, so a board with tabs never draws its bar without them first.
view_tabs_init <- function(options) {
  tags$script(
    HTML(
      sprintf(
        "document.documentElement.classList.toggle('blockr-view-tabs', %s);",
        if (view_tabs_on(options)) "true" else "false"
      )
    )
  )
}

# The tab line: filled and kept in step with the views menu by view-tabs.js,
# which forwards a tab's click to the menu's own item, so a switch takes the
# same path as one made in the menu.
view_tabs_ui <- function() {
  tagList(
    htmltools::htmlDependency(
      "blockr-view-tabs",
      pkg_version(),
      src = pkg_file("assets", "js"),
      script = "view-tabs.js"
    ),
    tags$span(class = "blockr-navbar-break", `data-navbar-slot` = "break"),
    tags$div(
      class = "blockr-view-tabs",
      role = "tablist",
      `aria-label` = "Views",
      `data-navbar-slot` = "viewtabs"
    )
  )
}

# The row at the foot of the views menu that turns the tabs on and off. It
# stands in for the option's switch in the options sidebar (view-tabs.js
# clicks that switch), and its check shows while <html> carries
# `.blockr-view-tabs`.
view_tabs_toggle_ui <- function() {
  tags$button(
    type = "button",
    class = paste(
      "dropdown-item blockr-menu__item blockr-menu__item--quiet",
      "blockr-view-tabs-toggle"
    ),
    role = "menuitemcheckbox",
    span(class = "blockr-menu__label", "Show views as tabs"),
    span(class = "blockr-menu__check", blockr.ui::small_icon("check"))
  )
}
