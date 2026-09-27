# Views as tabs: a second navbar line with one tab per view. Each user turns it
# on and off with "Show views as tabs" in the views menu. The choice is kept
# in the browser (localStorage), so it neither changes the workflow nor marks
# it unsaved, and each person keeps their own. `blockr_option("view_tabs")` is the
# default for someone who has not chosen yet.

view_tabs_key <- "blockr-view-tabs"

view_tabs_default <- function() {
  isTRUE(as.logical(blockr_option("view_tabs", FALSE)))
}

# Puts `.blockr-view-tabs` on <html> as the page is parsed, ahead of the
# navbar, so someone who chose tabs never sees the bar draw without them.
view_tabs_init <- function() {
  tags$script(
    HTML(
      sprintf(
        paste0(
          "(function () { var s = null;",
          " try { s = localStorage.getItem('%s'); } catch (e) {}",
          " document.documentElement.classList.toggle('blockr-view-tabs',",
          " s === null ? %s : s === '1'); })();"
        ),
        view_tabs_key,
        if (view_tabs_default()) "true" else "false"
      )
    )
  )
}

# The tab row: filled and kept in step with the views menu by view-tabs.js,
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

# The row at the foot of the views menu that turns the tabs on and off. Its
# check shows while <html> carries `.blockr-view-tabs`.
view_tabs_toggle_ui <- function() {
  tags$button(
    type = "button",
    class = paste(
      "dropdown-item blockr-menu__item blockr-menu__item--quiet",
      "blockr-view-tabs-toggle"
    ),
    role = "menuitemcheckbox",
    span(class = "blockr-menu__label", "Show views as tabs"),
    span(class = "blockr-menu__check", HTML(view_icons[["check"]]))
  )
}

#' Retired: where the board's pages were listed
#'
#' Showing views as tabs is now each user's own choice, made in the views
#' menu and kept in the browser. This board option did the same for everyone
#' and is kept only so that boards saved with it still restore. It has no UI
#' and no effect, and new boards do not carry it.
#'
#' @param value Ignored.
#' @param category Board options category.
#' @param ... Passed to [blockr.core::new_board_option()].
#'
#' @return A board option.
#' @keywords internal
#' @export
new_page_nav_option <- function(value = "path", category = "Board options",
                                ...) {

  new_board_option(
    id = "page_nav",
    default = value,
    ui = function(id) NULL,
    category = category,
    ...
  )
}

#' @rdname option_summary
#' @export
option_summary.page_nav_option <- function(x, value, ...) {
  NULL
}
