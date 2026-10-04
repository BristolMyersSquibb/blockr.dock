# The board options sidebar: a list of option categories, each row opening
# a page with that category's options. The script options-sidebar.js
# switches between the list and the pages.

# The categories, in the order the options come, with their options. An
# option need not have a category, and goes under "Other options" without.
option_categories <- function(options) {
  cats <- chr_ply(
    options,
    function(x) coal(board_option_category(x), "Other options")
  )
  split(as.list(options), factor(cats, levels = unique(cats)))
}

options_sidebar_ui <- function(id, options, generate_code = NULL) {

  cats <- option_categories(options)

  rows <- lapply(
    names(cats),
    function(cat) {
      tags$button(
        type = "button",
        class = "blockr-menu__item blockr-options-row",
        `data-category` = cat,
        tags$span(class = "blockr-options-row-name", cat),
        tags$span(
          class = "blockr-options-chevron",
          blockr.ui::small_icon("chevron")
        )
      )
    }
  )

  pages <- Map(
    function(cat, opts) {
      div(
        class = "blockr-options-page",
        `data-category` = cat,
        hidden = NA,
        lapply(opts, dock_option_ui, id)
      )
    },
    names(cats),
    cats
  )

  # "Show code" ends the list after a gap, and opens a page of its own with
  # the board as an R script.
  code_row <- code_page <- NULL
  if (not_null(generate_code)) {
    code_row <- tagList(
      div(class = "blockr-menu__gap"),
      tags$button(
        type = "button",
        class = paste(
          "blockr-menu__item blockr-menu__item--quiet blockr-options-row",
          "blockr-options-code"
        ),
        `data-category` = code_page_id,
        tags$span(class = "blockr-menu__icon", blockr.ui::small_icon("code")),
        tags$span(class = "blockr-menu__label", "Show code")
      )
    )
    code_page <- div(
      class = "blockr-options-page blockr-options-code-page",
      `data-category` = code_page_id,
      `data-label` = "Code",
      hidden = NA,
      generate_code
    )
  }

  div(
    class = "blockr-options",
    options_sidebar_dep(),
    div(class = "blockr-options-list", rows, code_row),
    unname(pages),
    code_page
  )
}

# The code page's key among the category pages; no option category is named
# with a leading dot.
code_page_id <- ".code"

options_sidebar_dep <- function() {
  htmltools::htmlDependency(
    "blockr-options-sidebar",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "options-sidebar.js"
  )
}
