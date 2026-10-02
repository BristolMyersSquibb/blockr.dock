# The board options sidebar: a list of option categories, each row opening
# a page with that category's options. The script options-sidebar.js
# switches between the list and the pages.

# The categories, in the order the options come, with their options.
option_categories <- function(options) {
  cats <- chr_ply(options, board_option_category)
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
        lapply(opts, board_option_ui, id)
      )
    },
    names(cats),
    cats
  )

  code_row <- NULL
  if (not_null(generate_code)) {
    code_row <- tagList(
      div(class = "blockr-menu__gap"),
      div(class = "blockr-options-code", generate_code)
    )
  }

  div(
    class = "blockr-options",
    options_sidebar_dep(),
    div(class = "blockr-options-list", rows, code_row),
    unname(pages)
  )
}

options_sidebar_dep <- function() {
  htmltools::htmlDependency(
    "blockr-options-sidebar",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "options-sidebar.js"
  )
}
