# The board options sidebar: a list of option categories, each row the
# category's name with a one-line summary of its current values under it;
# a row opens the category's page, and the header's back arrow (or Escape)
# returns to the list. options-sidebar.js drives the switch; the summaries
# are computed here and kept current by options_summary_observer().

#' One-line summary of a board option's value
#'
#' The board options sidebar lists each option category with a short summary
#' of its current values. By default the summary is built from the value:
#' text and single values as they are, `TRUE` as the option's name, `FALSE`
#' and empty values left out, a list of single values as those values, any
#' other list or longer vector by its size. A package
#' can give its option class a better line with a method, for instance to add
#' a unit ("8 turns") or a noun ("12 variables").
#'
#' @param x A board option (see [blockr.core::new_board_option()]).
#' @param value The option's current value.
#' @param ... Passed on to methods.
#'
#' @return A string, or `NULL` for nothing to show.
#'
#' @export
option_summary <- function(x, value, ...) {
  UseMethod("option_summary")
}

#' @rdname option_summary
#' @export
option_summary.default <- function(x, value, ...) {

  if (is.null(value) || !length(value) || is.function(value)) {
    return(NULL)
  }

  # A list of single values (a set of column roles) reads as its values; a
  # list of anything bigger (a map, a catalog) by its size.
  if (is.list(value)) {
    flat <- Filter(Negate(is.null), value)
    if (length(flat) && all(lgl_ply(flat, is_scalar_value))) {
      parts <- Filter(nzchar, chr_ply(flat, as.character))
      if (length(parts)) {
        return(paste(parts, collapse = " \u00b7 "))
      }
      return(NULL)
    }
    n <- length(value)
    return(paste(n, if (n == 1L) "item" else "items"))
  }

  if (length(value) > 1L) {
    n <- length(value)
    return(paste(n, if (n == 1L) "item" else "items"))
  }

  if (is.logical(value)) {
    if (isTRUE(value)) {
      return(gsub("_", " ", board_option_id(x)))
    }
    return(NULL)
  }

  if (is.na(value)) {
    return(NULL)
  }

  txt <- as.character(value)

  if (!nzchar(txt)) {
    return(NULL)
  }

  txt
}

#' @rdname option_summary
#' @export
option_summary.dark_mode_option <- function(x, value, ...) {
  switch(coal(value, "system"), light = "Light", dark = "Dark", "System")
}

#' @rdname option_summary
#' @export
option_summary.compact_option <- function(x, value, ...) {
  if (isTRUE(value)) "Compact" else NULL
}

is_scalar_value <- function(x) {
  is.atomic(x) && length(x) == 1L && !is.na(x)
}

# The summary of one category: its options' summaries, joined.
category_summary <- function(opts, values) {

  parts <- unlist(
    Map(
      function(opt, val) {
        res <- tryCatch(option_summary(opt, val), error = function(e) NULL)
        if (is_string(res) && nzchar(res)) res
      },
      opts,
      values[chr_ply(opts, board_option_id)]
    ),
    use.names = FALSE
  )

  if (!length(parts)) {
    return("Not set")
  }

  paste(parts, collapse = " \u00b7 ")
}

# "Table options" in a sidebar called "Board options" reads twice; the row
# shows the category's noun.
category_label <- function(category) {
  sub(" options$", "", category)
}

# The categories, in the order the options come, with their options.
option_categories <- function(options) {
  cats <- chr_ply(options, board_option_category)
  split(as.list(options), factor(cats, levels = unique(cats)))
}

options_sidebar_ui <- function(id, options, generate_code = NULL) {

  cats <- option_categories(options)
  values <- lapply(
    set_names(as.list(options), chr_ply(options, board_option_id)),
    board_option_value
  )

  rows <- Map(
    function(cat, opts) {
      tags$button(
        type = "button",
        class = "blockr-menu__item blockr-options-row",
        `data-category` = cat,
        tags$span(
          class = "blockr-options-row-text",
          tags$span(class = "blockr-options-row-name", category_label(cat)),
          tags$span(
            class = "blockr-options-row-summary",
            category_summary(opts, values)
          )
        ),
        tags$span(class = "blockr-options-chevron", HTML(options_icons$chev))
      )
    },
    names(cats),
    cats
  )

  pages <- Map(
    function(cat, opts) {
      div(
        class = "blockr-options-page",
        `data-category` = cat,
        `data-label` = category_label(cat),
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
    div(class = "blockr-options-list", unname(rows), code_row),
    unname(pages)
  )
}

options_icons <- list(
  chev = paste0(
    '<svg width="12" height="12" viewBox="0 0 12 12" fill="none" ',
    'stroke="currentColor" stroke-width="1.25" stroke-linecap="round" ',
    'stroke-linejoin="round" aria-hidden="true"><path d="M4.5 3l3 3-3 3">',
    "</path></svg>"
  )
)

options_sidebar_dep <- function() {
  htmltools::htmlDependency(
    "blockr-options-sidebar",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "options-sidebar.js"
  )
}

# Keeps the list's summaries current: any option value that changes (an
# edit in a page, a restore, a value set from R) re-sends every category's
# line. Options without a value in the session (none registered) read as
# NULL and leave their part of the line out.
options_summary_observer <- function(options, session = get_session()) {

  cats <- option_categories(options)
  ids <- chr_ply(options, board_option_id)
  target <- session$ns("settings_sidebar")

  observe(
    {
      values <- lapply(
        set_names(nm = ids),
        get_board_option_or_null,
        session = session
      )

      session$sendCustomMessage(
        "blockr-options-summary",
        list(
          sidebar = target,
          rows = lapply(cats, category_summary, values = values)
        )
      )
    }
  )
}
