#' Compact block headers
#'
#' A board option that switches every block header to its compact form: an
#' eyebrow line over the block's content, with the block's mark as a small
#' tinted square and the name in small capitals. Off by default, which keeps
#' the regular header (a 32px mark and a 16px title that may wrap to two
#' lines). It sits with the light/dark switch under "Theme options".
#'
#' @param value Logical, whether headers start compact. Defaults to the
#'   `compact` blockr option, else `FALSE`.
#' @param category Options sidebar category.
#' @param ... Passed to [blockr.core::new_board_option()].
#'
#' @return A `board_option` object.
#'
#' @examples
#' new_compact_option(TRUE)
#'
#' @export
new_compact_option <- function(value = blockr_option("compact", FALSE),
                               category = "Theme options", ...) {

  new_board_option(
    id = "compact",
    default = value,
    ui = function(id) {
      bslib::input_switch(NS(id, "compact"), "Compact", value)
    },
    server = function(..., session) {
      observeEvent(
        get_board_option_or_null("compact", session),
        {
          on <- isTRUE(get_board_option_value("compact", session))
          bslib::toggle_switch("compact", value = on, session = session)
          session$sendCustomMessage("blockr-compact", on)
        }
      )
    },
    category = category,
    ...
  )
}

#' @export
validate_board_option.compact_option <- function(x) {

  val <- board_option_value(NextMethod())

  if (!is_bool(val)) {
    blockr_abort(
      "Expecting `compact` to be a boolean.",
      class = "board_options_compact_invalid"
    )
  }

  invisible(x)
}

compact_dep <- function() {
  htmltools::htmlDependency(
    "blockr-compact",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "compact.js"
  )
}

#' Rails in step across pages
#'
#' A board option that opens and closes the rails of every page together.
#' Collapsing or expanding the rail on one edge of a page does the same to the
#' rail on that edge of every other page, and a page visited for the first
#' time opens with its rails the way the pages already visited show theirs.
#' Only a rail holding panels takes part: an empty one is not shown. On by
#' default; switched off, each page keeps its rails as they were left there.
#'
#' @param value Logical, whether the rails are kept in step. Defaults to the
#'   `sync_rails` blockr option, else `TRUE`.
#' @param category Options sidebar category.
#' @param ... Passed to [blockr.core::new_board_option()].
#'
#' @return A `board_option` object.
#'
#' @examples
#' new_sync_rails_option(FALSE)
#'
#' @export
new_sync_rails_option <- function(value = blockr_option("sync_rails", TRUE),
                                  category = "Board options", ...) {

  new_board_option(
    id = "sync_rails",
    default = value,
    ui = function(id) {
      bslib::input_switch(
        NS(id, "sync_rails"),
        "Sync rails across pages",
        value
      )
    },
    server = function(..., session) {
      observeEvent(
        get_board_option_or_null("sync_rails", session),
        {
          on <- rails_synced(session)
          bslib::toggle_switch("sync_rails", value = on, session = session)
          session$sendCustomMessage("blockr-rail-sync", on)
        }
      )
    },
    category = category,
    ...
  )
}

#' @export
validate_board_option.sync_rails_option <- function(x) {

  val <- board_option_value(NextMethod())

  if (!is_bool(val)) {
    blockr_abort(
      "Expecting `sync_rails` to be a boolean.",
      class = "board_options_sync_rails_invalid"
    )
  }

  invisible(x)
}

# Whether the board keeps its rails in step. A board without the option, such
# as one saved before it existed or one built with options of its own, does
# not.
rails_synced <- function(session = get_session()) {
  isTRUE(isolate(get_board_option_or_null("sync_rails", session)))
}

# The board options sidebar: a list of option categories, each row opening
# a page with that category's options. The script options-sidebar.js
# switches between the list and the pages.

#' Build the body of the board-options sidebar.
#'
#' Returns the options sidebar's body: a list of option categories, each
#' opening a page with that category's options (see
#' `options_sidebar_ui()`). Called at server time from
#' `board_server_callback()` when the user clicks the navbar gear, and
#' passed to `show_sidebar()`.
#'
#' Caller-supplied `options` (threaded down from `serve(board, options =
#' custom_options(...))` via `blockr_app_server.dock_board()` →
#' `board_server_callback()` → `settings_observer()`) wins. When the
#' caller passed nothing, falls back to `blockr.core::blockr_app_options(x)`
#' so the sidebar still includes options contributed by blocks on the
#' board and by registered block constructors, the same set `serve()`
#' would have computed on the default path.
#'
#' @param id Board module id.
#' @param x Current board (`board$board`).
#' @param plugins Board plugins.
#' @param options Augmented board options (board + block contributions).
#'   `NULL` means "no caller override"; the default is recomputed via
#'   `blockr.core::blockr_app_options(x)`.
#' @noRd
settings_body <- function(
  id,
  x,
  plugins = board_plugins(x),
  options = NULL
) {
  opt_ui_or_null <- function(plg, plgs, x) {
    if (plg %in% names(plgs)) board_ui(id, plgs[[plg]], x)
  }

  generate_code <- div(
    id = "generate_code",
    opt_ui_or_null("generate_code", plugins, x)
  )

  # Locked board: the options pages write board state via
  # set_board_option_value(), which core's gate rejects while locked. Drop them
  # so the settings sidebar offers only the read-only generated-code export.
  if (is_dock_locked()) {
    return(generate_code)
  }

  # Caller-supplied `options` (threaded from `serve()` through
  # `blockr_app_server.dock_board()` / `settings_observer()`) wins; fall
  # back to the recomputed default only when the caller has nothing to say.
  options <- coal(options, blockr.core::blockr_app_options(x))

  stopifnot(is_board_options(options))

  options_sidebar_ui(id, options, generate_code = generate_code)
}

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
        lapply(opts, board_option_ui, id)
      )
    },
    names(cats),
    cats
  )

  code_row <- NULL
  if (not_null(generate_code)) {
    code_row <- tagList(div(class = "blockr-menu__gap"), generate_code)
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
