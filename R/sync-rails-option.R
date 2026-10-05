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
