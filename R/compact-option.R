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
      dock_checkbox(NS(id, "compact"), "Compact", value)
    },
    server = function(..., session) {
      observeEvent(
        get_board_option_or_null("compact", session),
        {
          on <- isTRUE(get_board_option_value("compact", session))
          updateCheckboxInput(session, "compact", value = on)
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
