#' Body font
#'
#' A board option that picks the face the board is set in: Open Sans
#' (Bootstrap's, from bslib's Shiny preset) or Inter, which blockr.ui serves
#' from its own package. A trial, to compare the two on real boards. It sits
#' with the light/dark switch under "Theme options".
#'
#' @param value `"open-sans"` or `"inter"`. Defaults to the `font` blockr
#'   option, else `"open-sans"`.
#' @param category Options sidebar category.
#' @param ... Passed to [blockr.core::new_board_option()].
#'
#' @return A `board_option` object.
#'
#' @examples
#' new_font_option("inter")
#'
#' @export
new_font_option <- function(value = blockr_option("font", "open-sans"),
                            category = "Theme options", ...) {

  new_board_option(
    id = "font",
    default = value,
    ui = function(id) {
      blockr.ui::segmented_input(
        NS(id, "font"),
        "Font",
        c("Open Sans" = "open-sans", "Inter" = "inter"),
        selected = value
      )
    },
    server = function(..., session) {
      observeEvent(
        get_board_option_or_null("font", session),
        {
          font <- coal(get_board_option_value("font", session), "open-sans")
          blockr.ui::update_segmented_input("font", selected = font,
                                            session = session)
          session$sendCustomMessage("blockr-font", font)
        }
      )
    },
    category = category,
    ...
  )
}

#' @export
validate_board_option.font_option <- function(x) {

  val <- board_option_value(NextMethod())

  if (!is_string(val) || !val %in% c("open-sans", "inter")) {
    blockr_abort(
      "Expecting `font` to be \"open-sans\" or \"inter\".",
      class = "board_options_font_invalid"
    )
  }

  invisible(x)
}

font_dep <- function() {
  htmltools::htmlDependency(
    "blockr-font",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "font.js"
  )
}
