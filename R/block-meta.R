#' Get block metadata
#'
#' Returns various metadata for blocks or block categories, as well as styling
#' for block icons.
#'
#' - `blks_metadata()`: Retrieves metadata given a `block` or `blocks` object
#'   from the block registry. Can also handle blocks which are not
#'   registered and provides default values in that case. The `color` column
#'   is the category's colour, from [blockr.ui::category_color()].
#' - `blk_color()`: Deprecated. The category colours are blockr.ui's, and
#'   [blockr.ui::category_color()], which this now calls, looks them up.
#' - `blk_icon_data_uri()`: Deprecated. Processes block icons to add color and
#'   turn them into square-shaped icons. For the block's mark as an image,
#'   use [blockr.ui::block_mark_svg()], which draws it from the block's glyph
#'   and category.
#' - `block_status_badge()`: Derives a block's status badge from its eval
#'   status and error count -- the single derivation the dock card icon and
#'   the blockr.dag node badge share, so both show the same status. Returns a
#'   styling list (draw the badge) or `NULL` (no badge).
#'
#' @param blocks Blocks passed as `blocks` or `block` object
#'
#' @examples
#' blk <- blockr.core::new_dataset_block()
#' blks_metadata(blk)
#'
#' block_status_badge("waiting")
#'
#' @return Metadata is returned from `blks_metadata()` as a `data.frame` with
#' each row corresponding to a block. Both `blk_color()` and
#' `blk_icon_data_uri()` return character vectors. The badge
#' `block_status_badge()` draws is a list with its `label`; its fill as the
#' blockr.ui `token` and that token's light value `color`; `hollow` and
#' `outline`, for a badge drawn as an `outline`-wide ring in its fill; and the
#' dot's `size` and the width of the `ring` around it, in `ring_token` with the
#' light value `ring_color`. It is `NULL` for a status with no badge.
#'
#' @rdname meta
#' @export
blks_metadata <- function(blocks) {
  meta <- block_metadata(blocks)
  cbind(meta, color = blockr.ui::category_color(meta$category))
}

#' @param category Block category
#' @rdname meta
#' @export
blk_color <- function(category) {

  blockr_warn(
    "`blk_color()` is deprecated; use `blockr.ui::category_color()` instead.",
    class = "deprecated_blk_color",
    frequency = "once",
    frequency_id = "blockr_deprecated_blk_color"
  )

  blockr.ui::category_color(category)
}

#' @param icon_svg Character string containing the SVG icon markup
#' @param color Hex color code for the background
#' @param size Numeric size in pixels (default: 48)
#' @param mode Switch between URI and inline HTML mode
#' @rdname meta
#' @export
blk_icon_data_uri <- function(icon_svg, color, size = 48,
                              mode = c("uri", "inline")) {

  blockr_warn(
    "`blk_icon_data_uri()` is deprecated; use ",
    "`blockr.ui::block_mark_svg()` instead.",
    class = "deprecated_blk_icon_data_uri",
    frequency = "once",
    frequency_id = "blockr_deprecated_blk_icon_data_uri"
  )

  mode <- match.arg(mode)

  stopifnot(is_string(icon_svg), is_string(color), is.numeric(size))

  icon_style <- blockr_option("icon_style", "light")

  icon_content <- sub("^<svg[^>]*>", "", icon_svg)
  icon_content <- sub("</svg>$", "", icon_content)

  icon_size <- size * 0.6
  icon_offset <- size * 0.2
  corner_radius <- size * 0.15

  if (icon_style == "light") {
    icon_fill <- color
    bg_opacity <- 0.3
  } else {
    icon_fill <- "white"
    bg_opacity <- 1.0
  }

  bg_color <- hex_to_rgba(color, bg_opacity)

  svg <- sprintf(
    "<svg xmlns=\"http://www.w3.org/2000/svg\"
        width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\">
      <rect width=\"%d\" height=\"%d\" rx=\"%g\" ry=\"%g\" fill=\"%s\"/>
      <g transform=\"translate(%g, %g)\" fill=\"%s\">
        <svg width=\"%g\" height=\"%g\" viewBox=\"0 0 16 16\">%s</svg>
      </g>
    </svg>",
    size, size, size, size,
    size, size, corner_radius, corner_radius, bg_color,
    icon_offset, icon_offset, icon_fill,
    icon_size, icon_size, icon_content
  )

  if (mode == "inline") {
    return(HTML(svg))
  }

  paste0(
    "data:image/svg+xml;base64,",
    jsonlite::base64_enc(charToRaw(svg))
  )
}

#' Convert hex color to rgba with opacity
#' @param hex Hex color code
#' @param alpha Alpha/opacity value between 0 and 1
#' @keywords internal
#' @noRd
hex_to_rgba <- function(hex, alpha = 1.0) {
  # Remove # if present
  hex <- sub("^#", "", hex)

  # Convert hex to RGB
  r <- strtoi(substr(hex, 1, 2), base = 16)
  g <- strtoi(substr(hex, 3, 4), base = 16)
  b <- strtoi(substr(hex, 5, 6), base = 16)

  sprintf("rgba(%d, %d, %d, %g)", r, g, b, alpha)
}
