#' Get block color based on category
#'
#' Returns metadata such as color hex codes for block categories using the
#' Okabe-Ito colorblind-friendly palette.
#'
#' @param category Block category
#' @rdname meta
#' @export
blk_color <- function(category) {
  # Okabe-Ito colorblind-friendly palette
  # See: https://jfly.uni-koeln.de/color/
  switch(
    category,
    input = "#0072B2", # Blue
    transform = "#009E73", # Bluish green
    structured = "#56B4E9", # Sky blue
    plot = "#E69F00", # Orange
    table = "#CC79A7", # Reddish purple/pink
    model = "#F0E442", # Yellow (includes AI/ML)
    output = "#D55E00", # Vermilion
    utility = "#CCCCCC", # Light gray
    "#999999" # Medium gray (uncategorized)
  )
}

#' @param blocks Blocks passed as `blocks` or `block` object
#' @rdname meta
#' @export
blks_metadata <- function(blocks) {
  default_name <- function(x) {
    gsub("_", " ", class(x)[1L])
  }

  if (is_block(blocks)) {
    id <- registry_id_from_block(blocks)
  } else if (is_blocks(blocks)) {
    id <- lapply(blocks, registry_id_from_block)
  } else {
    blockr_abort("Unsupported input type for `blocks`.")
  }

  if (any(lengths(id) == 0L)) {
    cat <- default_category()

    res <- data.frame(
      id = id[lengths(id) == 0L],
      name = chr_ply(blocks[lengths(id) == 0L], default_name),
      description = "not available",
      category = cat,
      icon = default_icon(cat),
      package = "local",
      color = blk_color(cat)
    )

    if (is_blocks(blocks)) {
      rownames(res) <- names(blocks)[lengths(id) == 0L]
    }
  } else {
    res <- NULL
  }

  if (any(lengths(id) > 0L)) {
    reg <- block_metadata(id[lengths(id) > 0L])
    reg <- cbind(reg, color = chr_ply(reg$category, blk_color))

    if (is_blocks(blocks)) {
      rownames(reg) <- names(blocks)[lengths(id) > 0L]
    }

    res <- rbind(res, reg)

    if (is_blocks(blocks)) {
      res <- res[names(blocks), ]
    }
  }

  res
}

#' @param icon_svg Character string containing the SVG icon markup
#' @param color Hex color code for the background
#' @param size Numeric size in pixels (default: 48)
#' @param mode Switch between URI and inline HTML mode
#' @rdname meta
#' @export
blk_icon_data_uri <- function(icon_svg, color, size = 48,
                              mode = c("uri", "inline")) {

  mode <- match.arg(mode)

  stopifnot(is_string(icon_svg), is_string(color), is.numeric(size))

  # Get icon style preference (light or solid)
  icon_style <- blockr_option("icon_style", "light")

  # Extract the path/content from the icon SVG
  # Icon SVG is typically: <svg ...><path d="..."/></svg>
  # We want just the inner content
  icon_content <- sub("^<svg[^>]*>", "", icon_svg)
  icon_content <- sub("</svg>$", "", icon_content)

  # Create outer SVG with colored rounded rectangle and icon
  icon_size <- size * 0.6  # Icon takes 60% of total size
  icon_offset <- size * 0.2  # Center the icon
  corner_radius <- size * 0.15  # 15% corner radius

  # Determine icon fill and background opacity based on style
  if (icon_style == "light") {
    icon_fill <- color
    bg_opacity <- 0.3
  } else {
    icon_fill <- "white"
    bg_opacity <- 1.0
  }

  # Convert hex color to rgba with opacity
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

  # Convert to base64 data URI
  if (mode == "inline") {
    return(HTML(svg))
  }

  sprintf(
    "data:image/svg+xml;base64,%s",
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
