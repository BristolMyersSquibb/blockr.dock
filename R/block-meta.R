#' Get block metadata
#'
#' Returns various metadata for blocks or block categories, as well as styling
#' for block icons.
#'
#' - `blks_metadata()`: Retrieves metadata given a `block` or `blocks` object
#'   from the block registry. Can also handle blocks which are not
#'   registered and provides default values in that case.
#' - `blk_color()`: Produces colors using the Okabe-Ito colorblind-friendly
#'   palette for a character vector of block categories.
#' - `blk_icon_data_uri()`: Processes block icons to add color and turn them
#'   into square-shaped icons.
#'
#' @param blocks Blocks passed as `blocks` or `block` object
#'
#' @examples
#' blk <- blockr.core::new_dataset_block()
#' meta <- blks_metadata(blk)
#'
#' col <- blk_color(meta$category)
#' blk_icon_data_uri(meta$icon, col)
#'
#' @return Metadata is returned from `blks_metadata()` as a `data.frame` with
#' each row corresponding to a block. Both `blk_color()` and
#' `blk_icon_data_uri()` return character vectors.
#'
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
    reg <- cbind(reg, color = blk_color(reg$category))

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

#' @param category Block category
#' @rdname meta
#' @export
blk_color <- function(category) {
  chr_ply(
    category,
    switch,
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

  # Extract the path/content from the icon SVG
  # Icon SVG is typically: <svg ...><path d="..."/></svg>
  # We want just the inner content
  icon_content <- sub("^<svg[^>]*>", "", icon_svg)
  icon_content <- sub("</svg>$", "", icon_content)

  # Create outer SVG with colored rounded rectangle and white icon
  icon_size <- size * 0.6  # Icon takes 60% of total size
  icon_offset <- size * 0.2  # Center the icon
  corner_radius <- size * 0.15  # 15% corner radius

  svg <- sprintf(
    "<svg xmlns=\"http://www.w3.org/2000/svg\"
        width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\">
      <rect width=\"%d\" height=\"%d\" rx=\"%g\" ry=\"%g\" fill=\"%s\"/>
      <g transform=\"translate(%g, %g)\" fill=\"white\">
        <svg width=\"%g\" height=\"%g\" viewBox=\"0 0 16 16\">%s</svg>
      </g>
    </svg>",
    size, size, size, size,
    size, size, corner_radius, corner_radius, color,
    icon_offset, icon_offset,
    icon_size, icon_size, icon_content
  )

  # Convert to base64 data URI
  if (mode == "inline") {
    return(HTML(svg))
  }

  paste0(
    "data:image/svg+xml;base64,",
    jsonlite::base64_enc(charToRaw(svg))
  )
}
