off_canvas <- function(
  id,
  title,
  ...,
  width = "w-25",
  position = c("start", "top", "bottom", "end")
) {
  label <- paste0(id, "-title")

  div(
    class = glue("offcanvas offcanvas-{match.arg(position)} {width}"),
    tabindex = "-1",
    id = id,
    `aria-labelledby` = label,
    `data-bs-scroll` = "true",
    div(
      class = "offcanvas-header",
      h5(class = "offcanvas-title", id = label, title),
      tags$button(
        type = "button",
        class = "btn-close",
        `data-bs-dismiss` = "offcanvas",
        `aria-label` = "Close"
      )
    ),
    div(
      class = "offcanvas-body",
      ...
    )
  )
}

#' @keywords internal
collapse_container <- function(id, ...) {
  tags$div(class = "collapse", id = id, ...)
}

get_block_metadata <- function(x) {
  stopifnot(is_block(x))

  ctor <- attr(x, "ctor")
  ctor_name <- attr(ctor, "fun")

  if (is_string(ctor_name)) {
    blk <- sub("^new_", "", ctor_name)
    blks <- available_blocks()

    if (blk %in% names(blks)) {
      info <- blks[[blk]]

      res <- list(
        category = attr(info, "category"),
        name = attr(info, "name"),
        description = attr(info, "description"),
        package = attr(info, "package"),
        icon = attr(info, "icon")
      )

      return(res)
    }
  }

  list(
    category = "Uncategorized",
    name = block_name(x),
    description = "No description available",
    package = "local",
    icon = "question-square"
  )
}

#' Remap old category names to new category names
#'
#' Provides backward compatibility by mapping old category names to the new
#' standardized category system.
#'
#' @param category Character string with category name (old or new)
#' @return Character string with new category name
#' @keywords internal
remap_category <- function(category) {
  if (!length(category) || is.na(category)) {
    return("uncategorized")
  }

  # Old → New category mapping
  mapping <- c(
    # Old categories
    data = "input",
    file = "input",
    parse = "input",
    text = "utility",
    aiml = "model",
    timeseries = "structured",
    # New categories (pass through)
    input = "input",
    transform = "transform",
    structured = "structured",
    plot = "plot",
    table = "table",
    model = "model",
    output = "output",
    utility = "utility",
    uncategorized = "uncategorized"
  )

  # Get mapped category or return uncategorized if not found
  mapped <- mapping[category]
  if (is.na(mapped)) {
    return("uncategorized")
  }

  as.character(mapped)
}

blk_icon <- function(category, class = NULL) {
  stopifnot(is_string(category))

  icon(
    switch(
      category,
      data = "table",
      file = "file",
      parse = "gear",
      plot = "chart-line",
      transform = "wand-magic",
      table = "table",
      "reddit-alien"
    ),
    class
  )
}

#' Get block color based on category
#'
#' Returns the color hex code for a block category using the Okabe-Ito
#' colorblind-friendly palette.
#'
#' @param category Block category (old or new format)
#' @return Character string with hex color code
#' @export
blk_color <- function(category) {
  # Remap old categories to new ones
  category <- remap_category(category)

  # Okabe-Ito colorblind-friendly palette
  # See: https://jfly.uni-koeln.de/color/
  switch(
    category,
    input = "#0072B2",      # Blue
    transform = "#009E73",  # Bluish green
    structured = "#56B4E9", # Sky blue
    plot = "#E69F00",       # Orange
    table = "#CC79A7",      # Reddish purple/pink
    model = "#F0E442",      # Yellow (includes AI/ML)
    output = "#D55E00",     # Vermilion
    utility = "#CCCCCC",    # Light gray
    "#999999"               # Medium gray (uncategorized)
  )
}

move_dom_element <- function(from, to, session = get_session()) {
  session$sendCustomMessage(
    "move-element",
    list(
      from = from,
      to = to
    )
  )
}

drop_nulls <- function(x) {
  x[!lgl_ply(x, is.null)]
}
