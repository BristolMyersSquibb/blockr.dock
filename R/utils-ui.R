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

  if (is_string(ctor)) {
    blk <- sub("^new_", "", ctor)
    blks <- available_blocks()

    if (blk %in% names(blks)) {
      info <- blks[[blk]]

      res <- list(
        category = attr(info, "category"),
        name = attr(info, "name"),
        description = attr(info, "description"),
        package = attr(info, "package")
      )

      return(res)
    }
  }

  list(
    category = "Uncategorized",
    name = block_name(x),
    description = "No description available",
    package = "local"
  )
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
#' @param category Block category
#' @export
blk_color <- function(category) {
  # Palette is taken from:
  # https://siegal.bio.nyu.edu/color-palette/
  # very nice palette that is color-blind friendly.
  switch(
    category,
    data = "#0072B2",
    transform = "#56B4E9",
    plot = "#E69F00",
    file = "#CC79A7",
    parse = "#009E73",
    table = "#F0E442",
    text = "#D55E00",
    "#6c757d"
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
