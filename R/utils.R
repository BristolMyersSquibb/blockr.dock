off_canvas <- function(id, title, ..., width = "w-25",
                       position = c("start", "top", "bottom", "end")) {

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

dock_id <- function(ns = NULL) {

  res <- "dock"

  if (is.null(ns)) {
    return(res)
  }

  ns(res)
}

block_panel_id <- function(block_id, dock_id = NULL) {

  stopifnot(is.character(block_id), has_length(block_id))

  res <- paste0("block-", block_id)

  if (is.null(dock_id)) {
    return(res)
  }

  stopifnot(is_string(dock_id))

  paste0(dock_id, "-", res)
}
