#' @export
board_ui.dock_board <- function(id, x, ...) {
  tagList(
    off_canvas(
      id = NS(id, "blocks_offcanvas"),
      title = "Offcanvas blocks"
    ),
    options_ui(
      id,
      as_board_options(x),
      board_ui(id, board_plugins(x, which = "preserve_board"), x)
    ),
    dockViewR::dock_view_output(
      NS(id, "dock"),
      width = "100%",
      height = "100vh"
    )
  )
}

options_ui <- function(id, x, ...) {

  opts <- split(x, chr_ply(x, attr, "category"))

  offcanvas_id <- NS(id, "options_offcanvas")

  tagList(
    blockr_fab_dep(),
    tags$button(
      class = "blockr-fab",
      icon("gear"),
      `data-bs-toggle` = "offcanvas",
      `data-bs-target` = paste0("#", offcanvas_id),
      `aria-controls` = offcanvas_id
    ),
    off_canvas(
      id = offcanvas_id,
      position = "end",
      title = "Board options",
      ...,
      hr(),
      do.call(
        bslib::accordion,
        c(
          list(
            id = NS(id, "board_options"),
            multiple = TRUE,
            open = FALSE,
            class = "accordion-flush"
          ),
          map(
            do.call,
            rep(list(bslib::accordion_panel), length(opts)),
            map(
              list,
              title = names(opts),
              lapply(opts, lapply, board_option_ui, id)
            )
          )
        )
      )
    )
  )
}

blockr_fab_dep <- function() {
  htmltools::htmlDependency(
    "blockr-fab",
    pkg_version(),
    src = pkg_file("assets", "css"),
    stylesheet = "blockr-fab.css"
  )
}

#' @export
block_ui.dock_board <- function(id, x, blocks = NULL, ...) {

  block_panel <- function(x, id, ns) {

    blk_id <- ns(paste0("block_", id))
    blk_info <- get_block_metadata(x)

    card_tag <- div(
      class = "card",
      width = "100%",
      id = ns(id),
      div(
        class = "row g-0 px-4",
        div(
          class = paste("col-sm-2", "col-md-1", "col-lg-1", "d-flex",
                        "align-items-center", "justify-content-start"),
          blk_icon(blk_info$category, class = "fa-3x")
        ),
        div(
          class = "col-sm-10 col-md-11 col-lg-11",
          div(
            class = "card-body",
            div(
              class = paste("d-flex", "align-items-center",
                            "justify-content-start", "card-title gap-2"),
              bslib::tooltip(
                icon("info-circle"),
                p(icon("lightbulb"), "How to use this block?",),
                p(blk_info$description, ".")
              )
            ),
            div(
              class = "card-subtitle mb-2 text-body-secondary",
              span(class = "badge bg-secondary", "Type:", blk_info$category),
              span(class = "badge bg-secondary", "Package:", blk_info$package)
            )
          )
        )
      ),
      bslib::accordion(
        id = ns(paste0("accordion-", id)),
        multiple = TRUE,
        class = "accordion-flush",
        open = c("inputs", "outputs", "state"),
        bslib::accordion_panel(
          icon = icon("sliders"),
          title = "Block inputs",
          value = "inputs",
          expr_ui(blk_id, x)
        ),
        bslib::accordion_panel(
          icon = icon("chart-simple"),
          title = "Block output(s)",
          value = "outputs",
          style = "max-width: 100%; overflow-x: auto;",
          block_ui(blk_id, x)
        )
      )
    )

    tagAppendAttributes(
      card_tag,
      class = "border border-0 shadow-none"
    )
  }

  stopifnot(is_string(id))

  if (is.null(blocks)) {
    blocks <- board_blocks(x)
  } else if (is.character(blocks)) {
    blocks <- board_blocks(x)[blocks]
  }

  stopifnot(is_blocks(blocks))

  map(
    block_panel,
    blocks,
    names(blocks),
    MoreArgs = list(ns = NS(id))
  )
}
