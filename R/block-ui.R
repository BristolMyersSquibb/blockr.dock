#' @export
block_ui.dock_board <- function(id, x, blocks = NULL, ...) {

  block_panel <- function(x, id, ns) {

    blk_id <- ns(paste0("block_", id))
    blk_info <- get_block_metadata(x)

    card_tag <- div(
      class = "card",
      width = "100%",
      id = ns(as_block_handle_id(id)),
      div(
        class = c("row", "g-0", "px-4"),
        div(
          class = c(
            "col-sm-2",
            "col-md-1",
            "col-lg-1",
            "d-flex",
            "align-items-center",
            "justify-content-start"
          ),
          blk_icon(blk_info$category, class = "fa-3x")
        ),
        div(
          class = c("col-sm-10", "col-md-11", "col-lg-11"),
          div(
            class = "card-body",
            div(
              class = c(
                "d-flex",
                "align-items-center",
                "justify-content-start",
                "card-title gap-2"
              ),
              bslib::tooltip(
                icon("info-circle"),
                p(icon("lightbulb"), "How to use this block?"),
                p(blk_info$description, ".")
              )
            ),
            div(
              class = c("card-subtitle", "mb-2", "text-body-secondary"),
              span(
                class = c("badge", "bg-secondary"),
                "Type:",
                blk_info$category
              ),
              span(
                class = c("badge", "bg-secondary"),
                "Package:",
                blk_info$package
              )
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

  res <- map(
    block_panel,
    blocks,
    names(blocks),
    MoreArgs = list(ns = NS(id))
  )

  if (length(blocks) == 1L) {
    return(res[[1L]])
  }

  res
}

show_hide_block_dep <- function() {
  htmltools::htmlDependency(
    "show-hide-block",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "show-hide-block.js"
  )
}

#' @export
remove_block_ui.dock_board <- function(id, x, blocks = NULL, ...,
                                       session = get_session()) {

  if (is.null(blocks)) {
    blocks <- board_block_ids(x)
  }

  if (is_blocks(blocks)) {
    blocks <- names(blocks)
  }

  stopifnot(is.character(blocks), all(blocks %in% board_block_ids(x)))

  for (blk in blocks) {
    if (as_block_panel_id(blk) %in% block_panel_ids(session)) {
      hide_block_panel(blk)
    }

    removeUI(
      paste0("#", id, "-", blk),
      immediate = TRUE,
      session = session
    )
  }

  invisible(x)
}

#' @export
insert_block_ui.dock_board <- function(id, x, blocks = NULL, ...,
                                       session = get_session()) {

  if (is.null(blocks)) {
    blocks <- board_blocks(x)
  }

  if (is.character(blocks)) {
    blocks <- board_blocks(x)[blocks]
  }

  for (i in names(blocks)) {
    insertUI(
      paste0("#", id, "-blocks_offcanvas"),
      "beforeEnd",
      block_ui(id, x, blocks[i], ...),
      immediate = TRUE,
      session = session
    )

    show_block_panel(i, session = session)
  }

  invisible(x)
}

show_block_panel <- function(proxy, id, add_panel = TRUE) {

  stopifnot(is_string(id), is_bool(add_panel))

  if (add_panel) {
    add_block_panel(proxy, id)
  }

  show_block_ui(id, proxy$session)

  invisible(NULL)
}

hide_block_panel <- function(proxy, id, rm_panel = TRUE) {

  stopifnot(is_string(id), is_bool(rm_panel))

  hide_block_ui(id, proxy$session)

  if (rm_panel) {
    remove_block_panel(proxy, id)
  }

  invisible(NULL)
}

hide_block_ui <- function(id, session) {

  ns <- session$ns
  id <- as_block_handle_id(id)

  log_debug("hiding block {ns(id)}")

  from <- paste0("#", paste(dock_id(ns), id, sep = "-"), " .card")
  to <- paste0("#", ns("offcanvas"), " .offcanvas-body")

  move_dom_element(from, to, session)
}

show_block_ui <- function(id, session) {

  ns <- session$ns

  bid <- ns(as_block_handle_id(id))
  pid <- paste(dock_id(ns), as_block_panel_id(id), sep = "-")

  log_debug("showing block {bid} in panel {pid}")

  move_dom_element(paste0("#", bid), paste0("#", pid), session)
}
