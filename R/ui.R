#' @export
board_ui.dock_board <- function(id, x, ...) {
  tagList(
    show_hide_block_dep(),
    off_canvas(
      id = NS(id, "blocks_offcanvas"),
      title = "Offcanvas blocks",
      block_ui(id, x)
    ),
    options_ui(
      id,
      as_board_options(x),
      board_ui(id, board_plugins(x, which = "preserve_board"), x)
    ),
    dockViewR::dock_view_output(
      NS(id, dock_id()),
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
                p(icon("lightbulb"), "How to use this block?"),
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

show_block_panel <- function(id, add_panel = TRUE, session = get_session()) {

  stopifnot(is_string(id), is_bool(add_panel))

  ns <- session$ns

  if (add_panel) {
    add_block_panel(id, session)
  }

  bid <- ns(id)
  pid <- block_panel_id(id, dock_id(ns))

  log_debug("showing block {bid} in panel {pid}")

  session$sendCustomMessage(
    "show-block",
    list(
      block_id = paste0("#", bid),
      panel_id = paste0("#", pid)
    )
  )

  invisible(NULL)
}

hide_block_panel <- function(id, rm_panel = TRUE, session = get_session()) {

  stopifnot(is_string(id), is_bool(rm_panel))

  ns <- session$ns

  pid <- block_panel_id(id, dock_id(ns))

  log_debug("hiding block panel {pid}")

  session$sendCustomMessage(
    "hide-block",
    list(
      offcanvas = paste0("#", ns("offcanvas")),
      block_id = paste0("#", pid)
    )
  )

  if (rm_panel) {
    remove_block_panel(id, session)
  }

  invisible(NULL)
}

remove_block_panel <- function(id, session = get_session()) {
  did <- dock_id()
  pid <- block_panel_id(id)
  log_debug("removing block panel {pid} from dock {did}")
  dockViewR::remove_panel(did, pid, session = session)
}

add_block_panel <- function(id, session = get_session()) {

  did <- dock_id()
  pan <- block_panel(id)

  log_debug("adding block {id} to dock {did}")

  dockViewR::add_panel(did, panel = pan, session = session)

  invisible(NULL)
}

block_panel <- function(id) {

  pid <- block_panel_id(id)

  log_debug("creating block panel {pid}")

  dockViewR::panel(
    id = pid,
    title = paste("Block:", id),
    content = tagList(),
    renderer = "always",
    style = list(
      overflow = "auto",
      height = "100%"
    ),
    remove = list(enable = TRUE, mode = "manual")
  )
}

list_block_panels <- function(session = get_session()) {
  res <- dockViewR::get_panels_ids(dock_id(), session)
  res[is_block_panel_id(res)]
}

#' @export
update_ui.dock_board <- function(x, ..., session = get_session()) {
  restore_dock(board_layout(x), session)
  invisible(x)
}

#' @export
remove_block_ui.dock_board <- function(id, x, blocks = NULL, ...,
                                       session = get_session()) {

  if (is.null(blocks)) {
    blocks <- board_block_ids(x)
  }

  stopifnot(is.character(blocks), all(blocks %in% board_block_ids(x)))

  for (blk in blocks) {

    if (block_panel_id(blk) %in% list_block_panels()) {
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
    blocks <- board_block_ids(x)
  }

  stopifnot(is.character(blocks), all(blocks %in% board_block_ids(x)))

  for (blk in blocks) {
    insertUI(
      paste0("#", id, "-blocks_offcanvas"),
      "beforeEnd",
      block_ui(id, x, blk, ...),
      immediate = TRUE,
      session = session
    )

    if (blk %in% layout_panel_block_ids(x)) {
      show_block_panel(blk, session = session)
    }
  }

  invisible(x)
}
