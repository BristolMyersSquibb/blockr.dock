#' @export
block_ui.dock_board <- function(id, x, blocks = NULL, edit_ui = NULL, ...) {
  block_panel <- function(x, id, edit_ui, ns, board) {
    blk_id <- ns(paste0("block_", id))
    blk_info <- blks_metadata(x)

    # Edit plugin
    if (!is.null(edit_ui)) {
      edit_ui <- edit_ui$ui(x, NS(blk_id, "edit_block"))
    }

    # if yellow color, use black text, otherwise white for contrasts
    icon_color <- if (blk_info$color == "#F0E442") "text-dark" else "text-white"

    card_tag <- tags$div(
      class = "card",
      width = "100%",
      id = ns(as_block_handle_id(id)),
      tags$div(
        class = "card-body",
        div(
          class = "d-flex align-items-stretch gap-3",
          # Icon element
          div(
            class = paste(
              "d-flex align-items-center justify-content-center",
              "flex-shrink-0 rounded-3 shadow-sm"
            ),
            style = sprintf(
              paste0(
                "background: %s;",
                "border: 1px solid rgba(255, 255, 255, 0.2);",
                "width: 60px;",
                "min-height: 100%%;",
                "position: relative;"
              ),
              blk_info$color
            ),
            div(
              class = icon_color,
              style = "filter: drop-shadow(0 1px 2px rgba(0, 0, 0, 0.3));",
              HTML(blk_info$icon)
            )
          ),
          # Title section
          div(
            class = "d-flex flex-column justify-content-center flex-grow-1 min-height-0",
            block_card_title(board, x, id, blk_info, edit_ui, ns)
          )
        ),
        #edit_ui$block_summary,
        block_card_content(x, id, blk_id, ns)
      )
    )
    tagAppendAttributes(card_tag, class = "border border-0 shadow-none")
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
    MoreArgs = list(ns = NS(id), edit_ui = edit_ui, board = x)
  )

  if (length(blocks) == 1L) {
    return(res[[1L]])
  }

  res
}

#' @keywords internal
block_card_title <- function(board, block, id, info, edit_ui, ns) {
  tags$div(
    class = "d-flex align-items-center justify-content-between w-100",
    # Left side: block name and subtitle
    div(
      class = "flex-grow-1 pe-3",
      div(
        class = "card-title mb-1",
        style = "line-height: 1.3;",
        edit_ui$block_name
      ),
      block_card_subtitle(board, block, id, info)
    ),
    # Right side: toggles and dropdown
    div(
      class = "d-flex align-items-center gap-2 flex-shrink-0",
      block_card_toggles(id, ns),
      block_card_dropdown(id, info, ns)
    )
  )
}

#' Block subtitle id method
#'
#' @param x Board.
#' @param id Block id.
#'
#' @export
block_subtitle_id <- function(x, id) {
  UseMethod("block_subtitle_id", x)
}

#' @export
block_subtitle_id.dock_board <- function(x, id) {
  NULL
}

#' @export
block_subtitle_id.md_board <- function(x, id) {
  tagList(
    " | ",
    span("id:", id)
  )
}

#' @keywords internal
block_card_subtitle <- function(board, block, id, info) {
  div(
    class = "text-body-secondary small text-muted",
    style = "line-height: 1.2;",
    span(class(block)[1]),
    block_subtitle_id(board, id),
    tags$sup(
      class = "ms-1",
      tooltip(
        icon("info-circle", style = "color: #9ca3af; font-size: 0.75em;"),
        p(
          icon("lightbulb"),
          "How to use this block?",
        ),
        p(info$description, ".")
      )
    )
  )
}

#' @keywords internal
block_card_content <- function(block, id, blk_id, ns) {
  # Create accordion panels and hide their headers directly
  inputs_panel <- htmltools::tagQuery(
    accordion_panel(
      icon = icon("sliders"),
      title = "Block inputs",
      value = "inputs"
    )
  )$find(".accordion-header")$addAttrs(style = "display: none;")$reset()$find(
    ".accordion-body"
  )$append(expr_ui(blk_id, block))$allTags()

  inputs_panel$attribs$style <- "border: none; border-radius: 0;"

  outputs_panel <- htmltools::tagQuery(
    accordion_panel(
      icon = icon("chart-simple"),
      title = "Block output(s)",
      value = "outputs",
      style = "max-width: 100%; overflow-x: auto;"
    )
  )$find(".accordion-header")$addAttrs(style = "display: none;")$reset()$find(
    ".accordion-body"
  )$append(tagList(
    block_ui(blk_id, block),
    div(id = ns(paste0("outputs-issues-wrapper-", id)))
  ))$allTags()

  outputs_panel$attribs$style <- "border: none; border-radius: 0;"

  accordions <- accordion(
    id = ns(paste0("accordion-", id)),
    multiple = TRUE,
    open = c("inputs", "outputs"),
    inputs_panel,
    outputs_panel
  )

  tagList(
    div(id = ns(sprintf("errors-block-%s", id)), class = "mt-4"),
    accordions
  )
}

#' @keywords internal
block_card_toggles <- function(id, ns) {
  section_toggles <- shinyWidgets::checkboxGroupButtons(
    inputId = ns(sprintf("collapse-blk-section-%s", id)),
    status = "light",
    size = "sm",
    choices = set_names(
      c("inputs", "outputs"),
      c(
        "<small>inputs</small>",
        "<small>outputs</small>"
      )
    ),
    individual = TRUE,
    selected = c("inputs", "outputs")
  )

  # Remove the ms-auto class
  section_toggles$attribs$class <- trimws(gsub(
    "form-group|ms-auto",
    "",
    section_toggles$attribs$class
  ))

  section_toggles
}

#' @keywords internal
block_card_dropdown <- function(id, info, ns) {
  # Create a custom dropdown without the default button styling (no bg on hover, ...)
  tags$div(
    class = "dropdown",
    tags$button(
      class = "btn btn-link p-1 border-0 bg-transparent text-muted",
      type = "button",
      `data-bs-toggle` = "dropdown",
      `aria-expanded` = "false",
      onmouseover = "this.classList.add('text-dark');",
      onmouseout = "this.classList.remove('text-dark');",
      icon("ellipsis-vertical")
    ),
    tags$ul(
      class = "dropdown-menu dropdown-menu-end shadow-sm rounded-3 border-1",
      style = "min-width: 250px;",
      # Actions header
      tags$li(
        tags$h6(
          class = "dropdown-header text-uppercase fw-semibold small text-secondary",
          style = "font-size: 0.75rem; letter-spacing: 0.5px;",
          "Block Actions"
        )
      ),
      # Block actions
      tags$li(
        tags$button(
          class = "dropdown-item action-button py-2 position-relative text-center",
          type = "button",
          id = ns(sprintf("append-%s", id)),
          style = "padding-left: 2.5rem;",
          tags$span(
            class = "position-absolute start-0 top-50 translate-middle-y ms-3",
            icon("plus", class = "text-success")
          ),
          "Append block"
        )
      ),
      tags$li(
        tags$button(
          class = "dropdown-item action-button py-2 position-relative text-center text-danger",
          type = "button",
          id = ns(sprintf("delete-%s", id)),
          style = "padding-left: 2.5rem;",
          tags$span(
            class = "position-absolute start-0 top-50 translate-middle-y ms-3",
            icon("trash")
          ),
          "Delete block"
        )
      ),
      tags$li(tags$hr(class = "dropdown-divider my-2")),
      # Block details header
      tags$li(
        tags$h6(
          class = "dropdown-header text-uppercase fw-semibold small text-secondary",
          style = "font-size: 0.75rem; letter-spacing: 0.5px;",
          "Block Details"
        )
      ),
      # Block details content
      tags$li(
        tags$div(
          class = "px-3 py-1",
          # Package
          tags$div(
            class = "d-flex justify-content-between align-items-center mb-2",
            tags$span("Package", class = "text-muted small"),
            tags$span(info$package, class = "small fw-medium")
          ),
          # Type
          tags$div(
            class = "d-flex justify-content-between align-items-center mb-2",
            tags$span("Type", class = "text-muted small"),
            tags$span(info$category, class = "small fw-medium")
          ),
          # ID
          tags$div(
            class = "d-flex justify-content-between align-items-center mb-0",
            tags$span("ID", class = "text-muted small"),
            tags$span(id, class = "small fw-medium font-monospace")
          )
        )
      )
    )
  )
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
remove_block_ui.dock_board <- function(
  id,
  x,
  blocks = NULL,
  ...,
  session = get_session()
) {
  if (is.null(blocks)) {
    blocks <- board_block_ids(x)
  }

  if (is_blocks(blocks)) {
    blocks <- names(blocks)
  }

  stopifnot(is.character(blocks), all(blocks %in% board_block_ids(x)))

  proxy <- dock_proxy(session)

  for (blk in blocks) {
    if (as_block_panel_id(blk) %in% block_panel_ids(proxy)) {
      hide_block_panel(blk, proxy = proxy)
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
insert_block_ui.dock_board <- function(
  id,
  x,
  blocks = NULL,
  dock,
  ...,
  session = get_session()
) {
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

    show_block_panel(i, determine_panel_pos(dock), dock$proxy)
  }

  invisible(x)
}

show_block_panel <- function(id, add_panel = TRUE, proxy = dock_proxy()) {
  if (isTRUE(add_panel)) {
    add_block_panel(id, proxy = proxy)
  } else if (isFALSE(add_panel)) {
    select_block_panel(id, proxy)
  } else {
    add_block_panel(id, position = add_panel, proxy = proxy)
  }

  show_block_ui(id, proxy$session)

  invisible(NULL)
}

hide_block_panel <- function(id, rm_panel = TRUE, proxy = dock_proxy()) {
  hide_block_ui(id, proxy$session)

  if (isTRUE(rm_panel)) {
    remove_block_panel(id, proxy)
  }

  invisible(NULL)
}

hide_block_ui <- function(id, session) {
  ns <- session$ns

  bid <- ns(as_block_handle_id(id))
  oid <- paste0(ns("blocks_offcanvas"), " .offcanvas-body")

  log_debug("hiding block {bid} in {oid}")

  move_dom_element(paste0("#", bid), paste0("#", oid), session)
}

show_block_ui <- function(id, session) {
  ns <- session$ns

  bid <- ns(as_block_handle_id(id))
  pid <- paste(dock_id(ns), as_block_panel_id(id), sep = "-")

  log_debug("showing block {bid} in panel {pid}")

  move_dom_element(paste0("#", bid), paste0("#", pid), session)
}
