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

collapse_container <- function(id, ...) {
  tags$div(class = "collapse", id = id, ...)
}

#' Toggle Buttons (Bootstrap-free replacement for shinyWidgets::checkboxGroupButtons)
#'
#' @param inputId The input slot for Shiny
#' @param choices Named vector of choices (names = labels, values = values)
#' @param selected Which values are initially selected
#' @param class Additional CSS classes
#'
#' @return A Shiny input tag
#' @noRd
toggle_buttons <- function(inputId, choices, selected = names(choices),
                           class = "blockr-section-toggle") {
  buttons <- mapply(
    function(value, label) {
      is_selected <- value %in% selected
      tags$label(
        class = paste("btn btn-light", if (is_selected) "active" else ""),
        tags$input(
          type = "checkbox",
          class = "btn-check",
          autocomplete = "off",
          value = value,
          checked = if (is_selected) NA else NULL
        ),
        label
      )
    },
    choices,
    names(choices),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )

  tags$div(
    id = inputId,
    class = paste("blockr-toggle-buttons", class),
    `data-input-type` = "toggleButtons",
    tags$div(
      class = "btn-group btn-group-toggle",
      role = "group",
      buttons
    )
  )
}

#' Color Input (Bootstrap-free replacement for shinyWidgets::colorPickr)
#'
#' @param inputId The input slot for Shiny
#' @param label Display label
#' @param selected Initial color value (hex)
#'
#' @return A Shiny input tag
#' @noRd
color_input <- function(inputId, label = NULL, selected = "#3498db") {
  tags$div(
    class = "blockr-color-input form-group",
    if (!is.null(label)) tags$label(`for` = inputId, label),
    tags$input(
      type = "color",
      id = inputId,
      class = "form-control form-control-color",
      value = selected
    )
  )
}

#' Custom Popover (Bootstrap-free replacement for bslib::popover)
#'
#' @param trigger The element that triggers the popover
#' @param ... Content to display in the popover
#' @param title Optional title for the popover
#' @param placement Where to place the popover (top, bottom, left, right)
#' @param options List of options (e.g., trigger = "hover" or "click")
#'
#' @return A Shiny tag
#' @noRd
popover <- function(trigger, ..., title = NULL, placement = "bottom",
                    options = list()) {
  content <- list(...)
  trigger_type <- options$trigger %||% "hover"

  # Generate unique ID for this popover
  popover_id <- paste0("popover-", paste0(sample(letters, 8), collapse = ""))

  tags$span(
    class = "blockr-popover-wrapper",
    `data-popover-trigger` = trigger_type,
    tags$span(
      class = "blockr-popover-trigger",
      trigger
    ),
    tags$div(
      id = popover_id,
      class = paste("blockr-popover-content", paste0("placement-", placement)),
      if (!is.null(title)) tags$div(class = "blockr-popover-title", title),
      tags$div(class = "blockr-popover-body", content)
    )
  )
}

#' Custom Accordion (Bootstrap-free replacement for bslib::accordion)
#'
#' @param ... Accordion panels created with accordion_panel()
#' @param id Accordion ID for Shiny binding
#' @param multiple Allow multiple panels open at once
#' @param open Character vector of panel values to open initially, or TRUE for all
#' @param class Additional CSS classes
#'
#' @return A Shiny tag
#' @noRd
accordion <- function(..., id = NULL, multiple = FALSE, open = NULL,
                      class = NULL) {
  panels <- list(...)

  # Determine which panels should be open
  open_values <- if (isTRUE(open)) {
    # All panels open
    vapply(panels, function(p) p$attribs$`data-value` %||% "", character(1))
  } else if (is.character(open)) {
    open
  } else {
    character(0)
  }

  # Add open state to panels
  panels <- lapply(panels, function(panel) {
    value <- panel$attribs$`data-value`
    is_open <- value %in% open_values

    # Update classes based on open state
    if (is_open) {
      panel$attribs$class <- paste(panel$attribs$class, "show")
      # Find and update button and collapse
      panel <- htmltools::tagQuery(panel)$
        find(".accordion-button")$
        removeClass("collapsed")$
        reset()$
        find(".accordion-collapse")$
        addClass("show")$
        allTags()
    }
    panel
  })

  tags$div(
    id = id,
    class = paste("accordion", class),
    `data-multiple` = if (multiple) "true" else "false",
    panels
  )
}

#' Custom Accordion Panel (Bootstrap-free replacement for bslib::accordion_panel)
#'
#' @param title Panel title
#' @param ... Panel content
#' @param value Unique value for this panel (defaults to title)
#' @param icon Optional icon
#' @param style Optional inline styles for the panel
#'
#' @return A Shiny tag
#' @noRd
accordion_panel <- function(title, ..., value = title, icon = NULL,
                            style = NULL) {
  collapse_id <- paste0("collapse-", gsub("[^a-zA-Z0-9]", "-", value))

  tags$div(
    class = "accordion-item",
    `data-value` = value,
    style = style,
    tags$h2(
      class = "accordion-header",
      tags$button(
        class = "accordion-button collapsed",
        type = "button",
        `data-bs-toggle` = "collapse",
        `data-bs-target` = paste0("#", collapse_id),
        `aria-expanded` = "false",
        `aria-controls` = collapse_id,
        if (!is.null(icon)) tags$span(class = "accordion-icon me-2", icon),
        title
      )
    ),
    tags$div(
      id = collapse_id,
      class = "accordion-collapse collapse",
      tags$div(
        class = "accordion-body",
        ...
      )
    )
  )
}

#' Programmatically set accordion panel state
#'
#' @param id Accordion ID
#' @param values Character vector of panel values to open
#' @param session Shiny session
#'
#' @noRd
accordion_panel_set <- function(id, values, session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage(
    "blockr-accordion-set",
    list(id = session$ns(id), values = as.list(values))
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

determine_active_views <- function(layout) {

  xtr_leaf_id <- function(x) {

    if (x$type == "leaf") {
      return(set_names(coal(x$data$activeView, ""), x$data$id))
    }

    lapply(x$data, xtr_leaf_id)
  }

  rapply(
    xtr_leaf_id(layout[["grid"]][["root"]]),
    identity,
    "character"
  )
}

visible_exts <- function() {
  blockr_option("visible_extensions", "dag_extension")
}

determine_panel_pos <- function(dock) {

  active <- determine_active_views(dock$layout())

  keep_visible <- as_ext_panel_id(visible_exts())

  cands <- names(active)[!active %in% keep_visible]

  if (!length(cands)) {
    return(list(direction = "right"))
  }

  prev <- dock$prev_active_group()

  if (isTRUE(prev %in% cands)) {
    grp <- prev
  } else {
    grp <- last(cands)
  }

  list(referenceGroup = grp, direction = "within")
}

#' UI utilities
#'
#' Exported utilities for manipulating dock panels (i.e. displaying panels).
#'
#' @param id Object ID
#' @param board Board object
#' @param dock Object available as `dock` in extensions
#' @param type Either "block" or "extensions", depending on what kind of panel
#'   should be shown
#'
#' @return `NULL`, invisibly
#'
#' @rdname panel
#' @export
show_panel <- function(id, board, dock, type = c("block", "extension")) {

  stopifnot(is_string(id))

  type <- match.arg(type)
  proxy <- dock$proxy

  if (identical(type, "block")) {
    stopifnot(id %in% board_block_ids(board))
  } else {
    stopifnot(id %in% dock_ext_ids(board))
  }

  panels <- dock_panel_ids(proxy)

  if (identical(type, "block")) {
    panels <- panels[lgl_ply(panels, is_block_panel_id)]
  } else {
    panels <- panels[lgl_ply(panels, is_ext_panel_id)]
  }

  panels <- as_obj_id(panels)

  if (id %in% panels) {
    if (identical(type, "block")) {
      select_block_panel(id, proxy)
    } else {
      select_ext_panel(id, proxy)
    }

    return(invisible())
  }

  pos <- determine_panel_pos(dock)

  if (identical(type, "block")) {
    blocks <- board_blocks(board)
    add_block_panel(blocks[id], position = pos, proxy = proxy)
    show_block_ui(id, proxy$session)
  } else {
    exts <- dock_extensions(board)

    add_ext_panel(exts[[id]], position = pos, proxy = proxy)
    show_ext_ui(id, proxy$session)
  }

  invisible()
}

drop_nulls <- function(x) {
  x[!lgl_ply(x, is.null)]
}
