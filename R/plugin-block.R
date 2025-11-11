#' @keywords internal
edit_block_ui <- function(id, blk, blk_id, expr_ui, block_ui) {

  blk_info <- blks_metadata(blk)
  ns <- NS(id)

  div(
    class = "card-body",
    div(
      class = "d-flex align-items-stretch gap-3",
      # Icon element
      blk_icon_data_uri(blk_info$icon, blk_info$color, 60, "inline"),
      # Title section
      div(
        class = paste(
          "d-flex flex-column justify-content-center",
          "flex-grow-1 min-height-0"
        ),
        div(
          class = "d-flex align-items-center justify-content-between w-100",
          # Left side: block name and subtitle
          block_card_title(blk, id, blk_info),
          # Right side: toggles and dropdown
          div(
            class = "d-flex align-items-center gap-2 flex-shrink-0",
            block_card_toggles(blk, ns),
            block_card_dropdown(ns, blk_info, blk_id)
          )
        )
      )
    ),
    block_card_content(ns, expr_ui, block_ui)
  )
}

block_card_title <- function(block, id, info) {
  div(
    class = "flex-grow-1 pe-3",
    div(
      class = "card-title mb-1",
      style = "line-height: 1.3;",
      popover(
        uiOutput(NS(id, "block_name_out"), inline = TRUE),
        title = "Provide a new title",
        textInput(
          NS(id, "block_name_in"),
          "Block name",
          updateOn = "blur"
        )
      )
    ),
    div(
      class = "text-body-secondary small text-muted",
      style = "line-height: 1.2;",
      span(class(block)[1]),
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
  )
}

block_card_toggles <- function(blk, ns) {

  opts <- c("inputs", "outputs")

  section_toggles <- shinyWidgets::checkboxGroupButtons(
    inputId = ns("collapse_blk_sections"),
    status = "light",
    size = "sm",
    choices = set_names(opts, paste0("<small>", opts, "</small>")),
    individual = TRUE,
    selected = coal(attr(blk, "visible"), opts)
  )

  # Remove the ms-auto class
  section_toggles$attribs$class <- trimws(
    gsub("form-group|ms-auto", "", section_toggles$attribs$class)
  )

  section_toggles
}

block_card_dropdown <- function(ns, info, blk_id) {
  # Create a custom dropdown without the default
  # button styling (no bg on hover, ...)
  div(
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
      class = paste(
        "dropdown-menu dropdown-menu-end",
        "shadow-sm rounded-3 border-1"
      ),
      style = "min-width: 250px;",
      # Block details header
      tags$li(
        h6(
          class = paste(
            "dropdown-header text-uppercase",
            "fw-semibold small text-secondary"
          ),
          style = "font-size: 0.75rem; letter-spacing: 0.5px;",
          "Block Details"
        )
      ),
      # Block details content
      tags$li(
        div(
          class = "px-3 py-1",
          # Package
          div(
            class = "d-flex justify-content-between align-items-center mb-2",
            span("Package", class = "text-muted small"),
            span(info$package, class = "small fw-medium")
          ),
          # Type
          div(
            class = "d-flex justify-content-between align-items-center mb-2",
            span("Type", class = "text-muted small"),
            span(info$category, class = "small fw-medium")
          ),
          # ID
          div(
            class = "d-flex justify-content-between align-items-center mb-0",
            span("ID", class = "text-muted small"),
            span(blk_id, class = "small fw-medium font-monospace")
          )
        )
      )
    )
  )
}

block_card_content <- function(ns, expr_ui, block_ui) {

  inputs_panel <- htmltools::tagQuery(
    accordion_panel(
      icon = icon("sliders"),
      title = "Block inputs",
      value = "inputs"
    )
  )$find(
    ".accordion-header"
  )$addAttrs(
    style = "display: none;"
  )$reset(
  )$find(
    ".accordion-body"
  )$append(
    expr_ui
  )$allTags()

  inputs_panel$attribs$style <- "border: none; border-radius: 0;"

  outputs_panel <- htmltools::tagQuery(
    accordion_panel(
      icon = icon("chart-simple"),
      title = "Block output(s)",
      value = "outputs",
      style = "max-width: 100%; overflow-x: auto;"
    )
  )$find(
    ".accordion-header"
  )$addAttrs(
    style = "display: none;"
  )$reset(
  )$find(
    ".accordion-body"
  )$append(
    tagList(block_ui, div(id = ns("outputs_issues_wrapper")))
  )$allTags()

  outputs_panel$attribs$style <- "border: none; border-radius: 0;"

  tagList(
    div(id = ns("errors_block"), class = "mt-4"),
    accordion(
      id = ns("blk_accordion"),
      multiple = TRUE,
      open = c("inputs", "outputs"),
      inputs_panel,
      outputs_panel
    )
  )
}

#' @keywords internal
edit_block_server <- function(id, block_id, board, update, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      initial_block <- isolate(
        board_blocks(board$board)[[block_id]]
      )

      cur_name <- reactive({
        req(board_blocks(board$board)[[block_id]])
        block_name(board_blocks(board$board)[[block_id]])
      })

      output$block_name_out <- renderUI({
        h3(
          input$block_name_in,
          tags$sup(icon("pencil-square", class = "fa-2xs"))
        )
      })

      observeEvent(
        cur_name(),
        updateTextInput(
          session,
          "block_name_in",
          "Block name",
          cur_name()
        )
      )

      observeEvent(
        input$block_name_in,
        {
          req(input$block_name_in)
          if (!identical(cur_name(), input$block_name_in)) {
            new_val <- board_blocks(board$board)[[block_id]]
            block_name(new_val) <- input$block_name_in
            new_val <- as_blocks(set_names(list(new_val), block_id))
            update(list(blocks = list(mod = new_val)))
          }
        }
      )

      output$block_summary <- renderText(
        block_summary(
          initial_block,
          reval_if(board$blocks[[block_id]]$server$result)
        )
      )

      observeEvent(
        input$collapse_blk_sections,
        accordion_panel_set(
          "blk_accordion",
          input$collapse_blk_sections,
          session
        )
      )

      conds <- reactive(
        {
          req(block_id %in% names(board$blocks))
          lapply(
            set_names(nm = c("error", "warning", "message")),
            function(cnd, cnds) coal(unlst(lst_xtr(cnds, cnd)), list()),
            reactiveValuesToList(board$blocks[[block_id]]$server$cond)
          )
        }
      )

      update_blk_cond_observer(conds, session)

      list(
        visible = reactive(input$collapse_blk_sections)
      )
    }
  )
}

update_blk_cond_observer <- function(conds, session = get_session()) {

  ns <- session$ns

  observeEvent(
    conds(),
    {
      cnds <- conds()

      statuses <- drop_nulls(
        lapply(
          c("warning", "message"),
          function(status) {

            cl <- switch(
              status,
              warning = "warning",
              message = "light"
            )

            msgs <- NULL

            if (length(cnds[[status]])) {
              msgs <- tags$div(
                class = sprintf("alert alert-%s", cl),
                HTML(
                  cli::ansi_html(
                    paste(
                      unlist(cnds[[status]]),
                      collapse = "\n"
                    )
                  )
                )
              )
            }

            msgs
          }
        )
      )

      removeUI(
        paste0("#", ns("outputs_issues"))
      )

      if (length(statuses)) {
        insertUI(
          selector = paste0("#", ns("outputs_issues_wrapper")),
          ui = create_issues_ui(statuses, ns)
        )
      }

      msgs <- NULL

      removeUI(
        paste0("#", ns("errors_block"), " .alert")
      )

      if (length(cnds[["error"]])) {
        msgs <- tags$div(
          class = sprintf("alert alert-danger"),
          HTML(
            cli::ansi_html(
              paste(
                unlist(cnds[["error"]]),
                collapse = "\n"
              )
            )
          )
        )

        insertUI(
          paste0("#", ns("errors_block")),
          ui = msgs
        )
      }
    }
  )
}

create_issues_ui <- function(statuses, ns) {

  collapse_id <- ns("outputs_issues_collapse")

  div(
    id = ns("outputs_issues"),
    tags$button(
      class = paste(
        "btn btn-sm btn-outline-secondary",
        "mt-2 mb-2 position-relative"
      ),
      type = "button",
      `data-bs-toggle` = "collapse",
      `data-bs-target` = paste0("#", collapse_id),
      `aria-expanded` = "false",
      `aria-controls` = collapse_id,
      "View issues",
      span(
        class = paste(
          "position-absolute top-0 start-100",
          "translate-middle badge rounded-pill bg-danger"
        ),
        length(statuses)
      )
    ),
    collapse_container(
      id = collapse_id,
      statuses
    )
  )
}

edit_block_validator <- function(x) {

  stopifnot(
    is.list(x),
    setequal(names(x), "visible"),
    is.reactive(x[["visible"]])
  )

  invisible(x)
}
