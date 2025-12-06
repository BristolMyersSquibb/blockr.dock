edit_block_ui <- function(id, blk, blk_id, expr_ui, block_ui) {

  blk_info <- blks_metadata(blk)
  ns <- NS(id)

  div(
    class = "card-body",
    div(
      class = "d-flex align-items-stretch gap-3",
      # Icon element with tooltip
      span(
        title = blk_info$description,
        blk_icon_data_uri(blk_info$icon, blk_info$color, 46, "inline")
      ),
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
  ns <- NS(id)
  input_id <- ns("block_name_in")

  div(
    class = "flex-grow-1 pe-3",
    div(
      class = "card-title mb-0",
      style = "line-height: 1.0;",
      # Inline editable title container
      div(
        class = "blockr-inline-edit",
        # Display mode - click to edit
        div(
          id = ns("title_display"),
          class = "blockr-title-display d-inline-flex align-items-center gap-2",
          title = "Click to rename",
          style = paste(
            "padding: 4px 8px;",
            "margin: -4px -8px;",
            "border-radius: 4px;",
            "cursor: pointer;",
            "border: 2px dashed transparent;",
            "transition: border-color 0.15s ease;"
          ),
          onmouseover = paste0(
            "this.style.borderColor='#ddd';",
            "this.querySelector('.edit-icon').style.opacity='1';"
          ),
          onmouseout = paste0(
            "this.style.borderColor='transparent';",
            "this.querySelector('.edit-icon').style.opacity='0';"
          ),
          onclick = sprintf(
            paste0(
              "this.style.display='none';",
              "var editWrap = document.getElementById('%s');",
              "editWrap.style.display='block';",
              "var input = editWrap.querySelector('input');",
              "input.focus();",
              "input.select();"
            ),
            ns("title_edit")
          ),
          uiOutput(ns("block_name_out"), inline = TRUE),
          icon(
            "pen-to-square",
            class = "edit-icon",
            style = paste(
              "opacity: 0;",
              "font-size: 0.7em;",
              "color: #bbb;",
              "transition: opacity 0.15s ease;"
            )
          )
        ),
        # Edit mode - hidden by default
        div(
          id = ns("title_edit"),
          class = "blockr-title-edit",
          style = "display: none;",
          textInput(
            input_id,
            label = NULL,
            value = block_name(block)
          ),
          # JS to handle blur and enter key
          tags$script(HTML(sprintf(
            "$(document).ready(function() {
              var input = $('#%s');
              var display = $('#%s');
              var editWrap = $('#%s');
              input.on('blur', function() {
                editWrap.hide();
                display.css('display', 'flex');
              });
              input.on('keydown', function(e) {
                if (e.key === 'Enter') {
                  $(this).blur();
                }
              });
            });",
            input_id, ns("title_display"), ns("title_edit")
          )))
        )
      )
    ),
    popover(
      span(
        class = "blockr-subtitle",
        info$name
      ),
      # Title + package badge
      div(
        class = "d-flex align-items-center justify-content-between gap-2 mb-2",
        tags$strong(info$name),
        span(class = "badge-two-tone", info$package)
      ),
      # Description
      p(class = "mb-0", info$description),
      options = list(trigger = "hover")
    )
  )
}

block_card_toggles <- function(blk, ns) {

  if (is_dock_locked()) {
    return(NULL)
  }

  opts <- c("inputs", "outputs")

  section_toggles <- shinyWidgets::checkboxGroupButtons(
    inputId = ns("collapse_blk_sections"),
    status = "light",
    size = "sm",
    choices = set_names(opts, c("Input", "Output")),
    individual = TRUE,
    selected = coal(attr(blk, "visible"), opts)
  )

  # Remove the ms-auto class, add blockr-section-toggle
  section_toggles$attribs$class <- paste(
    "blockr-section-toggle",
    trimws(gsub("form-group|ms-auto", "", section_toggles$attribs$class))
  )

  section_toggles
}

block_card_dropdown <- function(ns, info, blk_id) {

  dd_header <- function(title) {
    tags$li(
      h6(class = "dropdown-header", title)
    )
  }

  dd_action <- function(title, id, symbol, class = character()) {

    cls <- c(
      "dropdown-item action-button py-2 position-relative",
      class
    )

    tags$li(
      tags$button(
        class = cls,
        type = "button",
        id = id,
        style = "padding-left: 2.5rem;",
        if (not_null(symbol)) {
          span(
            class = "position-absolute start-0 top-50 translate-middle-y ms-3",
            symbol
          )
        },
        title
      )
    )
  }

  dd_info <- function(key, val) {
    div(
      class = "d-flex justify-content-between align-items-center mb-2",
      span(key, class = "text-muted small"),
      span(val, class = "small fw-medium")
    )
  }

  dd_divider <- function() {
    tags$li(tags$hr(class = "dropdown-divider my-2"))
  }

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
        "dropdown-menu dropdown-menu-end blockr-block-dropdown",
        "shadow-sm rounded-3 border-1"
      ),
      style = "min-width: 250px;",
      if (!is_dock_locked()) {
        tagList(
          dd_header("Block Actions"),
          dd_action(
            "Append block",
            ns("append_block"),
            bsicons::bs_icon("plus", class = "text-success", size = "1.1em")
          ),
          dd_action(
            "Delete block",
            ns("delete_block"),
            bsicons::bs_icon("trash", class = "text-danger", size = "1.1em")
          ),
          dd_divider()
        )
      },
      dd_header("Block Details"),
      tags$li(
        div(
          class = "px-3 py-1",
          div(
            class = "d-flex justify-content-between align-items-center mb-2",
            span("Package", class = "text-muted small"),
            span(class = "badge-two-tone", info$package)
          ),
          dd_info("Type", info$category),
          div(
            class = "d-flex justify-content-between align-items-center mb-2",
            span("ID", class = "text-muted small"),
            div(
              class = "d-flex align-items-center gap-2",
              tags$code(
                blk_id,
                style = "font-size: var(--blockr-font-size-sm);"
              ),
              tags$button(
                class = "btn btn-link p-0 border-0 text-muted",
                style = "line-height: 1; text-decoration: none;",
                onclick = sprintf(
                  paste0(
                    "event.stopPropagation(); ",
                    "navigator.clipboard.writeText('%s'); ",
                    "var btn = this; ",
                    "var copyIcon = btn.querySelector('.copy-icon'); ",
                    "var checkIcon = btn.querySelector('.check-icon'); ",
                    "copyIcon.style.display = 'none'; ",
                    "checkIcon.style.display = ''; ",
                    "setTimeout(function() { ",
                    "checkIcon.style.display = 'none'; ",
                    "copyIcon.style.display = ''; }, 1500);"
                  ),
                  blk_id
                ),
                title = "Copy to clipboard",
                span(
                  class = "copy-icon",
                  bsicons::bs_icon("copy", size = "0.9em")
                ),
                span(
                  class = "check-icon text-success",
                  style = "display: none;",
                  bsicons::bs_icon("check", size = "0.9em")
                )
              )
            )
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
  )$addAttrs(
    style = paste0(
      "background-color: white;",
      "border-radius: 0;",
      "margin: 0 -16px 10px -16px;",
      "padding: 16px 16px 10px 16px;",
      "border-top: 1px solid var(--blockr-grey-300);",
      "border-bottom: 1px solid var(--blockr-grey-300);"
    )
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
  )$addAttrs(
    style = "padding: 0;"
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

edit_block_server <- function(callbacks = list()) {

  stopifnot(all(lgl_ply(callbacks, is.function)))

  function(id, block_id, board, update, ...) {

    stopifnot(is_string(block_id))

    dot_args <- list(...)

    moduleServer(
      id,
      function(input, output, session) {

        observeEvent(
          input$block_name_in,
          {
            req(
              input$block_name_in,
              block_id %in% board_block_ids(board$board)
            )

            blk <- board_blocks(board$board)[[block_id]]
            cur <- block_name(blk)

            if (identical(cur, input$block_name_in)) {
              return()
            }

            block_name(blk) <- input$block_name_in

            update(
              list(
                blocks = list(
                  mod = as_blocks(set_names(list(blk), block_id))
                )
              )
            )
          }
        )

        observeEvent(
          update(),
          {
            upd <- update()

            continue <- "blocks" %in% names(upd) &&
              "mod" %in% names(upd$blocks) &&
              block_id %in% names(upd$blocks$mod)

            if (!continue) {
              return()
            }

            new <- block_name(upd$blocks$mod[[block_id]])

            if (identical(new, input$block_name_in)) {
              return()
            }

            updateTextInput(
              session,
              "block_name_in",
              "Block name",
              new
            )
          }
        )

        output$block_name_out <- renderUI(
          span(
            input$block_name_in,
            class = "blockr-title"
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

        action_args <- c(list(board, update), dot_args, domain = session)

        do.call(
          append_block_action(
            reactive(req(block_id, input$append_block)),
            as_module = FALSE
          ),
          action_args
        )

        do.call(
          remove_block_action(
            reactive(req(block_id, input$delete_block)),
            as_module = FALSE
          ),
          action_args
        )

        if (length(callbacks)) {

          for (fun in callbacks) {

            res <- do.call(
              fun,
              c(
                list(block_id, board, update, conds),
                dot_args,
                list(session = session)
              )
            )

            if (!is.null(res)) {
              blockr_abort(
                "Expecting edit block server callbacks to return `NULL`.",
                class = "invalid_edit_block_server_callback_result"
              )
            }
          }
        }

        list(
          visible = reactive(input$collapse_blk_sections)
        )
      }
    )
  }
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

      remove_ui(
        paste0("#", ns("outputs_issues"))
      )

      if (length(statuses)) {
        insert_ui(
          selector = paste0("#", ns("outputs_issues_wrapper")),
          ui = create_issues_ui(statuses, ns)
        )
      }

      msgs <- NULL

      remove_ui(
        paste0("#", ns("errors_block"), " > div")
      )

      if (length(cnds[["error"]])) {
        msgs <- tags$div(
          class = "blockr-error",
          bsicons::bs_icon("exclamation-circle", class = "blockr-error-icon"),
          tags$span(
            HTML(
              cli::ansi_html(
                paste(
                  unlist(cnds[["error"]]),
                  collapse = "<br>"
                )
              )
            )
          )
        )

        insert_ui(
          paste0("#", ns("errors_block")),
          ui = msgs
        )
      }
    }
  )
}

create_issues_ui <- function(statuses, ns) {

  collapse_id <- ns("outputs_issues_collapse")
  n_issues <- length(statuses)
  issue_text <- paste(n_issues, if (n_issues == 1) "issue" else "issues")

  div(
    id = ns("outputs_issues"),
    class = "mt-3",
    tags$div(
      class = paste(
        "d-flex align-items-center justify-content-between",
        "blockr-issues-toggle"
      ),
      `data-bs-toggle` = "collapse",
      `data-bs-target` = paste0("#", collapse_id),
      `aria-expanded` = "false",
      `aria-controls` = collapse_id,
      span(issue_text),
      icon("chevron-down", class = "blockr-meta")
    ),
    collapse_container(
      id = collapse_id,
      div(
        class = "pt-2",
        statuses
      )
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
