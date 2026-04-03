edit_block_ui <- function(id, blk, blk_id, expr_ui, block_ui,
                          ctrl_ui = NULL) {

  blk_info <- blks_metadata(blk)
  ns <- NS(id)

  visible <- coal(attr(blk, "visible"), c("header", "inputs", "outputs"))
  header_hidden <- !"header" %in% visible

  div(
    class = paste("card-body", if (header_hidden) "blockr-header-hidden"),
    if (header_hidden) {
      tags$script(HTML(sprintf(
        "$(function() { Shiny.setInputValue('%s', false); });",
        ns("header_visible")
      )))
    },
    div(
      class = "d-flex align-items-stretch gap-3 blockr-header-row",
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
          # Right side: toggles, pin, dropdown
          div(
            class = "d-flex align-items-center gap-1 flex-shrink-0",
            ctrl_header_icon(ns, ctrl_ui),
            block_card_toggles(blk, ns),
            block_card_dropdown(ns, blk_info, blk_id)
          )
        )
      )
    ),
    # Restore header button — only visible when header is hidden
    tags$button(
      class = "btn blockr-restore-header-btn",
      type = "button",
      title = "Show header",
      onclick = sprintf(
        paste0(
          "var card = this.closest('.card-body');",
          "card.classList.remove('blockr-header-hidden');",
          "Shiny.setInputValue('%s', true, {priority: 'event'});"
        ),
        ns("header_visible")
      ),
      icon("chevron-down")
    ),
    ctrl_collapsible_section(ns, ctrl_ui),
    block_card_content(ns, expr_ui, block_ui, visible)
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

  icon_labels <- list(
    as.character(icon("sliders")),
    as.character(icon("eye"))
  )

  tooltip_titles <- c("Controls", "Preview")

  section_toggles <- shinyWidgets::checkboxGroupButtons(
    inputId = ns("collapse_blk_sections"),
    status = "light",
    size = "sm",
    choiceNames = icon_labels,
    choiceValues = opts,
    individual = TRUE,
    selected = intersect(coal(attr(blk, "visible"), opts), opts)
  )

  # Remove the ms-auto class, add blockr-section-toggle
  section_toggles$attribs$class <- paste(
    "blockr-section-toggle",
    trimws(gsub("form-group|ms-auto", "", section_toggles$attribs$class))
  )

  tagList(
    section_toggles,
    tags$script(HTML(sprintf(
      "$(function() {
        var id = '%s';
        var titles = %s;
        var btns = $('#' + id).find('.btn');
        btns.each(function(i) {
          $(this).attr('data-bs-toggle', 'tooltip')
                 .attr('data-bs-title', titles[i])
                 .attr('data-bs-placement', 'bottom');
        });
        // Init tooltips in closest card-body
        var card = document.getElementById(id).closest('.card-body');
        if (card) {
          var sel = '[data-bs-toggle=\"tooltip\"]';
          card.querySelectorAll(sel).forEach(function(t) {
            var tip = new bootstrap.Tooltip(t, {
              delay: { show: 1000, hide: 0 },
              trigger: 'hover'
            });
            // Hide tooltip on click to prevent it sticking
            t.addEventListener('click', function() { tip.hide(); });
          });
        }
      });",
      ns("collapse_blk_sections"),
      jsonlite::toJSON(tooltip_titles)
    )))
  )
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
      class = "d-flex justify-content-between align-items-center mb-3",
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
      class = "btn btn-light blockr-header-icon",
      type = "button",
      `data-bs-toggle` = "dropdown",
      `aria-expanded` = "false",
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
          tags$li(
            tags$button(
              class = "dropdown-item py-2 position-relative",
              type = "button",
              onclick = sprintf(
                paste0(
                  "var card = this.closest('.card-body');",
                  "card.classList.add('blockr-header-hidden');",
                  "Shiny.setInputValue('%s', false, {priority: 'event'});"
                ),
                ns("header_visible")
              ),
              span(
                class = paste(
                  "position-absolute start-0",
                  "top-50 translate-middle-y ms-3"
                ),
                bsicons::bs_icon(
                  "chevron-bar-contract",
                  class = "text-muted",
                  size = "1.1em"
                )
              ),
              "Hide header"
            )
          ),
          dd_divider()
        )
      },
      dd_header("Block Details"),
      tags$li(
        div(
          class = "px-3 py-2",
          div(
            class = "d-flex justify-content-between align-items-center mb-3",
            span("Package", class = "text-muted small"),
            span(class = "badge-two-tone", info$package)
          ),
          dd_info("Type", info$category),
          div(
            class = "d-flex justify-content-between align-items-center",
            span("ID", class = "text-muted small"),
            div(
              class = "d-flex align-items-center gap-2",
              tags$code(
                blk_id,
                style = "font-size: var(--blockr-font-size-xs);"
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

block_card_content <- function(ns, expr_ui, block_ui, initial_visible = NULL) {

  open_panels <- setdiff(
    coal(initial_visible, c("header", "inputs", "outputs")),
    "header"
  )

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
      open = open_panels,
      inputs_panel,
      outputs_panel
    )
  )
}

ctrl_header_icon <- function(ns, ctrl_ui) {

  if (is.null(ctrl_ui)) {
    return(NULL)
  }

  target_id <- ns("ctrl_section")

  tags$button(
    class = "btn btn-light blockr-header-icon blockr-sparkle-btn",
    type = "button",
    `data-bs-toggle` = "tooltip",
    `data-bs-title` = "AI Assistant",
    `data-bs-placement` = "bottom",
    onclick = sprintf(
      paste0(
        "var section = document.getElementById('%s');",
        "if (section) {",
        "  section.classList.toggle('ai-collapsed');",
        "  this.classList.toggle('active');",
        "}"
      ),
      target_id
    ),
    HTML(paste0(
      '<svg class="blockr-sparkle-svg" width="16" height="16" ',
      'viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">',
      '<path class="sparkle-main" d="M12 2L13.5 8.5L20 10L13.5 11.5L12 18',
      'L10.5 11.5L4 10L10.5 8.5L12 2Z" fill="currentColor"/>',
      '<path class="sparkle-sm sparkle-sm-1" d="M19 15L19.75 17.25L22 18',
      'L19.75 18.75L19 21L18.25 18.75L16 18L18.25 17.25L19 15Z" ',
      'fill="currentColor" opacity="0.7"/>',
      '<path class="sparkle-sm sparkle-sm-2" d="M5 1L5.5 2.5L7 3L5.5 3.5',
      'L5 5L4.5 3.5L3 3L4.5 2.5L5 1Z" fill="currentColor" opacity="0.5"/>',
      '</svg>' # nolint: quotes_linter.
    ))
  )
}

ctrl_collapsible_section <- function(ns, ctrl_ui) {

  if (is.null(ctrl_ui)) {
    return(NULL)
  }

  div(
    id = ns("ctrl_section"),
    class = "blockr-ctrl-section ai-collapsed",
    div(
      class = "blockr-ctrl-section-inner",
      div(
        class = "blockr-ctrl-section-header",
        span("AI Assistant", class = "blockr-ctrl-section-label")
      ),
      div(
        class = "blockr-ctrl-content",
        ctrl_ui
      )
    )
  )
}

edit_block_server <- function(callbacks = list()) {

  stopifnot(all(lgl_ply(callbacks, is.function)))

  function(id, block_id, board, update, actions, ...) {

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
          {
            selected <- coal(input$collapse_blk_sections, character(0))
            all_panels <- c("inputs", "outputs")
            to_open <- intersect(selected, all_panels)
            to_close <- setdiff(all_panels, selected)
            if (length(to_open) > 0L) {
              accordion_panel_open("blk_accordion", to_open, session)
            }
            if (length(to_close) > 0L) {
              accordion_panel_close("blk_accordion", to_close, session)
            }
          },
          ignoreNULL = FALSE,
          ignoreInit = TRUE
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

        observeEvent(
          input$append_block,
          actions[["append_block_action"]](block_id)
        )

        observeEvent(
          input$delete_block,
          actions[["remove_block_action"]](block_id)
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
          visible = reactive({
            sections <- input$collapse_blk_sections
            header <- input$header_visible
            c(if (!isFALSE(header)) "header", sections)
          })
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
      bsicons::bs_icon("chevron-down", class = "blockr-meta")
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
