edit_block_ui <- function(id, blk, blk_id, expr_ui, block_ui,
                          ctrl_ui = NULL, ctrl_meta = NULL) {

  blk_info <- blks_metadata(blk)
  ns <- NS(id)

  div(
    class = "card-body",
    div(
      class = "d-flex align-items-stretch gap-3",
      div(
        class = "blockr-block-icon",
        span(
          title = blk_info$description,
          blk_icon_data_uri(blk_info$icon, blk_info$color, 46, "inline")
        ),
        uiOutput(ns("status_indicator"), inline = TRUE)
      ),
      div(
        class = paste(
          "d-flex flex-column justify-content-center",
          "flex-grow-1 min-height-0"
        ),
        div(
          class = "d-flex align-items-center justify-content-between w-100",
          block_card_title(blk, id, blk_info),
          div(
            class = "d-flex align-items-center gap-1 flex-shrink-0",
            block_card_toggles(blk, ns, ctrl_meta),
            block_card_dropdown(ns, blk_info, blk_id)
          )
        )
      )
    ),
    block_card_content(ns, expr_ui, block_ui, ctrl_ui)
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

block_card_toggles <- function(blk, ns, ctrl_meta = NULL) {

  vals <- c("inputs", "outputs")
  icon_labels <- list(
    as.character(icon("sliders")),
    as.character(icon("eye"))
  )
  tooltip_titles <- c("Controls", "Preview")

  if (!is.null(ctrl_meta)) {
    vals <- c(vals, "ctrl")
    icon_labels <- c(
      icon_labels,
      list(as.character(ctrl_button_label(ctrl_meta)))
    )
    tooltip_titles <- c(
      tooltip_titles,
      coal(ctrl_meta$tooltip, ctrl_meta$label, "Control")
    )
  }

  section_toggles <- shinyWidgets::checkboxGroupButtons(
    inputId = ns("collapse_blk_sections"),
    status = "light",
    size = "sm",
    choiceNames = icon_labels,
    choiceValues = vals,
    individual = TRUE,
    selected = coal(attr(blk, "visible"), c("inputs", "outputs"))
  )

  section_toggles$attribs$class <- paste(
    "blockr-section-toggle",
    trimws(gsub("form-group|ms-auto", "", section_toggles$attribs$class))
  )

  # Locked dock: render the widget hidden so `input$collapse_blk_sections`
  # still seeds the accordion via the observer below and honors any saved
  # `attr(blk, "visible")` on restore (#122). The tooltip wiring is skipped
  # because nothing is hoverable.
  #
  # NOTE: this is a UI-only hide, not a real lock. `display: none` removes the
  # widget from view but the checkbox group is still a live Shiny input — a
  # client can flip it via `Shiny.setInputValue()` and the observer below will
  # happily mutate accordion state. "Lock" here is a UX affordance, not a
  # server-side trust boundary. The principled fix is to drop the hidden
  # widget entirely and seed the accordion directly from `attr(blk, "visible")`
  # on startup, so the observer can be gated on `!is_dock_locked()`. TODO(#TBD).
  if (is_dock_locked()) {
    return(div(style = "display: none;", section_toggles))
  }

  tagList(
    section_toggles,
    tags$script(HTML(sprintf(
      "$(function() {
        var btns = $('#%s').find('.btn');
        var titles = %s;
        btns.each(function(i) { $(this).attr('title', titles[i]); });
      });",
      ns("collapse_blk_sections"),
      jsonlite::toJSON(tooltip_titles)
    )))
  )
}

ctrl_button_label <- function(meta) {

  label <- if (nzchar(coal(meta$label, ""))) meta$label
  inner <- if (is.null(meta$icon)) label else tagList(meta$icon, label)

  if (is.null(meta$class)) {
    return(inner)
  }

  span(class = meta$class, inner)
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

block_card_content <- function(ns, expr_ui, block_ui, ctrl_ui = NULL) {

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
    tagList(
      block_ui,
      uiOutput(ns("status_note")),
      div(id = ns("outputs_issues_wrapper"))
    )
  )$allTags()

  outputs_panel$attribs$style <- "border: none; border-radius: 0;"

  ctrl_panel <- if (!is.null(ctrl_ui)) build_ctrl_panel(ctrl_ui)

  tagList(
    div(id = ns("errors_block"), class = "mt-4"),
    accordion(
      id = ns("blk_accordion"),
      multiple = TRUE,
      open = c("inputs", "outputs"),
      ctrl_panel,
      inputs_panel,
      outputs_panel
    )
  )
}

build_ctrl_panel <- function(ctrl_ui) {

  panel <- htmltools::tagQuery(
    accordion_panel(
      title = "Control",
      value = "ctrl"
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
    ctrl_ui
  )$allTags()

  panel$attribs$style <- "border: none; border-radius: 0;"

  panel
}

ctrl_btn_label <- function(fn) coal(attr(fn, "ctrl_label"), "Control")
ctrl_btn_icon  <- function(fn) attr(fn, "ctrl_icon")
ctrl_btn_class <- function(fn) attr(fn, "ctrl_class")

edit_block_server <- function(callbacks = list()) {

  stopifnot(all(lgl_ply(callbacks, is.function)))

  function(id, block_id, board, update, actions, ...) {

    stopifnot(is_string(block_id))

    dot_args <- list(...)

    moduleServer(
      id,
      function(input, output, session) {

        cur_name <- reactive(
          {
            blk <- board_blocks(board$board)[[block_id]]
            if (is_block(blk)) block_name(blk) else NULL
          }
        )

        observeEvent(
          cur_name(),
          {
            if (identical(cur_name(), input$block_name_in)) {
              return()
            }

            updateTextInput(
              session,
              "block_name_in",
              "Block name",
              cur_name()
            )
          }
        )

        observeEvent(
          input$block_name_in,
          {
            req(
              input$block_name_in,
              block_id %in% board_block_ids(board$board)
            )

            if (identical(cur_name(), input$block_name_in)) {
              return()
            }

            update(
              list(
                blocks = list(
                  mod = set_names(
                    list(list(block_name = input$block_name_in)),
                    block_id
                  )
                )
              )
            )
          }
        )

        output$block_name_out <- renderUI(
          span(
            input$block_name_in,
            class = "blockr-title"
          )
        )

        blk_status <- reactive(reval_if(board$eval[[block_id]]))

        output$status_indicator <- renderUI(
          block_status_indicator(blk_status())
        )

        output$status_note <- renderUI(
          block_status_note(blk_status())
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
            block_cond_buckets(board$blocks[[block_id]]$server$conditions())
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
          visible = reactive(input$collapse_blk_sections)
        )
      }
    )
  }
}

block_cond_buckets <- function(df) {

  df <- df[df$phase != "status", , drop = FALSE]

  lapply(
    set_names(nm = c("error", "warning", "message")),
    function(sev) df$message[df$severity == sev]
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

block_status_indicator <- function(status) {

  if (!is_string(status)) {
    return(NULL)
  }

  info <- switch(
    status,
    waiting = list(tone = "waiting", label = "Waiting for a data input"),
    unset = list(tone = "unset", label = "Set this block's inputs"),
    failed = list(tone = "failed", label = "Evaluation failed")
  )

  if (is.null(info)) {
    return(NULL)
  }

  tags$span(
    class = paste0("blockr-status-dot blockr-status-dot-", info$tone),
    title = info$label,
    role = "img",
    `aria-label` = info$label
  )
}

block_status_note <- function(status) {

  if (!is_string(status)) {
    return(NULL)
  }

  info <- switch(
    status,
    waiting = list(icon = "diagram-3", text = "Waiting for a data input"),
    unset = list(icon = "sliders", text = "Set this block's inputs")
  )

  if (is.null(info)) {
    return(NULL)
  }

  div(
    class = "blockr-status-note",
    bsicons::bs_icon(info$icon, class = "blockr-status-note-icon"),
    tags$span(info$text)
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
