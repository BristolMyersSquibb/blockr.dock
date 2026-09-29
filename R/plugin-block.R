edit_block_ui <- function(id, blk, blk_id, expr_ui, block_ui,
                          ctrl_ui = NULL, ctrl_meta = NULL) {

  blk_info <- blks_metadata(blk)
  ns <- NS(id)
  has_inputs <- has_expr_ui(blk)
  visible <- visible_sections(blk)

  div(
    class = "card-body",
    # Header parts each carry an owned class and nothing here is styled
    # inline: an inline declaration outranks any sheet a theme can attach, so
    # a single `style=` attribute on a part is enough to make that part
    # unthemable.
    div(
      class = "blockr-block-header",
      div(
        class = "blockr-block-icon",
        block_mark(blk, blk_info),
        block_status_dot(ns)
      ),
      div(
        class = "blockr-block-header-main",
        div(
          class = "blockr-block-header-row",
          block_card_title(blk, id, blk_info),
          div(
            class = "blockr-block-header-actions",
            block_card_toggles(visible, ns, ctrl_meta, has_inputs),
            block_card_dropdown(ns, blk_info, blk_id)
          )
        )
      )
    ),
    block_card_content(ns, expr_ui, block_ui, visible, ctrl_ui, has_inputs)
  )
}

visible_sections <- function(blk) {
  as.character(coal(attr(blk, "visible"), c("inputs", "outputs")))
}

# The toggle reports `NULL` for "every section hidden", which is also what the
# input holds before the card has reported at all -- and the freeze gate has
# to tell those apart. Shiny registers the key on the first report even when
# the value is `NULL`, so the key discriminates; the unconditional read is
# what takes the dependency that wakes a caller still on the other branch.
reported_sections <- function(input) {

  sections <- input$collapse_blk_sections

  if ("collapse_blk_sections" %in% names(input)) {
    coal(sections, character())
  }
}

# The block's mark: its category colour as a tinted square with the glyph in
# that colour. The colour is handed to the stylesheet as a custom property
# rather than painted inline, so the tint, the size and the radius stay a
# theme's to change. The header prints no subtitle; the block type, with the
# package as a badge, is the mark's tooltip (Blockr.tooltip, handed over by
# block-tooltips.js from these attributes).
block_mark <- function(blk, info) {

  type <- gsub("_", " ", class(blk)[1L])

  span(
    class = "blockr-block-mark",
    style = paste0("--blockr-dock-cat: ", info$color, ";"),
    `data-blockr-tip` = type,
    `data-blockr-tip-badge` = info$package,
    `aria-label` = paste(type, info$package, sep = ", "),
    role = "img",
    HTML(info$icon)
  )
}

block_card_title <- function(block, id, info) {
  ns <- NS(id)
  input_id <- ns("block_name_in")
  editable <- !is_dock_locked()

  div(
    class = "blockr-block-title-wrap",
    div(
      class = "blockr-block-title",
      # Inline editable title container
      div(
        class = "blockr-inline-edit",
        # Display mode. A double-click (or "Rename" in the block's menu)
        # starts editing, so a single click is free to select the panel. The
        # affordance is blockr.ui's editable text (text cursor, "Double-click
        # to edit" tooltip) and a hover wash in blockr-dock.css; the handler
        # only swaps the two modes and focuses the field. The name is hidden
        # with `visibility`, not `display`: it keeps its box, so the row keeps
        # its height and the field, positioned against it, lands on the name.
        # A locked board refuses the rename, so its title offers none.
        div(
          id = ns("title_display"),
          class = "blockr-title-display",
          `data-blockr-editable` = if (editable) "",
          ondblclick = if (editable) {
            sprintf(
              paste0(
                "this.style.visibility='hidden';",
                "var editWrap = document.getElementById('%s');",
                "editWrap.style.display='block';",
                "var input = editWrap.querySelector('input');",
                "input.focus();",
                "input.select();"
              ),
              ns("title_edit")
            )
          },
          tags$span(class = "blockr-title", block_name(block))
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
          div(class = "blockr-title-error", "A block needs a name"),
          # The displayed title mirrors this input, so it is kept in sync here
          # rather than by a server-rendered output: `updateTextInput()` fires
          # 'change', so a rename decided by the board lands the same way a
          # keystroke does, with no render round-trip. Enter and a click
          # elsewhere commit, Escape restores the name editing began with. An
          # empty name is refused in place on Enter and dropped on blur; the
          # server ignores it either way.
          tags$script(HTML(sprintf(
            "$(document).ready(function() {
              var input = $('#%s');
              var display = $('#%s');
              var editWrap = $('#%s');
              var before = input.val();
              input.on('focus', function() {
                before = input.val();
                editWrap.removeClass('is-invalid');
              });
              input.on('blur', function() {
                if (!$.trim(input.val())) {
                  input.val(before).trigger('change');
                }
                editWrap.removeClass('is-invalid');
                editWrap.hide();
                // Clear the inline visibility the handler wrote, rather than
                // setting one: the class owns how the row looks.
                display.css('visibility', '');
              });
              input.on('keydown', function(e) {
                if (e.key === 'Enter') {
                  if (!$.trim(input.val())) {
                    editWrap.addClass('is-invalid');
                    return;
                  }
                  $(this).blur();
                }
                if (e.key === 'Escape') {
                  $(this).val(before).trigger('change');
                  $(this).blur();
                }
              });
              input.on('input change', function() {
                if ($.trim(input.val())) {
                  editWrap.removeClass('is-invalid');
                }
                display.find('.blockr-title').text($(this).val());
              });
            });",
            input_id, ns("title_display"), ns("title_edit")
          )))
        )
      )
    )
  )
}

block_card_toggles <- function(visible, ns, ctrl_meta = NULL,
                               has_inputs = TRUE) {

  vals <- c("inputs", "outputs")
  icon_labels <- list(
    as.character(icon("sliders")),
    as.character(icon("eye"))
  )
  tooltip_titles <- c("Controls", "Preview")

  if (!has_inputs) {
    keep <- vals != "inputs"
    vals <- vals[keep]
    icon_labels <- icon_labels[keep]
    tooltip_titles <- tooltip_titles[keep]
  }

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
    selected = visible
  )

  section_toggles$attribs$class <- paste(
    "blockr-section-toggle",
    trimws(gsub("form-group|ms-auto", "", section_toggles$attribs$class))
  )

  # A locked card offers no toggle at all: the accordion is seeded from
  # `visible` at render, so nothing here has to report back to place it. The
  # widget used to be rendered hidden for exactly that seeding, which left a
  # live Shiny input a client could flip via `Shiny.setInputValue()`.
  if (is_dock_locked()) {
    return(NULL)
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

  # Starts the same in-place rename as a double-click on the title. The
  # timeout lets the dropdown finish closing first, so its focus handling does
  # not blur the field it just opened.
  dd_rename <- function(display_id, symbol) {
    tags$li(
      tags$button(
        class = "dropdown-item py-2 position-relative",
        type = "button",
        onclick = sprintf(
          paste0(
            "setTimeout(function() {",
            "document.getElementById('%s').dispatchEvent(",
            "new MouseEvent('dblclick', {bubbles: true})); }, 0);"
          ),
          display_id
        ),
        span(
          class = "position-absolute start-0 top-50 translate-middle-y ms-3",
          symbol
        ),
        "Rename"
      )
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
      title = "More actions",
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
          dd_rename(
            ns("title_display"),
            bsicons::bs_icon("pencil", class = "text-muted", size = "1.1em")
          ),
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

block_card_content <- function(ns, expr_ui, block_ui, visible,
                               ctrl_ui = NULL, has_inputs = TRUE) {

  inputs_panel <- if (has_inputs) {
    accordion_panel(
      title = NULL,
      value = "inputs",
      expr_ui
    )
  }

  outputs_panel <- accordion_panel(
    title = NULL,
    value = "outputs",
    block_ui,
    block_status_notes(ns),
    block_issues_ui(ns)
  )

  ctrl_panel <- if (!is.null(ctrl_ui)) {
    accordion_panel(title = NULL, value = "ctrl", ctrl_ui)
  }

  sections <- c(
    if (!is.null(ctrl_ui)) "ctrl",
    if (has_inputs) "inputs",
    "outputs"
  )

  tagList(
    div(id = ns("errors_block"), class = "blockr-block-errors"),
    accordion(
      id = ns("blk_accordion"),
      class = "blockr-block-accordion",
      # The open sections, kept current by block-sections.js. The stylesheet
      # reads it to draw the rule above the preview only when controls are
      # open above it, which the preview's own panel cannot see. Only sections
      # the card has count: a block without inputs still lists "inputs" among
      # its visible sections by default.
      `data-open` = paste(intersect(visible, sections), collapse = " "),
      multiple = TRUE,
      # An empty set has to travel as FALSE: bslib reads `character()` the same
      # as an absent `open` and falls back to opening the first panel.
      open = if (length(visible)) visible else FALSE,
      ctrl_panel,
      inputs_panel,
      outputs_panel
    )
  )
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
            # The field reports every keystroke, and the card refuses a name of
            # spaces the same as an empty one.
            req(
              trimws(input$block_name_in),
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

        blk_status <- reactive(reval_if(board$eval[[block_id]]))

        conds <- reactive(
          {
            req(block_id %in% names(board$blocks))
            block_cond_buckets(board$blocks[[block_id]]$server$conditions())
          }
        )

        output$status_indicator <- render_attrs(
          block_status_dot_attrs(blk_status(), sum(lengths(conds()$error)))
        )

        output$status_note <- render_attrs(
          block_status_note_attrs(blk_status())
        )

        # These carry user toggles only -- the card paints with its saved
        # sections already open -- so a locked dock, which renders no toggle
        # widget, wires neither and a forged `collapse_blk_sections` moves
        # nothing.
        if (!is_dock_locked()) {

          observeEvent(
            input$collapse_blk_sections,
            accordion_panel_set(
              "blk_accordion",
              input$collapse_blk_sections,
              session
            )
          )

          # Hiding the last section reports NULL, which the setter above drops
          # as its `ignoreNULL` default -- and could not carry anyway, since the
          # set message rejects an empty selection. Closing all is its own
          # message, observed separately.
          observeEvent(
            is.null(input$collapse_blk_sections),
            if (is.null(input$collapse_blk_sections)) {
              accordion_panel_close("blk_accordion", TRUE, session)
            },
            ignoreInit = TRUE
          )
        }

        output$issues_count <- renderText(cond_issue_label(conds()))
        outputOptions(output, "issues_count", suspendWhenHidden = FALSE)

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

        # Read once rather than per board commit: building a block's controls
        # to answer costs ~10ms, and core re-runs this server whenever the
        # block is modified, so the answer cannot go stale under it.
        blk <- board_blocks(isolate(board$board))[[block_id]]

        # A locked card has no toggle to report from, and its sections cannot
        # move from what it painted with -- so report that set instead. It is
        # what serialization stores, and what freeze_hidden_inputs() reads to
        # tell a card that has reported in from one still painting.
        visible <- if (is_dock_locked()) {
          reactive(visible_sections(blk))
        } else {
          reactive(reported_sections(input))
        }

        list(
          visible = visible,
          has_inputs = has_expr_ui(blk)
        )
      }
    )
  }
}

block_cond_buckets <- function(df) {

  df <- df[df$phase != "status", , drop = FALSE]

  lapply(
    set_names(nm = c("error", "warning", "message")),
    function(sev) {
      rows <- df[df$severity == sev, , drop = FALSE]
      rows <- rows[!duplicated(rows$id), , drop = FALSE]
      set_names(rows$message, rows$id)
    }
  )
}

update_blk_cond_observer <- function(conds, session = get_session()) {

  ns <- session$ns
  rendered <- character()

  observeEvent(
    conds(),
    {
      specs <- cond_ui_specs(conds(), ns)
      keep <- names(specs)

      for (dom_id in setdiff(rendered, keep)) {
        remove_ui(paste0("#", dom_id))
      }

      for (dom_id in setdiff(keep, rendered)) {
        insert_ui(
          selector = specs[[dom_id]]$selector,
          ui = specs[[dom_id]]$ui
        )
      }

      rendered <<- keep
    }
  )
}

cond_ui_specs <- function(cnds, ns) {

  selectors <- c(
    error = paste0("#", ns("errors_block")),
    warning = paste0("#", ns("outputs_issues_warnings")),
    message = paste0("#", ns("outputs_issues_messages"))
  )

  specs <- list()

  for (severity in names(selectors)) {

    msgs <- cnds[[severity]]
    ids <- names(msgs)

    for (i in seq_along(msgs)) {

      dom_id <- ns(paste0("cond_", severity, "_", ids[[i]]))

      specs[[dom_id]] <- list(
        selector = selectors[[severity]],
        ui = cond_alert(dom_id, msgs[[i]], severity)
      )
    }
  }

  specs
}

cond_alert <- function(dom_id, msg, severity) {

  content <- HTML(cli::ansi_html(msg))

  if (identical(severity, "error")) {
    return(
      tags$div(
        id = dom_id,
        class = "blockr-error",
        bsicons::bs_icon("exclamation-circle", class = "blockr-error-icon"),
        tags$span(content)
      )
    )
  }

  cl <- switch(severity, warning = "warning", message = "light")

  tags$div(
    id = dom_id,
    class = sprintf("blockr-issue alert alert-%s", cl),
    content
  )
}

cond_issue_label <- function(cnds) {

  n <- length(cnds$warning) + length(cnds$message)

  paste(n, if (n == 1L) "issue" else "issues")
}

block_issues_ui <- function(ns) {

  collapse_id <- ns("outputs_issues_collapse")

  div(
    id = ns("outputs_issues"),
    class = "mt-3 blockr-issues",
    tags$div(
      class = paste(
        "d-flex align-items-center justify-content-between",
        "blockr-issues-toggle"
      ),
      `data-bs-toggle` = "collapse",
      `data-bs-target` = paste0("#", collapse_id),
      `aria-expanded` = "false",
      `aria-controls` = collapse_id,
      textOutput(ns("issues_count"), inline = TRUE),
      bsicons::bs_icon("chevron-down", class = "blockr-meta")
    ),
    collapse_container(
      id = collapse_id,
      div(
        class = "pt-2",
        div(id = ns("outputs_issues_warnings")),
        div(id = ns("outputs_issues_messages"))
      )
    )
  )
}

block_status_style <- function(status) {

  if (!is_string(status)) {
    return(NULL)
  }

  spec <- switch(
    status,
    stale = list(
      color = "#6b7280",
      label = "Inputs changed since this block last ran"
    ),
    waiting = list(color = "#f59e0b", label = "Waiting for a data input"),
    unset = list(color = "#f59e0b", label = "Set this block's inputs"),
    failed = list(color = "#dc2626", label = "Evaluation failed")
  )

  if (is.null(spec)) {
    return(NULL)
  }

  c(spec, list(size = 8L, ring = 2L, ring_color = "#ffffff"))
}

#' @param status A block eval status: `stale`, `waiting`, `unset` and `failed`
#'   carry a badge; `ready` carries none; `dormant` is indeterminate; any other
#'   value yields no badge. The `size` field is the coloured dot's pixel
#'   diameter and `ring` its white outline width, both shared so the dock card
#'   icon and the DAG node badge render identically.
#' @param error_count Number of error conditions the block has raised. A
#'   positive count promotes the badge to `failed`, catching render-phase
#'   errors that leave the eval status `ready`. A `stale` block is exempt:
#'   its conditions were raised against inputs it no longer has.
#' @rdname meta
#' @export
block_status_badge <- function(status, error_count = 0L) {

  # A stale block's conditions predate the upstream change that made it stale,
  # and it has not re-run since, so they say nothing about whether it would
  # still fail on its current inputs.
  if (isTRUE(status == "stale")) {
    return(block_status_style("stale"))
  }

  if (error_count > 0L) {
    status <- "failed"
  }

  # A dormant block has no computed status: return `NA` to signal "leave the
  # badge as-is", so a persistent renderer (the DAG node) keeps its last-known
  # badge rather than clearing it when the block drops out of the eval set.
  if (isTRUE(status == "dormant")) {
    return(NA)
  }

  block_status_style(status)
}

block_status_dot <- function(ns) {
  tags$span(
    id = ns("status_indicator"),
    class = "blockr-status-dot blockr-attr-output"
  )
}

block_status_dot_attrs <- function(status, error_count = 0L) {

  spec <- block_status_badge(status, error_count)

  if (!is.list(spec)) {
    return(list(style = "", title = "", role = "", `aria-label` = ""))
  }

  # The fill reads a blockr.ui meaning token with the shared literal as its
  # fallback, so the dot follows the scheme and a theme while the DAG keeps
  # using the literal. A waiting block draws a ring instead of a dot: the
  # solid amber stays for the block that needs input, not for every block
  # downstream of it.
  key <- if (error_count > 0L) "failed" else status
  token <- switch(
    key,
    stale = "--blockr-color-text-muted",
    failed = "--blockr-color-border-danger",
    "--blockr-color-border-warning"
  )
  fill <- sprintf("var(%s, %s)", token, spec$color)
  ring <- sprintf(
    "0 0 0 %dpx var(--blockr-color-bg-surface, %s)", spec$ring, spec$ring_color
  )

  if (identical(key, "waiting")) {
    shadow <- paste0("inset 0 0 0 1.5px ", fill, ", ", ring)
    fill <- sprintf("var(--blockr-color-bg-surface, %s)", spec$ring_color)
  } else {
    shadow <- ring
  }

  list(
    style = htmltools::css(
      width = paste0(spec$size, "px"),
      height = paste0(spec$size, "px"),
      `background-color` = fill,
      `box-shadow` = shadow
    ),
    title = spec$label,
    role = "img",
    `aria-label` = spec$label
  )
}

block_status_note_icons <- c(waiting = "diagram-3", unset = "sliders")

block_status_notes <- function(ns) {
  div(
    id = ns("status_note"),
    class = "blockr-status-note-slot blockr-attr-output",
    lapply(names(block_status_note_icons), block_status_note)
  )
}

block_status_note_attrs <- function(status) {
  list(
    `data-status` = if (is_string(status)) status else ""
  )
}

block_status_note <- function(status) {

  if (!is_string(status) || !status %in% names(block_status_note_icons)) {
    return(NULL)
  }

  div(
    class = "blockr-status-note",
    `data-status` = status,
    bsicons::bs_icon(
      block_status_note_icons[[status]],
      class = "blockr-status-note-icon"
    ),
    tags$span(block_status_style(status)$label)
  )
}

render_attrs <- function(expr, env = parent.frame(), quoted = FALSE) {
  createRenderFunction(
    installExprFunction(expr, "func", env, quoted)
  )
}

attr_output_dep <- function() {
  htmltools::htmlDependency(
    "blockr-attr-output",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "attr-output.js"
  )
}

block_sections_dep <- function() {
  htmltools::htmlDependency(
    "blockr-block-sections",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "block-sections.js"
  )
}

edit_block_validator <- function(x) {

  stopifnot(
    is.list(x),
    setequal(names(x), c("visible", "has_inputs")),
    is.reactive(x[["visible"]]),
    is_bool(x[["has_inputs"]])
  )

  invisible(x)
}
