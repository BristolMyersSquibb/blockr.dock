edit_block_ui <- function(id, blk, blk_id, expr_ui, block_ui,
                          ctrl_ui = NULL, ctrl_meta = NULL) {

  blk_info <- blks_metadata(blk)
  ns <- NS(id)
  has_inputs <- has_expr_ui(blk)
  visible <- visible_sections(blk)

  div(
    class = "blockr-block-card-body",
    # Header parts take their look from the stylesheet, never from a `style=`
    # attribute, which would outrank any sheet a theme attaches. The status
    # dot is the exception: its binding writes the spec it shares with the DAG.
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
            block_card_dropdown(ns, blk_id, has_inputs)
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
# The key test runs under isolate(): `names()` on a module's input depends on
# the session's whole name set, so every new input anywhere on the board
# would invalidate every block's `visible` -- and freeze_hidden_inputs(),
# which reads all of them, would redo the whole board per card mount.
reported_sections <- function(input) {

  sections <- input$collapse_blk_sections

  if ("collapse_blk_sections" %in% isolate(names(input))) {
    coal(sections, character())
  }
}

# The category colour goes to the stylesheet as a custom property rather than
# being painted inline, so the tint, size and radius stay a theme's to change.
# Type and package become the mark's tooltip through block-tooltips.js.
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
        # A double-click (or "Rename" in the block's menu) starts editing,
        # which block-rename.js does, so a single click stays free to select
        # the panel. A locked board refuses renames, so its title is not
        # marked editable.
        div(
          id = ns("title_display"),
          class = "blockr-title-display",
          `data-blockr-editable` = if (editable) "",
          tags$span(class = "blockr-title", block_name(block))
        ),
        # Edit mode - hidden by default
        div(
          id = ns("title_edit"),
          class = "blockr-title-edit",
          style = "display: none;",
          # The displayed title mirrors this input (block-rename.js), so a
          # rename decided by the board, which `updateTextInput()` delivers,
          # lands the way a keystroke does, with no render round-trip.
          # With `updateOn = "blur"` the name reaches the server on Enter or
          # when the field loses focus, so a rename is one board update rather
          # than one per keystroke (about half a second of server time each on
          # a large board); the browser shows the title as it is typed.
          textInput(
            input_id,
            label = NULL,
            value = block_name(block),
            updateOn = "blur"
          ),
          div(class = "blockr-title-error", "A block needs a name")
        )
      )
    )
  )
}

# The card's open sections, as the `collapse_blk_sections` input
# (section-toggle.js). The preview and a block's own control have a button
# each; the controls are toggled from the card's "…" menu.
block_card_toggles <- function(visible, ns, ctrl_meta = NULL,
                               has_inputs = TRUE) {

  # A locked card offers no toggle at all: the sections are seeded from
  # `visible` at render, so nothing here has to report back to place them. The
  # widget used to be rendered hidden for exactly that seeding, which left a
  # live Shiny input a client could flip via `Shiny.setInputValue()`.
  if (is_dock_locked()) {
    return(NULL)
  }

  sections <- c(
    if (has_inputs) "inputs",
    "outputs",
    if (!is.null(ctrl_meta)) "ctrl"
  )

  open <- intersect(visible, sections)

  toggle <- function(section, title, label) {
    on <- section %in% open
    tags$button(
      type = "button",
      class = paste("btn btn-light", if (on) "active"),
      `data-section` = section,
      `aria-pressed` = tolower(on),
      title = title,
      label
    )
  }

  div(
    id = ns("collapse_blk_sections"),
    class = "blockr-section-toggle",
    `data-sections` = paste(open, collapse = " "),
    toggle("outputs", "Preview", blockr.ui::small_icon("eye")),
    # A block's own control is drawn as the preview is: its icon, or its
    # label where it has none, and named by its label.
    if (!is.null(ctrl_meta)) {
      toggle("ctrl", ctrl_meta$label, coal(ctrl_meta$icon, ctrl_meta$label))
    },
    section_toggle_dep()
  )
}

section_toggle_dep <- function() {
  htmltools::htmlDependency(
    "blockr-section-toggle",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "section-toggle.js"
  )
}

# The block's "…" menu, drawn by Blockr.menu (blockr.ui) from the config on
# the trigger; block-menu.js opens it and turns each item's `action` into a
# pick: `section` toggles one of the card's sections, `input` sends the same
# event the old dropdown buttons did, so the server's observers are
# unchanged, `rename` starts the in-place rename and `copy` puts the block
# ID on the clipboard. A locked dock lists no action that changes the board.
block_card_dropdown <- function(ns, blk_id, has_inputs = FALSE) {

  locked <- is_dock_locked()

  # Plain actions have no icon; Remove, which destroys something, has the
  # bin and sits after a divider (design system, "The block's '…' menu").
  # The block's type and package are the tooltip on its mark, not a head.
  items <- c(
    if (has_inputs && !locked) {
      list(
        list(
          label = "Controls", icon = "sliders", action = "section",
          section = "inputs", target = ns("collapse_blk_sections")
        ),
        list(divider = TRUE)
      )
    },
    if (!locked) {
      list(
        list(
          label = "Rename", action = "rename", target = ns("title_display")
        ),
        list(
          label = "Append block", action = "input", target = ns("append_block")
        )
      )
    },
    list(
      list(
        label = "Copy block ID", meta = blk_id, mono = TRUE, action = "copy",
        target = blk_id
      )
    ),
    if (!locked) {
      list(
        list(divider = TRUE),
        list(
          label = "Remove block", icon = "trash", danger = TRUE,
          action = "input", target = ns("delete_block")
        )
      )
    }
  )

  config <- list(align = "end", items = items)

  tags$button(
    id = ns("block_menu"),
    class = "btn btn-light blockr-header-icon blockr-block-menu-btn",
    type = "button",
    title = "More actions",
    `data-blockr-menu` = jsonlite::toJSON(config, auto_unbox = TRUE),
    blockr.ui::small_icon("dots"),
    block_menu_dep()
  )
}

block_menu_dep <- function() {
  htmltools::htmlDependency(
    "blockr-block-menu",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "block-menu.js"
  )
}

block_card_content <- function(ns, expr_ui, block_ui, visible,
                               ctrl_ui = NULL, has_inputs = TRUE) {

  # A closed section is `hidden`, so Shiny suspends the outputs in it. In
  # the browser, section-toggle.js opens and closes them as the header's
  # toggles flip; the server only hears the new set.
  section <- function(value, ...) {
    div(
      class = "blockr-block-section",
      `data-value` = value,
      hidden = if (!value %in% visible) NA,
      div(class = "blockr-block-section-body", ...)
    )
  }

  tagList(
    div(id = ns("errors_block"), class = "blockr-block-errors"),
    div(
      id = ns("blk_sections"),
      class = "blockr-block-sections",
      if (!is.null(ctrl_ui)) section("ctrl", ctrl_ui),
      if (has_inputs) section("inputs", expr_ui),
      section(
        "outputs",
        block_ui,
        block_status_notes(ns),
        block_issues_ui(ns)
      )
    )
  )
}

# A control without a label, or with an empty one, is named "Control".
ctrl_btn_label <- function(fn) {
  label <- attr(fn, "ctrl_label")
  if (is_string(label) && nzchar(label)) label else "Control"
}

ctrl_btn_icon <- function(fn) attr(fn, "ctrl_icon")

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

            # The value goes by name: the third positional argument is the
            # label, and the rename field has none (it would push the field
            # off the name it sits on).
            updateTextInput(session, "block_name_in", value = cur_name())
          }
        )

        observeEvent(
          input$block_name_in,
          {
            # Shiny sends the field on Enter and on blur before block-rename.js
            # refuses an empty name or restores the old one, so a blank name
            # still arrives here. The card refuses a name of spaces the same as
            # an empty one.
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

        output$issues_count <- renderText(cond_issue_label(conds()))
        outputOptions(output, "issues_count", suspendWhenHidden = FALSE)

        update_blk_cond_observer(conds, session)

        observeEvent(
          input$append_block,
          actions[["append_block_action"]](
            block_id,
            at = input$append_block$at
          )
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

  tags$div(
    id = dom_id,
    class = sprintf("blockr-issue blockr-issue--%s", severity),
    content
  )
}

cond_issue_label <- function(cnds) {

  n <- length(cnds$warning) + length(cnds$message)

  paste(n, if (n == 1L) "issue" else "issues")
}

# The warnings and messages under the preview, folded behind their count. A
# native disclosure: the summary is the toggle, and the browser opens and
# closes it.
block_issues_ui <- function(ns) {
  tags$details(
    id = ns("outputs_issues"),
    class = "blockr-issues",
    tags$summary(
      class = "blockr-issues-toggle",
      textOutput(ns("issues_count"), inline = TRUE),
      blockr.ui::small_icon("chevron")
    ),
    div(
      class = "blockr-issues-body",
      div(id = ns("outputs_issues_warnings")),
      div(id = ns("outputs_issues_messages"))
    )
  )
}

block_status_style <- function(status) {

  if (!is_string(status)) {
    return(NULL)
  }

  # Each colour is named by the blockr.ui token it reads, next to that token's
  # light value, which a renderer that cannot read CSS (the DAG's canvas) or a
  # page without the token sheet falls back to.
  spec <- switch(
    status,
    stale = list(
      color = "#6b7280",
      token = "--blockr-color-text-muted",
      label = "Inputs changed since this block last ran"
    ),
    waiting = list(
      color = "#d97706",
      token = "--blockr-color-border-warning",
      label = "Waiting for a data input"
    ),
    unset = list(
      color = "#d97706",
      token = "--blockr-color-border-warning",
      label = "Set this block's inputs"
    ),
    failed = list(
      color = "#dc2626",
      token = "--blockr-color-border-danger",
      label = "Evaluation failed"
    )
  )

  if (is.null(spec)) {
    return(NULL)
  }

  # A waiting block is drawn hollow: the solid amber stays for the block that
  # needs input, not for every block downstream of it.
  c(
    spec,
    list(
      hollow = identical(status, "waiting"),
      outline = 1.5,
      size = 8L,
      ring = 2L,
      ring_color = "#ffffff",
      ring_token = "--blockr-color-bg-surface"
    )
  )
}

#' @param status A block eval status: `stale`, `waiting`, `unset` and `failed`
#'   carry a badge; `ready` and `unevaluated` carry none; any other value
#'   yields no badge. The `size` field is the coloured dot's pixel
#'   diameter and `ring` the width of the ring around it, both shared by the
#'   dock card icon and the DAG node badge.
#' @param error_count Number of error conditions the block has raised. A
#'   positive count promotes the badge to `failed`, catching render-phase
#'   errors that leave the eval status `ready`. A `stale` block is exempt:
#'   its conditions were raised against inputs it no longer has.
#' @rdname meta
#' @export
block_status_badge <- function(status, error_count = 0L) {

  # A stale block's conditions predate the change that made it stale,
  # and it has not re-run since, so they say nothing about whether it would
  # still fail on its current inputs.
  if (isTRUE(status == "stale")) {
    return(block_status_style("stale"))
  }

  if (error_count > 0L) {
    status <- "failed"
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

  fill <- sprintf("var(%s, %s)", spec$token, spec$color)
  surface <- sprintf("var(%s, %s)", spec$ring_token, spec$ring_color)
  ring <- sprintf("0 0 0 %dpx %s", spec$ring, surface)

  list(
    style = htmltools::css(
      width = paste0(spec$size, "px"),
      height = paste0(spec$size, "px"),
      `background-color` = if (spec$hollow) surface else fill,
      `box-shadow` = if (spec$hollow) {
        sprintf("inset 0 0 0 %gpx %s, %s", spec$outline, fill, ring)
      } else {
        ring
      }
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

block_rename_dep <- function() {
  htmltools::htmlDependency(
    "blockr-block-rename",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "block-rename.js"
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
