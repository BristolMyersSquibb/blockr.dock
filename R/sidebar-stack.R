stack_menu_ui <- function(id, board, target = NULL) {
  stopifnot(is.character(id), length(id) == 1L, nzchar(id))
  stopifnot(
    is.null(target) ||
      (is.character(target) && length(target) == 1L && nzchar(target))
  )

  ns <- NS(id)
  ctx <- resolve_stack_target(board, target)
  metas <- stack_menu_block_metas(board, ctx$pool, ctx$selected)

  htmltools::attachDependencies(
    stack_menu_panel(ns, metas, ctx),
    list(block_browser_dep(), stack_menu_dep())
  )
}

stack_menu_server <- function(id, board = NULL, target = NULL) {
  stopifnot(is.character(id), length(id) == 1L, nzchar(id))

  board_fn <- as_arg_reactive(board)
  target_fn <- as_arg_reactive(target)

  moduleServer(
    id,
    function(input, output, session) {
      # Live board sync: when a board reactive is supplied, refresh the
      # open menu in place on every board change by pushing a `menu:sync`
      # diff to our own binding. Self-scoped (addressed to this module's
      # `commit` input), so it no-ops harmlessly when the panel isn't
      # mounted - the consumer needs no ownership tracking. `ignoreInit`
      # because the initial render is already current.
      if (is.reactive(board)) {
        observeEvent(
          board_fn(),
          {
            brd <- board_fn()
            tgt <- target_fn()
            # In edit mode the stack being edited may have just been
            # removed from the board; there's nothing left to sync to, so
            # skip (the consumer decides whether to close the sidebar).
            # Without this guard `stack_sync_payload()` -> `lookup_stack()`
            # aborts. Bare "commit": the module session namespaces it to
            # the panel root's id (passing `session$ns("commit")` would
            # double-prefix).
            if (is.null(tgt) ||
                  tgt %in% board_stack_ids(brd)) {
              session$sendInputMessage("commit", stack_sync_payload(brd, tgt))
            }
          },
          ignoreInit = TRUE
        )
      }

      eventReactive(
        input$commit,
        {
          spec <- list(
            blocks = input$commit$blocks %||% character(),
            name = input[["stack_name"]],
            color = input[["stack_color"]],
            id = input[["stack_id"]]
          )
          # The menu owns validation when the consumer opts in by passing
          # a board reactive: a rejected commit notifies and `req()`s out,
          # so the committed reactive never fires with an invalid spec and
          # the consumer needs no validators of its own. With `board =
          # NULL` (snapshot mode) validation is skipped for backward
          # compatibility.
          if (is.reactive(board)) {
            validate_stack_spec(spec, board_fn(), target_fn(), session)
          }
          stack_commit_value(spec, target_fn())
        },
        ignoreNULL = TRUE
      )
    }
  )
}

# Turn the validated spec into a stacks object of `dock_stack` objects -
# one stack carrying name + colour, keyed by its id (`spec$id` in create,
# the edited stack's `target` id in edit). The colour is a first-class
# attribute of `dock_stack` rather than something smuggled onto a bare
# core `stack`, so the consumer applies the result without reshaping.
# `dock_stack` lives in `blockr.dock`, which owns the colored-stack type;
# the stack menu is only meaningful in that context.
stack_commit_value <- function(spec, target) {

  id <- target %||% spec$id
  stk <- new_dock_stack(
    as.character(spec$blocks %||% character()),
    name = spec$name,
    color = spec$color
  )
  as_stacks(set_names(list(stk), id))
}

# Validate the committed stack spec against the current board. In create
# mode (`target` is NULL) the id must be a fresh, non-empty scalar; in
# edit mode the id field is absent and immutable, so only name / colour
# are checked. Each failure notifies and `req(FALSE)`s, which propagates
# up through the enclosing `eventReactive` and stops it firing.
validate_stack_spec <- function(spec, board, target, session) {
  existing_ids <- safe_ids(board, board_stack_ids)
  if (is.null(target) && !is_new_id(spec$id, existing_ids)) {
    notify(
      "Please choose a valid stack ID.", type = "warning", session = session
    )
    req(FALSE)
  }
  if (!(is_string(spec$name) && nzchar(spec$name))) {
    notify(
      "Please choose a valid stack name.", type = "warning", session = session
    )
    req(FALSE)
  }
  if (!is_hex_color(spec$color)) {
    notify(
      "Please choose a valid stack color.", type = "warning", session = session
    )
    req(FALSE)
  }
  invisible(TRUE)
}

# Build the `menu:sync` payload: the full set of cards that SHOULD exist
# for the current board, each with its rendered markup so the client can
# insert ones it doesn't yet have. The client removes cards no longer in
# the set, inserts missing ones, and leaves surviving cards (and their
# selected state) untouched.
stack_sync_payload <- function(board, target) {
  ctx <- resolve_stack_target(board, target)
  metas <- stack_menu_block_metas(board, ctx$pool, ctx$selected)
  list(
    type = "menu:sync",
    cards = lapply(metas, function(m) {
      list(id = m$type, html = as.character(stack_block_card(m)))
    })
  )
}

stack_menu_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-stack-menu",
    version = utils::packageVersion("blockr.dock"),
    package = "blockr.dock",
    src = "assets",
    stylesheet = "css/blockr-stack-menu.css",
    script = "js/blockr-stack-menu.js",
    all_files = FALSE
  )
}

# ---- target resolution ------------------------------------------------

# Resolve everything the panel needs to know about the create / edit
# context in one place: the pool of eligible block ids, the pre-selected
# subset, the pre-filled name / color, and the seeded stack id default.
# The caller (a sidebar or modal) owns its own title chrome; the menu
# does not render context labels.
resolve_stack_target <- function(board, target) {
  pool <- stack_eligible_blocks(board)

  if (is.null(target)) {
    return(list(
      mode = "create",
      pool = pool,
      selected = character(),
      name = seed_stack_name(board),
      color = default_stack_color(),
      stack_id = seed_stack_id(board)
    ))
  }

  stack <- lookup_stack(board, target)
  selected <- as.character(stack_blocks(stack))

  list(
    mode = "edit",
    # Augment the pool with the stack's own blocks so the user can
    # deselect them; otherwise they'd be invisible (not in available).
    pool = union(pool, selected),
    selected = selected,
    name = stack_name(stack) %||% "",
    color = attr(stack, "color", exact = TRUE) %||% default_stack_color(),
    stack_id = NULL
  )
}

lookup_stack <- function(board, target) {
  stacks <- board_stacks(board)
  if (!(target %in% names(stacks))) {
    blockr_abort(
      paste0(
        "No stack with id ", encodeString(target, quote = "'"),
        " on the board."
      ),
      class = "blockr_ui_stack_menu_unknown_target"
    )
  }
  stacks[[target]]
}

# ---- per-block metadata -----------------------------------------------

# Build one card meta per board block in `pool`. The card's
# `data-block-type` carries the board block id (repurposed). Visual
# metadata (category / icon / package / description) is pulled from
# the matching registry entry; the card label uses the block's
# user-visible name so "Dataset 1" / "Dataset 2" read correctly.
stack_menu_block_metas <- function(board, pool, selected) {
  if (length(pool) == 0L) return(list())

  blocks <- if (is.null(board)) list() else board_blocks(board)
  registry <- available_blocks()

  lapply(pool, function(blk_id) {
    blk <- blocks[[blk_id]]
    entry <- registry_entry_for(blk, registry)
    label <- block_name(blk) %||% blk_id
    list(
      type = blk_id,
      name = if (nzchar(label)) label else blk_id,
      description = entry_attr(entry, "description", ""),
      category = entry_attr(entry, "category", ""),
      icon = entry_attr(entry, "icon", ""),
      package = entry_attr(entry, "package", "local"),
      selected = blk_id %in% selected
    )
  })
}

registry_entry_for <- function(blk, registry) {
  if (is.null(blk) || length(registry) == 0L) return(NULL)
  uid <- registry_id_from_block(blk)
  if (length(uid)) registry[[uid]] else NULL
}

# ---- panel assembly ---------------------------------------------------

stack_menu_panel <- function(ns, metas, ctx) {
  groups <- category_groups(metas)
  is_edit <- identical(ctx$mode, "edit")

  tags$div(
    id = ns("commit"),
    class = "blockr-stack-menu",
    `data-mode` = ctx$mode,
    tags$h4(
      class = "blockr-stack-menu-section-header",
      "Stack blocks"
    ),
    tags$input(
      type = "search",
      class = "blockr-block-browser-search",
      placeholder = "Search...",
      `aria-label` = "Search blocks"
    ),
    tags$div(
      class = "blockr-block-browser-categories",
      lapply(names(groups), function(cat) {
        category_section(cat, groups[[cat]], stack_block_card)
      })
    ),
    tags$div(
      class = "blockr-block-browser-empty",
      "No blocks match your search."
    ),
    stack_menu_form(ns, ctx, is_edit),
    tags$button(
      type = "button",
      class = "blockr-stack-menu-confirm",
      if (is_edit) "Update stack" else "Create stack"
    )
  )
}

stack_block_card <- function(meta) {
  card_classes <- "blockr-block-browser-card blockr-stack-menu-card"
  if (isTRUE(meta$selected)) {
    card_classes <- paste(card_classes, "card-selected")
  }
  # The data-name haystack still includes the block id so search hits
  # "src" or "head1" too; data-description / data-package are kept on
  # the element so the shared block-browser search code works, but
  # they are NOT rendered as visible chrome.
  card_attrs <- list(
    class = card_classes,
    `data-block-type` = meta$type,
    `data-name` = paste(meta$name, meta$type),
    `data-description` = meta$description,
    `data-package` = meta$package,
    `data-category` = meta_category(meta)
  )
  if (isTRUE(meta$selected)) {
    card_attrs$`data-selected` <- "true"
  }

  do.call(
    tags$div,
    c(
      card_attrs,
      list(
        tags$div(
          class = "blockr-block-browser-card-header",
          tags$span(
            class = "blockr-block-browser-card-icon",
            if (nzchar(meta$icon)) htmltools::HTML(meta$icon) else NULL
          ),
          tags$div(
            class = "blockr-stack-menu-card-titles",
            tags$span(
              class = "blockr-block-browser-card-name",
              meta$name
            ),
            tags$span(
              class = "blockr-stack-menu-card-id",
              paste0("id: ", meta$type)
            )
          )
        )
      )
    )
  )
}

# Panel-level form. All three fields (name, colour, id) are visible
# top-level in create mode - the sidebar has room for them and the
# auto-seeded id is still useful chrome (users can rename it). Edit
# mode omits the id input entirely (the id is immutable once a stack
# is created). The form is preceded by a small uppercase section
# header (matching the category labels above) and rendered inside a
# subtly-tinted panel so the configuration block reads as distinct
# from the picker.
stack_menu_form <- function(ns, ctx, is_edit) {
  tagList(
    tags$h4(
      class = "blockr-stack-menu-section-header",
      "Stack settings"
    ),
    tags$div(
      class = "blockr-stack-menu-form",
      text_field_tag(ns, "stack_name", "Stack name", ctx$name, "My stack"),
      color_field_tag(ns, ctx$color),
      if (!is_edit) {
        text_field_tag(ns, "stack_id", "Stack id", ctx$stack_id, "stack_id")
      }
    )
  )
}

text_field_tag <- function(ns, key, label, value, placeholder) {
  tags$div(
    class = "blockr-stack-menu-field",
    tags$label(`for` = ns(key), label),
    tags$input(
      id = ns(key),
      type = "text",
      value = value %||% "",
      placeholder = placeholder
    )
  )
}

color_field_tag <- function(ns, value) {
  hex <- value %||% default_stack_color()
  tags$div(
    class = "blockr-stack-menu-field blockr-stack-menu-color",
    tags$label(`for` = ns("stack_color"), "Stack color"),
    tags$div(
      class = "blockr-stack-menu-color-preview",
      tags$span(class = "blockr-stack-menu-color-swatch"),
      tags$input(
        id = ns("stack_color"),
        type = "text",
        class = "blockr-stack-menu-hex",
        value = hex,
        spellcheck = "false",
        autocomplete = "off",
        `aria-label` = "Hex colour value"
      )
    ),
    tags$input(
      type = "range",
      class = "blockr-stack-menu-hue",
      min = "0",
      max = "360",
      step = "1",
      value = "0",
      `aria-label` = "Hue"
    ),
    tags$input(
      type = "range",
      class = "blockr-stack-menu-lightness",
      min = "20",
      max = "85",
      step = "1",
      value = "60",
      `aria-label` = "Lightness"
    )
  )
}

# ---- small helpers ----------------------------------------------------

# Seed colour for the create flow. The dock can override `stack_color`
# via `updateTextInput()` after the panel is shown if it wants a
# smarter starting point.
default_stack_color <- function() {
  "#66c2a5"
}

seed_stack_id <- function(board) {
  seed_ids(safe_board_ids(board, board_stack_ids), 1L)
}

# A unique, human-readable default name for the create flow. Uses the
# same `rand_names()` generator as the seeded id (adjective_animal, e.g.
# "savoury_midge"), kept distinct from the board's existing stack names
# so it reads as a real, submittable value out of the box.
seed_stack_name <- function(board) {
  existing <- if (is.null(board)) {
    character()
  } else {
    vapply(
      board_stacks(board),
      function(s) stack_name(s) %||% "",
      character(1L),
      USE.NAMES = FALSE
    )
  }
  seed_ids(existing, 1L)
}

# Block ids on the board that are not currently a member of any stack.
# `available_stack_blocks()` computes exactly this when seeded
# with the board's block ids (its default seeds with stack ids instead).
stack_eligible_blocks <- function(board) {
  if (is.null(board)) return(character())
  available_stack_blocks(
    board,
    blocks = board_block_ids(board)
  )
}
