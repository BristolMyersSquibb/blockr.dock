block_browser_ui <- function(id, board = NULL, target = NULL) {
  stopifnot(is.character(id), length(id) == 1L, nzchar(id))
  stopifnot(is.null(target) || inherits(target, "bb_target"))

  ns <- NS(id)
  mode <- target_mode(target)
  # The card list is a pure function of the registry: no board-seeded
  # default ids are baked in (the server resolves a unique id at commit),
  # so the add-flow markup is board-independent and can be rendered once.
  # `board` is used only by resolve_target() for the prepend port picker
  # and the append / prepend context subtitle.
  metas <- browser_block_metas(mode)
  tgt <- resolve_target(board, target)

  htmltools::attachDependencies(
    browser_panel(ns, metas, mode, tgt),
    list(block_browser_dep())
  )
}

append_to <- function(block_id = NULL) {
  new_bb_target("append", block_id)
}

prepend_to <- function(block_id = NULL) {
  new_bb_target("prepend", block_id)
}

block_browser_server <- function(id, board = NULL, target = NULL) {
  stopifnot(is.character(id), length(id) == 1L, nzchar(id))

  board_fn <- as_arg_reactive(board)
  target_fn <- as_arg_reactive(target)

  moduleServer(
    id,
    function(input, output, session) {
      # `input$commit` is the binding's value; the `nonce` it carries
      # makes every add a fresh event, so eventReactive fires once per
      # add. When a `board` reactive is supplied the module validates the
      # committed ids (a non-empty duplicate notifies and `req()`s out),
      # then returns a ready-to-apply value: a `blocks` object for the add
      # flow, or `list(blocks, links)` for append / prepend - parity with
      # link_menu_server() / stack_menu_server().
      eventReactive(
        input$commit,
        {
          spec <- input$commit
          spec[["nonce"]] <- NULL
          brd <- board_fn()
          tgt <- target_fn()
          if (is.reactive(board)) {
            validate_block_spec(spec, brd, tgt, session)
          }
          block_commit_value(spec, brd, tgt)
        },
        ignoreNULL = TRUE
      )
    }
  )
}

# Turn the committed spec into a ready-to-apply value. The add flow
# yields a `blocks` object (one id-keyed block); append / prepend yield
# `list(blocks, links)` with the link's input port resolved. Block
# construction and port resolution use blockr.core primitives only (no
# blockr.dock dependency), mirroring link_menu's commit value and the
# dock's former build_block_from_spec + block_input_select assembly.
block_commit_value <- function(spec, board, target) {
  blk <- build_browser_block(spec)
  blk_id <- resolve_browser_id(spec$id, board, board_block_ids)
  blocks <- as_blocks(set_names(list(blk), blk_id))

  if (target_mode(target) == "add") {
    return(blocks)
  }

  list(
    blocks = blocks,
    links = block_commit_link(spec, board, target, blk, blk_id)
  )
}

# Build the link that wires the new block to its target. Append: the new
# block (blk_id) receives from the source (target$id), so the port is a
# free slot on the NEW block. Prepend: the new block feeds INTO the
# target, so the port is a free slot on the TARGET block.
block_commit_link <- function(spec, board, target, blk, blk_id) {
  links <- safe_board_links(board)
  link_id <- resolve_browser_id(
    spec$link_id, board, board_link_ids
  )

  if (target$mode == "append") {
    input <- spec$block_input
    if (is.null(input) || !nzchar(input)) {
      input <- resolve_free_input(blk, blk_id, links)
    }
    lnk <- new_link(from = target$id, to = blk_id, input = input)
  } else {
    tgt_blk <- board_block(board, target$id)
    input <- spec$target_input
    if (is.null(input) || !nzchar(input)) {
      input <- resolve_free_input(tgt_blk, target$id, links)
    }
    lnk <- new_link(from = blk_id, to = target$id, input = input)
  }

  as_links(set_names(list(lnk), link_id))
}

# Construct the block instance. The user's title (when supplied) is the
# block name; otherwise we let blockr.core derive the default name. The
# id - not the name - is what must be board-unique, and that is handled
# separately by resolve_browser_id().
build_browser_block <- function(spec) {
  if (!is.null(spec$title) && nzchar(spec$title)) {
    create_block(spec$type, block_name = spec$title)
  } else {
    create_block(spec$type)
  }
}

# An explicit, board-unique id is kept as-is; otherwise (empty field, or
# a collision) a fresh unique id is generated against the board.
resolve_browser_id <- function(spec_id, board, getter) {
  existing <- safe_board_ids(board, getter)
  if (is_new_id(spec_id, existing)) {
    spec_id
  } else {
    rand_names(old_names = existing, n = 1L)
  }
}

# Reject a non-empty committed id that collides with the board (block id
# always; link id for append / prepend). An empty id is valid - it means
# "assign one for me", resolved in block_commit_value().
validate_block_spec <- function(spec, board, target, session) {
  reject_collision(
    spec$id, safe_board_ids(board, board_block_ids),
    "block", session
  )
  if (target_mode(target) %in% c("append", "prepend")) {
    reject_collision(
      spec$link_id, safe_board_ids(board, board_link_ids),
      "link", session
    )
  }
  invisible(TRUE)
}

reject_collision <- function(id, existing, what, session) {
  if (!is.null(id) && nzchar(id) && id %in% existing) {
    notify(
      paste0("Please choose a valid ", what, " ID."),
      type = "warning", session = session
    )
    req(FALSE)
  }
  invisible(TRUE)
}

# The board's links, or an empty links object for a NULL board.
safe_board_links <- function(board) {
  if (is.null(board)) return(links())
  tryCatch(
    board_links(board),
    error = function(e) links()
  )
}

block_browser_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-block-browser",
    version = utils::packageVersion("blockr.dock"),
    package = "blockr.dock",
    src = "assets",
    stylesheet = "css/blockr-block-browser.css",
    script = "js/blockr-block-browser.js",
    all_files = FALSE
  )
}

# ---- target descriptor -------------------------------------------------

new_bb_target <- function(mode, block_id = NULL) {
  stopifnot(
    is.null(block_id) ||
      (is.character(block_id) && length(block_id) == 1L && nzchar(block_id))
  )
  structure(
    list(mode = mode, id = block_id),
    class = c(paste0("bb_target_", mode), "bb_target")
  )
}

# The resulting flow: "add" when there's no target, else the target's
# own mode. The rest of the module branches on this string.
target_mode <- function(target) {
  if (is.null(target)) "add" else target$mode
}

# ---- panel assembly ----------------------------------------------------

# Build the per-block metadata list from the registry. Independent of
# board state: default block / link ids are no longer seeded here (the
# server resolves a unique id at commit), so the rendered markup is a
# pure function of the registry. Block-input slots are still computed
# for the append flow (each `safe_block_inputs()` instantiates a block,
# so we skip the work otherwise) to drive the linkable-block filter and
# the in-card port picker.
browser_block_metas <- function(mode) {
  registry <- available_blocks()
  metas <- lapply(seq_along(registry), function(i) {
    entry <- registry[[i]]
    # `type` is the registry uid (e.g. "dataset_block"), so consumers
    # can do `create_block(spec$type, ...)` rather than
    # rely on the constructor's function name being importable.
    list(
      type = names(registry)[[i]],
      name = entry_attr(entry, "name", names(registry)[[i]]),
      description = entry_attr(entry, "description", ""),
      category = entry_attr(entry, "category", ""),
      icon = entry_attr(entry, "icon", ""),
      package = entry_attr(entry, "package", "local"),
      ctor = entry
    )
  })

  need_inputs <- mode == "append"

  # For append, the new block has to receive a link from the source, so
  # candidates need either a named input slot or variadic arity (`NA`)
  # which accepts arbitrary fresh slots. Source-only blocks (arity 0,
  # e.g. dataset_block) can't be appended and are filtered out.
  # Variadic blocks (e.g. rbind_block) return character(0) from
  # block_inputs() but DO accept links - the server generates a fresh
  # slot name. Prepend / add are unfiltered.
  if (need_inputs) {
    for (i in seq_along(metas)) {
      metas[[i]]$inputs <- safe_block_inputs(metas[[i]]$ctor)
      metas[[i]]$variadic <- safe_block_variadic(metas[[i]]$ctor)
    }
    metas <- Filter(
      function(m) length(m$inputs) > 0L || isTRUE(m$variadic),
      metas
    )
  } else {
    for (i in seq_along(metas)) {
      metas[[i]]$inputs <- character()
      metas[[i]]$variadic <- FALSE
    }
  }

  metas
}

# Everything the panel needs to know about the trigger block, in one
# place: the context subtitle, the target's input slot names, and the
# `data-target-arity` attribute (prepend only).
resolve_target <- function(board, target) {
  # `target = NULL` is the add flow; a target with a `NULL` id is a
  # source-less append / prepend descriptor (the source is supplied
  # server-side at commit), used to pre-render the panel once. Either
  # way there is no concrete block to derive a subtitle / port picker
  # from, so the panel is rendered context-free (the consumer supplies
  # the "Append from X" context via the sidebar title).
  if (is.null(target) || is.null(target$id)) {
    return(list(subtitle = NULL, inputs = character(), attrs = list()))
  }

  blk <- board_block(board, target$id)
  trigger_name <- if (!is.null(blk)) {
    nm <- tryCatch(block_name(blk), error = function(e) NULL)
    if (is.null(nm) || !nzchar(nm)) target$id else nm
  } else {
    target$id
  }
  verb <- if (target$mode == "append") "Append from" else "Prepend to"
  subtitle <- tags$p(
    class = "blockr-block-browser-context",
    verb, " ", tags$strong(trigger_name)
  )

  inputs <- character()
  attrs <- list()
  if (target$mode == "prepend" && !is.null(blk)) {
    # Filter out slots already taken by incoming links to the target,
    # so the user is never offered a port they can't actually use.
    inputs <- setdiff(
      block_inputs(blk),
      taken_inputs(board, target$id)
    )
    arity <- block_arity(blk)
    if (is.na(arity)) {
      attrs <- list(`data-target-arity` = "inf")
    } else if (is.numeric(arity) && length(arity) == 1L) {
      attrs <- list(`data-target-arity` = as.character(arity))
    }
  }

  list(subtitle = subtitle, inputs = inputs, attrs = attrs)
}

browser_panel <- function(ns, metas, mode, tgt) {
  groups <- category_groups(metas)

  # The root element IS the Shiny input: its `id` is the commit input id
  # the `blockr.dock.blockBrowser` InputBinding reports against, read by
  # `block_browser_server()` as `input$commit`.
  root_attrs <- c(
    list(
      id = ns("commit"),
      class = "blockr-block-browser",
      `data-mode` = mode
    ),
    tgt$attrs
  )

  do.call(
    tags$div,
    c(
      root_attrs,
      list(
        tgt$subtitle,
        tags$input(
          type = "search",
          class = "blockr-block-browser-search",
          placeholder = "Search...",
          `aria-label` = "Search blocks"
        ),
        tags$div(
          class = "blockr-block-browser-categories",
          lapply(names(groups), function(cat) {
            category_section(
              cat, groups[[cat]],
              function(m) browser_block_card(m, ns, mode, tgt$inputs)
            )
          })
        ),
        tags$div(
          class = "blockr-block-browser-empty",
          "No blocks match your search."
        )
      )
    )
  )
}

# Shared category block for both panels: the wrapper chrome is identical;
# only the per-entry card differs, so callers pass a `card_fn` that maps
# one meta to its card tag.
category_section <- function(category, entries, card_fn) {
  tags$div(
    class = "blockr-block-browser-category",
    `data-category` = category,
    tags$h3(category),
    tags$div(
      class = "blockr-block-browser-cards",
      lapply(entries, card_fn)
    )
  )
}

browser_block_card <- function(meta, ns, mode, target_inputs) {
  tags$div(
    class = "blockr-block-browser-card",
    `data-block-type` = meta$type,
    `data-name` = meta$name,
    # `data-description` feeds the search index and seeds the `title`
    # restore on collapse. The description's real home is the expanded
    # card's band (keyboard/touch reachable, styled); `title` is only an
    # additive mouse-hover preview while collapsed - the JS toggle drops
    # it on expand so it never doubles up with the visible band.
    `data-description` = meta$description,
    title = if (nzchar(meta$description)) meta$description else NULL,
    `data-package` = meta$package,
    `data-category` = meta_category(meta),
    # Compact row: the icon tile centres against a single title row
    # (icon · name · package badge · chevron). The description band and
    # form below stay hidden until the card is expanded, keeping the
    # resting list dense (the finalized "compact" density of the spec).
    tags$div(
      class = "blockr-block-browser-card-header",
      tags$span(
        class = "blockr-block-browser-card-icon",
        if (nzchar(meta$icon)) htmltools::HTML(meta$icon) else NULL
      ),
      tags$div(
        class = "blockr-block-browser-card-body",
        tags$div(
          class = "blockr-block-browser-card-titles",
          tags$span(class = "blockr-block-browser-card-name", meta$name),
          tags$span(
            class = "blockr-block-browser-card-package", meta$package
          ),
          tags$button(
            type = "button",
            class = "blockr-block-browser-card-chevron",
            `aria-label` = "Configure before adding",
            chevron_icon()
          )
        )
      )
    ),
    # Description band: a full-width sibling of the header (not indented,
    # per spec), revealed only when the card expands into its elevated
    # panel. Dropped from the DOM when there is no description.
    if (nzchar(meta$description)) {
      tags$p(
        class = "blockr-block-browser-card-descr-band",
        meta$description
      )
    },
    card_advanced(meta, ns, mode, target_inputs)
  )
}

# Render only the fields the flow needs (mode is fixed per render, so
# there is no need to render every field and hide the inapplicable ones
# via CSS). `htmltools` drops the NULL children for the omitted fields.
card_advanced <- function(meta, ns, mode, target_inputs) {
  field_id <- function(suffix) ns(paste0(meta$type, "_", suffix))
  show_link <- mode %in% c("append", "prepend")
  # Only ask the user to pick a slot when there is an actual choice
  # (>= 2 slots). With a single slot the new-block input port is
  # forced; the dock falls back to `block_inputs(blk)[1L]`.
  show_block_input <- mode == "append" && length(meta$inputs) > 1L
  show_target_input <- mode == "prepend" && length(target_inputs) > 1L
  add_label <- switch(mode,
    add = "Add",
    append = "Append",
    prepend = "Prepend"
  )

  tags$div(
    class = "blockr-block-browser-card-advanced",
    # Empty default: the server resolves a unique id at commit (avoiding
    # the board's ids). An explicit value here overrides that.
    field_text(
      class_suffix = "id",
      id = field_id("id"),
      label = "Block ID",
      value = "",
      placeholder = "auto"
    ),
    field_text(
      class_suffix = "title",
      id = field_id("title"),
      label = "Block title",
      value = "",
      placeholder = meta$name
    ),
    if (show_link) {
      field_text(
        class_suffix = "link-id",
        id = field_id("link_id"),
        label = "Link ID",
        value = "",
        placeholder = "auto"
      )
    },
    if (show_block_input) {
      field_select(
        class_suffix = "block-input",
        id = field_id("block_input"),
        label = "New block input port",
        options = meta$inputs
      )
    },
    if (show_target_input) {
      field_select(
        class_suffix = "target-input",
        id = field_id("target_input"),
        label = "Target input port",
        options = target_inputs
      )
    },
    tags$button(
      type = "button",
      class = "blockr-block-browser-card-add",
      plus_icon(),
      add_label
    )
  )
}

field_wrapper <- function(class_suffix, id, label, control) {
  tags$div(
    class = paste0(
      "blockr-block-browser-field blockr-block-browser-field-", class_suffix
    ),
    tags$label(`for` = id, label),
    control
  )
}

field_text <- function(class_suffix, id, label, value, placeholder = NULL) {
  field_wrapper(
    class_suffix, id, label,
    tags$input(
      type = "text",
      id = id,
      value = value,
      placeholder = placeholder
    )
  )
}

field_select <- function(class_suffix, id, label, options) {
  field_wrapper(
    class_suffix, id, label,
    tags$select(
      id = id,
      lapply(options, function(opt) {
        tags$option(value = opt, opt)
      })
    )
  )
}

chevron_icon <- function() {
  tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    viewBox = "0 0 16 16",
    fill = "currentColor",
    `aria-hidden` = "true",
    tags$path(
      d = paste0(
        "M1.646 4.646a.5.5 0 0 1 .708 0L8 10.293l5.646-5.647a.5.5 0 0 1",
        " .708.708l-6 6a.5.5 0 0 1-.708 0l-6-6a.5.5 0 0 1 0-.708z"
      )
    )
  )
}

# Plus glyph for the ghost-outline add button (spec: 13px stroked icon
# left of the label). Stroked rather than filled to match the button's
# light, outline weight.
plus_icon <- function() {
  tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    viewBox = "0 0 24 24",
    fill = "none",
    stroke = "currentColor",
    `stroke-width` = "2",
    `stroke-linecap` = "round",
    `stroke-linejoin` = "round",
    `aria-hidden` = "true",
    tags$path(d = "M12 5v14M5 12h14")
  )
}

# ---- small helpers -----------------------------------------------------

entry_attr <- function(entry, key, default) {
  val <- attr(entry, key, exact = TRUE)
  if (is.null(val) || (is.character(val) && !nzchar(val))) default else val
}

meta_category <- function(m) {
  if (nzchar(m$category)) m$category else "Uncategorized"
}

# Split metas into category buckets in display order, "Uncategorized"
# last. Returns a named list of meta groups, keyed and ordered by
# category; both the block-browser and stack-menu panels render from it.
category_groups <- function(metas) {
  cats <- vapply(metas, meta_category, character(1L))
  order <- unique(cats)
  order <- c(
    setdiff(order, "Uncategorized"),
    if ("Uncategorized" %in% order) "Uncategorized" else character()
  )
  split(metas, cats)[order]
}

# n unique ids avoiding `existing` (and each other); empty for n == 0.
seed_ids <- function(existing, n) {
  if (n > 0L) {
    rand_names(old_names = existing, n = n)
  } else {
    character()
  }
}

# Construct one block instance with no args to read its input slot names.
# Returns character(0) on any error.
safe_block_inputs <- function(ctor) {
  tryCatch(
    {
      blk <- ctor()
      block_inputs(blk)
    },
    error = function(e) character()
  )
}

# TRUE when the block has variadic arity (NA) - accepts an arbitrary
# number of input links with fresh slot names. False otherwise (arity
# 0 or finite). Used to keep variadic blocks as valid append targets
# even though `block_inputs()` returns character(0).
safe_block_variadic <- function(ctor) {
  tryCatch(
    is.na(block_arity(ctor())),
    error = function(e) FALSE
  )
}

safe_board_ids <- function(board, getter) {
  if (is.null(board)) return(character())
  tryCatch(getter(board), error = function(e) character())
}

board_block <- function(board, id) {
  if (is.null(board)) return(NULL)
  blocks <- tryCatch(board_blocks(board), error = function(e) NULL)
  if (is.null(blocks)) return(NULL)
  blocks[[id]]
}

# Input slots of `block_id` that are already wired by an incoming link
# on `board`. Used to filter the `target_input` picker for prepend so a
# taken slot is never offered.
taken_inputs <- function(board, block_id) {
  if (is.null(board)) return(character())
  links_df <- tryCatch(
    as.data.frame(board_links(board)),
    error = function(e) NULL
  )
  if (is.null(links_df) || !nrow(links_df) ||
        !all(c("to", "input") %in% names(links_df))) {
    return(character())
  }
  as.character(links_df$input[links_df$to == block_id])
}
