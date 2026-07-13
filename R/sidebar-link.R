link_menu_ui <- function(id, board, anchor) {
  stopifnot(is.character(id), length(id) == 1L, nzchar(id))

  # link_eligible_pools() validates `anchor`; the same check covers
  # link_menu_ui() so we don't double-validate.
  pools <- link_eligible_pools(board, anchor)

  ns <- NS(id)
  htmltools::attachDependencies(
    link_menu_panel(ns, board, anchor, pools),
    list(block_browser_dep(), link_menu_dep())
  )
}

link_menu_server <- function(id, board = NULL, anchor = NULL) {
  stopifnot(is.character(id), length(id) == 1L, nzchar(id))

  board_fn <- as_arg_reactive(board)
  anchor_fn <- as_arg_reactive(anchor)

  moduleServer(
    id,
    function(input, output, session) {
      # Live board sync: when a board reactive is supplied, refresh the
      # open menu in place on every board change by pushing a `menu:sync`
      # diff to our own binding. Self-scoped (bare "commit" - the module
      # session namespaces it), so it no-ops when the panel isn't mounted.
      if (is.reactive(board)) {
        observeEvent(
          board_fn(),
          {
            anc <- anchor_fn()
            brd <- board_fn()
            # The anchor block may have been removed; nothing to sync to.
            if (!is.null(anc) && nzchar(anc) &&
                  anc %in% board_block_ids(brd)) {
              session$sendInputMessage(
                "commit", link_sync_payload(brd, anc, session$ns)
              )
            }
          },
          ignoreInit = TRUE
        )
      }

      eventReactive(
        input$commit,
        {
          spec <- input$commit
          spec[["nonce"]] <- NULL
          # The menu owns validation when the consumer opts in with a
          # board reactive: a duplicate / empty link id notifies and
          # `req()`s out, so the committed reactive never fires invalid.
          if (is.reactive(board)) {
            validate_link_spec(spec, board_fn(), session)
          }
          link_commit_value(spec, board_fn())
        },
        ignoreNULL = TRUE
      )
    }
  )
}

# Turn the committed spec into a ready-to-apply `blockr.core` links
# object: one link keyed by its id. The target input slot is resolved
# here, since the menu only renders a port picker for finite-arity
# targets with more than one free slot - so `block_input` arrives NULL
# for arity-1 and variadic targets. The consumer adds the result as-is.
link_commit_value <- function(spec, board) {
  input <- spec$block_input
  if (is.null(input) || !nzchar(input)) {
    input <- resolve_free_input(
      board_blocks(board)[[spec$target]],
      spec$target,
      board_links(board)
    )
  }
  lnk <- new_link(
    from = spec$source, to = spec$target, input = input
  )
  as_links(set_names(list(lnk), spec$link_id))
}

# Pick the target's input slot for a new link, mirroring blockr.dock's
# `block_input_select(mode = "inputs")` with only blockr.core primitives:
# the first free named input, or - for a variadic target - a freshly
# generated numeric slot.
resolve_free_input <- function(block, block_id, links) {
  curr <- links[links$to == block_id]$input
  free <- setdiff(block_inputs(block), curr)

  if (is.na(block_arity(block))) {
    num <- suppressWarnings(as.integer(curr))
    num <- num[!is.na(num)]
    slot <- if (!length(num)) {
      "1"
    } else {
      mis <- setdiff(seq_len(max(num)), num)
      as.character(if (length(mis)) min(mis) else max(num) + 1L)
    }
    free <- c(free, slot)
  }

  free[1L]
}

# Reject an empty / duplicate link id, mirroring the stack menu's
# self-validation. The eligible-pool logic already guarantees a valid
# source / target pair, so only the id needs checking here.
validate_link_spec <- function(spec, board, session) {
  existing <- board_link_ids(board)
  if (!is_new_id(spec$link_id, existing)) {
    notify(
      "Please choose a valid link ID.", type = "warning", session = session
    )
    req(FALSE)
  }
  invisible(TRUE)
}

# Build the `menu:sync` payload: the full set of cards that SHOULD exist
# for the current board / anchor (both directions), each with its
# rendered markup tagged by direction, plus the per-target free input
# map and a fresh link-id seed. The client inserts / removes / retunes
# cards in place. Card markup is produced by the same builder used for
# the initial render.
link_sync_payload <- function(board, anchor, ns) {
  pools <- link_eligible_pools(board, anchor)
  registry <- available_blocks()
  blocks <- board_blocks(board)

  cards <- c(
    link_sync_cards(
      blocks, registry, pools$incoming, anchor, "incoming",
      pools$free_inputs, ns, board
    ),
    link_sync_cards(
      blocks, registry, pools$outgoing, anchor, "outgoing",
      pools$free_inputs, ns, board
    )
  )

  list(
    type = "menu:sync",
    cards = cards,
    free_inputs = pools$free_inputs,
    link_id_seed = seed_link_id(board)
  )
}

link_sync_cards <- function(blocks, registry, pool, anchor, direction,
                            free_inputs, ns, board) {
  lapply(pool, function(blk_id) {
    meta <- link_card_meta(
      blk_id, blocks, registry, anchor, direction, free_inputs
    )
    list(
      id = blk_id,
      direction = direction,
      html = as.character(link_block_card(meta, ns, board, direction))
    )
  })
}

link_menu_dep <- function() {
  htmltools::htmlDependency(
    name = "sidebar-link",
    version = utils::packageVersion("blockr.dock"),
    package = "blockr.dock",
    src = "assets",
    stylesheet = "css/sidebar-link.css",
    script = "js/sidebar-link.js",
    all_files = FALSE
  )
}

link_eligible_pools <- function(board, anchor) {
  stopifnot(is.character(anchor), length(anchor) == 1L, nzchar(anchor))
  validate_anchor(board, anchor)

  blocks <- board_blocks(board)
  links_df <- as.data.frame(board_links(board))

  outgoing <- compute_outgoing_targets(blocks, anchor, links_df)
  incoming <- compute_incoming_sources(blocks, anchor, links_df)

  # Per-target free named inputs, keyed by target block id. OUTGOING
  # cards target the card itself; INCOMING cards all target the
  # anchor. The pool-update push reads this map to refresh each
  # visible card's block-input <select> options after a commit.
  targets <- unique(c(outgoing, if (length(incoming)) anchor))
  free <- set_names(
    lapply(targets, function(id) free_named_inputs(blocks[[id]], id, links_df)),
    targets
  )
  list(outgoing = outgoing, incoming = incoming, free_inputs = free)
}

# ---- eligibility -------------------------------------------------------

validate_anchor <- function(board, anchor) {
  ids <- if (is.null(board)) {
    character()
  } else {
    board_block_ids(board)
  }
  if (!(anchor %in% ids)) {
    blockr_abort(
      paste0(
        "No block with id ", encodeString(anchor, quote = "'"),
        " on the board."
      ),
      class = "blockr_dock_link_menu_unknown_anchor"
    )
  }
  invisible(anchor)
}

# Every non-anchor block with at least one free input port or variadic
# arity, EXCLUDING candidates whose addition as `anchor -> candidate`
# would close a cycle (candidate already reaches anchor through the
# existing link graph). blockr.core has no exported eligibility
# helper that fits, so we reimplement it locally on top of
# `block_inputs()`, `block_arity()`, and the links data frame.
compute_outgoing_targets <- function(blocks, anchor, links_df) {
  cycle_seed <- ancestors_of(anchor, links_df)
  ids <- setdiff(names(blocks), c(anchor, cycle_seed))
  keep <- vapply(
    ids,
    function(id) has_input_capacity(blocks[[id]], id, links_df),
    logical(1L)
  )
  ids[keep]
}

# Every non-anchor block, but ONLY when the anchor itself has a free
# input (or is variadic). EXCLUDES candidates that anchor already
# reaches (adding `candidate -> anchor` would close a cycle).
compute_incoming_sources <- function(blocks, anchor, links_df) {
  if (!has_input_capacity(blocks[[anchor]], anchor, links_df)) {
    return(character())
  }
  cycle_seed <- descendants_of(anchor, links_df)
  setdiff(names(blocks), c(anchor, cycle_seed))
}

# Block ids that reach `start` by following outgoing links (i.e. that
# already have a directed path INTO `start`). Adding `start -> X`
# would create a cycle iff `X` is in this set.
ancestors_of <- function(start, links_df) {
  walk_reachable(start, links_df, from_col = "to", to_col = "from")
}

# Block ids that `start` reaches by following outgoing links. Adding
# `X -> start` would create a cycle iff `X` is in this set.
descendants_of <- function(start, links_df) {
  walk_reachable(start, links_df, from_col = "from", to_col = "to")
}

walk_reachable <- function(start, links_df, from_col, to_col) {
  if (is.null(links_df) || !nrow(links_df) ||
        !all(c(from_col, to_col) %in% names(links_df))) {
    return(character())
  }
  from_vec <- as.character(links_df[[from_col]])
  to_vec <- as.character(links_df[[to_col]])
  visited <- character()
  frontier <- start
  while (length(frontier)) {
    next_nodes <- unique(to_vec[from_vec %in% frontier])
    next_nodes <- setdiff(next_nodes, c(visited, start))
    visited <- c(visited, next_nodes)
    frontier <- next_nodes
  }
  visited
}

# Can `blk` accept one more incoming link? TRUE for variadic; TRUE
# when there's at least one named input not already wired; FALSE
# otherwise.
has_input_capacity <- function(blk, blk_id, links_df) {
  if (is.null(blk)) return(FALSE)
  if (is.na(block_arity(blk))) return(TRUE)
  length(free_named_inputs(blk, blk_id, links_df)) > 0L
}

# Free named inputs = block_inputs(blk) minus the ports already wired
# by incoming links to `blk_id`. Variadic blocks return character() -
# they accept a fresh slot which the consumer generates server-side,
# so no port picker is shown for them.
free_named_inputs <- function(blk, blk_id, links_df) {
  if (is.null(blk)) return(character())
  if (is.na(block_arity(blk))) return(character())
  used <- if (is.null(links_df) || !nrow(links_df) ||
                !all(c("to", "input") %in% names(links_df))) {
    character()
  } else {
    as.character(links_df$input[links_df$to == blk_id])
  }
  setdiff(block_inputs(blk), used)
}

# ---- panel assembly ----------------------------------------------------

link_menu_panel <- function(ns, board, anchor, pools) {
  is_empty <- length(pools$outgoing) == 0L && length(pools$incoming) == 0L

  root_attrs <- list(
    id = ns("commit"),
    class = paste(
      "blockr-link-menu",
      if (is_empty) "is-empty" else ""
    ),
    `data-anchor` = anchor
  )

  do.call(
    tags$div,
    c(
      root_attrs,
      list(
        tags$input(
          type = "search",
          class = "blockr-block-browser-search",
          placeholder = "Search...",
          `aria-label` = "Search blocks"
        ),
        tags$div(
          class = "blockr-link-menu-directions",
          # Order follows the visual left-to-right data flow: sources
          # (INPUT FROM) feed the anchor, the anchor feeds targets
          # (OUTPUT TO). Showing INPUT FROM first reads more naturally.
          if (length(pools$incoming) > 0L) {
            direction_section(
              ns, board, anchor, pools$incoming,
              direction = "incoming", header = "Input from",
              free_inputs = pools$free_inputs
            )
          },
          if (length(pools$outgoing) > 0L) {
            direction_section(
              ns, board, anchor, pools$outgoing,
              direction = "outgoing", header = "Output to",
              free_inputs = pools$free_inputs
            )
          }
        ),
        tags$div(
          class = "blockr-block-browser-empty",
          paste0(
            "This block can't be linked: no other blocks have free ",
            "inputs, and the anchor itself has no free inputs either."
          )
        )
      )
    )
  )
}

# One card's metadata. Shared by the initial render (`direction_section`)
# and the live `menu:sync` payload (`link_sync_cards`) so both produce
# identical card markup.
link_card_meta <- function(blk_id, blocks, registry, anchor, direction,
                           free_inputs) {
  blk <- blocks[[blk_id]]
  entry <- registry_entry_for(blk, registry)
  label <- block_name(blk) %||% blk_id
  target_id <- if (direction == "outgoing") blk_id else anchor
  list(
    id = blk_id,
    name = if (nzchar(label)) label else blk_id,
    category = entry_attr(entry, "category", ""),
    icon = entry_attr(entry, "icon", ""),
    package = entry_attr(entry, "package", "local"),
    description = entry_attr(entry, "description", ""),
    target_id = target_id,
    target_inputs = free_inputs[[target_id]] %||% character()
  )
}

direction_section <- function(ns, board, anchor, pool, direction, header,
                              free_inputs) {
  registry <- available_blocks()
  blocks <- board_blocks(board)

  metas <- lapply(pool, function(blk_id) {
    link_card_meta(blk_id, blocks, registry, anchor, direction, free_inputs)
  })

  groups <- category_groups(metas)

  tags$div(
    class = "blockr-link-menu-direction",
    `data-direction` = direction,
    tags$h4(class = "blockr-link-menu-section-header", header),
    tags$div(
      class = "blockr-block-browser-categories",
      lapply(names(groups), function(cat) {
        category_section(
          cat, groups[[cat]],
          function(m) link_block_card(m, ns, board, direction)
        )
      })
    )
  )
}

link_block_card <- function(meta, ns, board, direction) {
  tags$div(
    class = "blockr-block-browser-card blockr-link-menu-card",
    `data-block-type` = meta$id,
    `data-direction` = direction,
    `data-name` = paste(meta$name, meta$id),
    `data-description` = meta$description,
    `data-package` = meta$package,
    `data-category` = meta_category(meta),
    tags$div(
      class = "blockr-block-browser-card-header",
      tags$span(
        class = "blockr-block-browser-card-icon",
        if (nzchar(meta$icon)) htmltools::HTML(meta$icon) else NULL
      ),
      tags$div(
        class = "blockr-link-menu-card-titles",
        tags$span(
          class = "blockr-block-browser-card-name",
          meta$name
        ),
        tags$span(
          class = "blockr-link-menu-card-id",
          paste0("id: ", meta$id)
        )
      ),
      tags$button(
        type = "button",
        class = "blockr-block-browser-card-chevron",
        `aria-label` = "Configure before adding",
        chevron_icon()
      )
    ),
    link_card_advanced(meta, ns, board)
  )
}

# Per-card advanced form: link_id always; block_input only when the
# target end has finite arity > 1 free slots.
link_card_advanced <- function(meta, ns, board) {
  card_ns <- function(suffix) ns(paste0("card_", meta$id, "_", suffix))
  link_id_default <- seed_link_id(board)
  show_port <- length(meta$target_inputs) > 1L

  tags$div(
    class = "blockr-block-browser-card-advanced",
    field_text(
      class_suffix = "link-id",
      id = card_ns("link_id"),
      label = "Link ID",
      value = link_id_default
    ),
    if (show_port) {
      field_select(
        class_suffix = "block-input",
        id = card_ns("block_input"),
        label = "Block input port",
        options = meta$target_inputs
      )
    },
    tags$button(
      type = "button",
      class = "blockr-block-browser-card-add",
      "Add link"
    )
  )
}

# ---- helpers -----------------------------------------------------------

# `field_text()`, `field_select()`, and `chevron_icon()` are defined
# in `block-browser.R` and reused here via package scope. The link
# menu shares the `.blockr-block-browser-field-*` class space with
# the block browser, so the markup is identical.

seed_link_id <- function(board) {
  out <- seed_ids(board_link_ids(board), 1L)
  if (length(out) == 0L) "" else out
}
