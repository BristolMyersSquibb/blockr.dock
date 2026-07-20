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

link_menu_server <- function(id, board, anchor) {
  stopifnot(is.character(id), length(id) == 1L, nzchar(id))

  moduleServer(
    id,
    function(input, output, session) {
      # Live board sync: refresh the open menu in place on every board
      # change by pushing a `menu:sync` diff to our own binding. Self-scoped
      # (bare "commit" - the module session namespaces it), so it no-ops
      # when the panel isn't mounted.
      observeEvent(
        board(),
        {
          anc <- anchor()
          brd <- board()
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

      eventReactive(
        input$commit,
        {
          spec <- input$commit
          spec[["nonce"]] <- NULL
          # The menu owns validation: a duplicate / empty link id notifies
          # and `req()`s out, so the committed reactive never fires invalid.
          validate_link_spec(spec, board(), session)
          link_commit_value(spec, board())
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
# the first free named input, or - for a variadic target - an empty
# (positional) slot, per core's name-or-position input model.
resolve_free_input <- function(block, block_id, links) {
  curr <- links[links$to == block_id]$input
  free <- setdiff(block_inputs(block), curr)

  if (is.na(block_arity(block))) {
    free <- c(free, "")
  }

  free[1L]
}

# Reject an empty / duplicate link id, mirroring the stack menu's
# self-validation. The eligible-pool logic already guarantees a valid
# source / target pair, so only the id needs checking here.
validate_link_spec <- function(spec, board, session) {
  existing <- board_link_ids(board)
  if (!id_available(spec$link_id, existing)) {
    notify(
      "Please choose a valid link ID.", type = "warning", session = session
    )
    req(FALSE)
  }

  # core rejects two identically named inputs on one block, so a
  # user-supplied variadic slot name has to be free on the target.
  name <- spec$block_input
  if (!is.null(name) && nzchar(name)) {
    links <- board_links(board)
    if (name %in% links[links$to == spec$target]$input) {
      notify(
        "This input name is already used on the target block.",
        type = "warning", session = session
      )
      req(FALSE)
    }
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
  if (!(anchor %in% board_block_ids(board))) {
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
    target_inputs = free_inputs[[target_id]] %||% character(),
    target_variadic = is.na(block_arity(blocks[[target_id]]))
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
    if (meta$target_variadic) {
      field_text(
        class_suffix = "input-name",
        id = card_ns("input_name"),
        label = "Input name (optional)",
        value = "",
        placeholder = "leave blank for an unnamed input"
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

# ---- edit-link menu ----------------------------------------------------

# The edit menu is the single-link counterpart to the connect menu: it
# edits one existing link rather than offering a pool of new ones. `from`
# and `to` are block pickers (the block-browser selectize, so they read
# like the add-panel picker), the input port / name field re-renders for
# the currently selected target, and the same eligibility, acyclicity and
# uniqueness rules as `add_link_action` are enforced at commit. The commit
# removes and re-adds the link under the same id, so the id survives.
#
# The endpoints and input field are `uiOutput`s so the open sidebar tracks
# the board live: a block renamed / added / removed elsewhere (e.g. while
# the sidebar is pinned) refreshes the pickers, keeping the selection.

edit_link_menu_ui <- function(id, board, link_id) {
  stopifnot(is.character(id), length(id) == 1L, nzchar(id))

  ns <- NS(id)
  row <- edit_link_row(board, link_id)

  body <- if (is.null(row)) {
    tags$p(
      class = "blockr-block-browser-empty",
      "This link is no longer on the board."
    )
  } else {
    tagList(
      css_block_selectize(),
      uiOutput(ns("endpoints")),
      uiOutput(ns("input_field")),
      actionButton(
        ns("confirm"), "Update link",
        class = "btn-primary blockr-link-edit-confirm"
      )
    )
  }

  htmltools::attachDependencies(
    tags$div(class = "blockr-link-edit", body),
    list(link_menu_dep())
  )
}

edit_link_menu_server <- function(id, board, link_id) {
  stopifnot(is.character(id), length(id) == 1L, nzchar(id))

  link_id_fn <- as_accessor(link_id)

  moduleServer(
    id,
    function(input, output, session) {

      # Authoritative source / target selection. Switching the edited link
      # resets it to that link's committed endpoints - the persisted
      # `input$from/to` still hold the previously edited link's pick until
      # the client re-reports, so relying on them would leak one edit into
      # the next (and mis-key the input control on a variadic vs finite
      # target). Within an edit it tracks the user's picks.
      sel <- reactiveValues(from = NULL, to = NULL)

      observeEvent(link_id_fn(), {
        row <- edit_link_row(isolate(board()), link_id_fn())
        sel$from <- if (is.null(row)) NULL else row$from
        sel$to <- if (is.null(row)) NULL else row$to
      }, ignoreNULL = FALSE)

      observeEvent(input$from, sel$from <- input$from, ignoreInit = TRUE)
      observeEvent(input$to, sel$to <- input$to, ignoreInit = TRUE)

      # Source / target pickers, seeded from the link's committed endpoints
      # and re-rendered on the link switching or any board change - so a
      # rename / add / remove elsewhere refreshes the options and labels
      # while the sidebar stays open. The selectize is client-owned once
      # rendered, so this does not depend on `sel` (which would re-init it
      # on every pick).
      output$endpoints <- renderUI({
        lid <- link_id_fn()
        req(length(lid) == 1L, !is.na(lid), nzchar(lid))
        brd <- board()
        row <- edit_link_row(brd, lid)
        req(row)

        ids <- board_block_ids(brd)
        tagList(
          edit_link_block_select(
            session$ns("from"), "Source block", brd, ids, row$from
          ),
          # Only blocks that can still receive a link (a free named input,
          # or variadic arity) are offered as targets; the edited link's own
          # slot is freed first, so its current target stays selectable.
          edit_link_block_select(
            session$ns("to"), "Target block", brd,
            edit_link_target_ids(brd, lid), row$to
          )
        )
      })

      output$input_field <- renderUI({
        lid <- link_id_fn()
        req(length(lid) == 1L, !is.na(lid), nzchar(lid))
        brd <- board()
        row <- edit_link_row(brd, lid)
        req(row)
        edit_link_input_field(session$ns, brd, lid, sel$to %||% row$to, row)
      })

      eventReactive(
        input$confirm,
        {
          lid <- link_id_fn()
          req(length(lid) == 1L, !is.na(lid), nzchar(lid))
          brd <- board()
          req(lid %in% board_link_ids(brd))

          spec <- gather_edit_link_spec(input, brd, lid, sel$from, sel$to)
          validate_edit_link_spec(spec, brd, lid, session)

          # Return only the changed fields as a `links$mod` delta: the link
          # keeps its id (and its untouched fields), which core merges via
          # `update_link()`. Empty when nothing changed, so the action then
          # issues no update.
          list(delta = edit_link_delta(spec, brd, lid), nonce = input$confirm)
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )
    }
  )
}

# The input slot control for the currently selected target: a free-text
# name for a variadic target (pre-filled only when the target is
# unchanged, since a fresh target has no current slot), else a port picker
# over the target's free named inputs. The edited link is excluded from
# the occupancy so its own slot reads as free.
edit_link_input_field <- function(ns, board, link_id, to_id, row) {
  to_blk <- board_blocks(board)[[to_id]]

  if (is.null(to_blk)) {
    return(NULL)
  }

  links_df <- as.data.frame(links_without(board, link_id))

  if (is.na(block_arity(to_blk))) {
    return(
      textInput(
        ns("input_name"),
        "Input name (optional)",
        value = default_variadic_name(to_id, row),
        placeholder = "leave blank for an unnamed input"
      )
    )
  }

  free <- free_named_inputs(to_blk, to_id, links_df)

  if (!length(free)) {
    return(
      tags$p(
        class = "blockr-block-browser-empty",
        "This target block has no free input port."
      )
    )
  }

  selectInput(
    ns("input_port"), "Input port", free,
    selected = default_finite_slot(free, to_id, row)
  )
}

# `from` / `to` are the authoritative selections tracked by the server
# (reset on a link switch); they fall back to the link's committed
# endpoints when the pickers have not reported yet.
gather_edit_link_spec <- function(input, board, link_id, from = NULL,
                                  to = NULL) {
  row <- edit_link_row(board, link_id)
  from <- from %||% row$from
  to <- to %||% row$to
  to_blk <- board_blocks(board)[[to]]

  # An untouched field falls back to what the rendered control pre-selects
  # (the current slot when the target is unchanged), so a no-op confirm
  # produces no delta - never a switch to a different free slot.
  slot <- if (is.null(to_blk)) {
    ""
  } else if (is.na(block_arity(to_blk))) {
    input$input_name %||% default_variadic_name(to, row)
  } else {
    free <- free_named_inputs(
      to_blk, to, as.data.frame(links_without(board, link_id))
    )
    input$input_port %||% default_finite_slot(free, to, row)
  }

  list(
    from = as.character(from),
    to = as.character(to),
    input = as.character(slot)
  )
}

# The slot a target's input control defaults to: the link's current slot
# when the target is unchanged (and still free), else the first free port
# / an unnamed variadic slot. Shared by the renderer and the commit
# gather so an untouched control and its server-side fallback agree.
default_finite_slot <- function(free, to_id, row) {
  if (identical(to_id, row$to) && row$input %in% free) row$input else free[1L]
}

default_variadic_name <- function(to_id, row) {
  if (identical(to_id, row$to)) row$input else ""
}

# Reject a self-link, a redirect that closes a cycle, or an input slot
# that is taken / out of range - mirroring the eligibility the connect
# menu enforces up front by filtering. Each failure notifies and
# `req(FALSE)`s, stopping the enclosing `eventReactive`.
validate_edit_link_spec <- function(spec, board, link_id, session) {
  blocks <- board_blocks(board)

  if (!(spec$from %in% names(blocks) && spec$to %in% names(blocks))) {
    notify(
      "Please choose valid blocks to link.", type = "warning",
      session = session
    )
    req(FALSE)
  }

  if (identical(spec$from, spec$to)) {
    notify(
      "A link's source and target must differ.", type = "warning",
      session = session
    )
    req(FALSE)
  }

  links_df <- as.data.frame(links_without(board, link_id))

  if (spec$from %in% descendants_of(spec$to, links_df)) {
    notify(
      "This would create a cycle.", type = "warning", session = session
    )
    req(FALSE)
  }

  to_blk <- blocks[[spec$to]]

  if (is.na(block_arity(to_blk))) {
    used <- links_df$input[links_df$to == spec$to]
    if (nzchar(spec$input) && spec$input %in% used) {
      notify(
        "This input name is already used on the target block.",
        type = "warning", session = session
      )
      req(FALSE)
    }
  } else {
    free <- free_named_inputs(to_blk, spec$to, links_df)
    if (!(nzchar(spec$input) && spec$input %in% free)) {
      notify(
        "Please choose a free input port on the target block.",
        type = "warning", session = session
      )
      req(FALSE)
    }
  }

  invisible(TRUE)
}

# The subset of `from` / `to` / `input` that actually changed. Empty when
# the user confirmed without editing (the commit is then skipped).
edit_link_delta <- function(spec, board, link_id) {
  row <- edit_link_row(board, link_id)
  fields <- c("from", "to", "input")
  unchanged <- lgl_mply(identical, spec[fields], row[fields])
  spec[fields][!unchanged]
}

# The edited link's current fields as a plain list, or NULL when the id is
# absent (removed elsewhere mid-edit).
edit_link_row <- function(board, link_id) {
  if (is.null(board) ||
        !(length(link_id) == 1L && !is.na(link_id) && nzchar(link_id))) {
    return(NULL)
  }

  df <- as.data.frame(board_links(board))
  pos <- match(link_id, df$id)

  if (is.na(pos)) {
    return(NULL)
  }

  list(
    from = as.character(df$from[pos]),
    to = as.character(df$to[pos]),
    input = as.character(df$input[pos])
  )
}

links_without <- function(board, link_id) {
  lnks <- board_links(board)
  lnks[setdiff(board_link_ids(board), link_id)]
}

# Block ids eligible as the link's target: those with input capacity (a
# free named input, or variadic arity), computed with the edited link
# removed so its current target - whose slot then frees up - stays in the
# list. Reuses the connect menu's `has_input_capacity()`.
edit_link_target_ids <- function(board, link_id) {
  blocks <- board_blocks(board)
  links_df <- as.data.frame(links_without(board, link_id))
  ids <- names(blocks)
  keep <- vapply(
    ids,
    function(id) has_input_capacity(blocks[[id]], id, links_df),
    logical(1L)
  )
  ids[keep]
}

# A single-select block picker rendered with the block-browser selectize
# (icon, name, package badge, id) - the same look as the add-panel picker
# - preselected to `selected`. `build_block_options()` /
# `js_blk_selectize_render()` are the shared block-browser helpers.
edit_link_block_select <- function(id, label, board, blk_ids, selected) {
  selectizeInput(
    id,
    label = label,
    choices = NULL,
    options = list(
      options = build_block_options(board, blk_ids),
      items = list(selected),
      maxItems = 1L,
      valueField = "value",
      labelField = "label",
      searchField = c("label", "description", "searchtext"),
      render = js_blk_selectize_render()
    )
  )
}
