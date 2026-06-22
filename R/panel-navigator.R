#' Panel navigator
#'
#' A right-side sidebar that lists *every* block on the board, grouped by
#' its blockr.core stack (or "Ungrouped"), with a per-row eye toggle
#' reflecting whether the block's panel is on the current view. It turns
#' the add-panel picker inside-out: instead of "what can I add", it shows
#' "here is everything you have, and where it is".
#'
#' The navigator lives entirely in blockr.dock and is built on the
#' `blockr.ui` sidebar + card primitives (it reuses the block-browser card
#' chrome via `blockr.ui::block_browser_dep()` but renders its own markup,
#' so blockr.ui needs no changes). It is wired by
#' `panel_navigator_observer()` from `board_server_callback()`:
#'
#' * the navbar "Blocks" button (`open_panel_navigator`) opens the sidebar,
#'   rendering the current board / view state server-side;
#' * the card eye toggles a block's panel on the active view via
#'   `show_block_panel()` / `hide_block_panel()`;
#' * clicking a shown row reveals (selects) its panel via
#'   `select_block_panel()`.
#'
#' Visibility is a *view* concern (it lives in the dock layout, not the
#' core board), which is exactly why the navigator is a dock feature: the
#' eye toggles per-view panel membership, while the block list and stack
#' grouping it reads come straight from blockr.core.
#'
#' This first cut covers the read / eye / reveal path; rename, drag-to-stack
#' and "+ new stack" are layered on top later (they route through the core
#' board `update()` channel, not the dock).
#'
#' @param board Reactive board state (list with `$board`).
#' @param update Board update signal (reserved for the later core-event
#'   path; unused in this cut).
#' @param dock The active dock handle (`active_dock`): carries `proxy`,
#'   `live_panels`, `board_ns`.
#' @param session Shiny session (the board session).
#'
#' @noRd
panel_navigator_observer <- function(board, update, dock,
                                     session = get_session()) {
  input <- session$input
  ns <- session$ns
  sidebar_id <- ns("panel_navigator_sidebar")

  render_nav <- function() {
    blockr.ui::show_sidebar(
      sidebar_id,
      title = "Blocks",
      ui = panel_navigator_body(board$board, dock, ns)
    )
  }

  # Re-render on the next flush (after `update()` has mutated the board) but
  # only while the sidebar is open — used after grouping changes that move
  # cards between sections or add a section. Rename / toggle update the DOM
  # client-side, so they skip this to keep scroll / search state.
  refresh_nav <- function() {
    session$onFlushed(
      function() {
        isolate(
          if (isTRUE(blockr.ui::sidebar_state(sidebar_id)$open)) {
            render_nav()
          }
        )
      },
      once = TRUE
    )
  }

  observeEvent(input$open_panel_navigator, render_nav())

  observeEvent(input$panel_nav_event, {
    ev <- input$panel_nav_event
    req(ev$kind)
    brd <- board$board

    switch(
      ev$kind,
      # Dock (view-membership) events — applied on the active dock.
      toggle = nav_toggle(brd, dock, ev$id, ev$to),
      focus = if (ev$id %in% board_block_ids(brd)) {
        select_block_panel(ev$id, dock$proxy)
      },
      # Core events — applied through the board `update()` channel.
      rename = nav_rename_block(brd, update, ev$id, ev$value),
      rename_stack = nav_rename_stack(brd, update, ev$id, ev$value),
      assign = {
        nav_reassign_stack(brd, update, ev$id, ev$to)
        refresh_nav()
      },
      add_stack = {
        nav_add_stack(brd, update)
        refresh_nav()
      },
      invisible(NULL)
    )
  })

  invisible(NULL)
}

# ---- event handlers ----------------------------------------------------

nav_toggle <- function(board, dock, bid, to) {
  if (!bid %in% board_block_ids(board)) {
    return(invisible())
  }
  if (identical(to, "add")) {
    show_block_panel(board_blocks(board)[bid], add_panel = TRUE, dock = dock)
  } else {
    hide_block_panel(bid, rm_panel = TRUE, dock = dock)
  }
}

nav_rename_block <- function(board, update, bid, value) {
  value <- trimws(coal(value, ""))
  if (nzchar(value) && bid %in% board_block_ids(board)) {
    update(list(
      blocks = list(mod = set_names(list(list(block_name = value)), bid))
    ))
  }
}

nav_rename_stack <- function(board, update, sid, value) {
  value <- trimws(coal(value, ""))
  if (nzchar(value) && sid %in% board_stack_ids(board)) {
    # Partial-arg delta: `update_stack()` merges it onto the live stack, so
    # sending only `name` preserves the blocks and colour.
    update(list(
      stacks = list(mod = set_names(list(list(name = value)), sid))
    ))
  }
}

# Move a block to stack `to` ("" / NA -> Ungrouped). One block lives in at
# most one stack, so this is a remove-from-source + add-to-target pair, sent
# as a single delta so blockr.core validates the consistent end state.
nav_reassign_stack <- function(board, update, bid, to) {
  if (!bid %in% board_block_ids(board)) {
    return(invisible())
  }

  stacks <- board_stacks(board)
  to <- if (is.null(to) || !nzchar(to)) NA_character_ else to

  src <- NA_character_
  for (sid in names(stacks)) {
    if (bid %in% stack_blocks(stacks[[sid]])) {
      src <- sid
      break
    }
  }

  if (identical(src, to)) {
    return(invisible())
  }
  if (!is.na(to) && !to %in% names(stacks)) {
    return(invisible())
  }

  mods <- list()
  if (!is.na(src)) {
    mods[[src]] <- list(blocks = setdiff(stack_blocks(stacks[[src]]), bid))
  }
  if (!is.na(to)) {
    mods[[to]] <- list(blocks = union(stack_blocks(stacks[[to]]), bid))
  }

  if (length(mods)) {
    update(list(stacks = list(mod = mods)))
  }
}

# Create an empty stack with a fresh id and a default "Stack N" name (the
# user renames it inline afterwards). It renders immediately as a drop zone.
nav_add_stack <- function(board, update) {
  existing <- chr_ply(board_stacks(board), stack_name)
  n <- length(existing) + 1L
  while (paste("Stack", n) %in% existing) {
    n <- n + 1L
  }

  new_id <- rand_names(old_names = board_stack_ids(board))
  stk <- as_dock_stacks(
    set_names(list(new_dock_stack(character(), name = paste("Stack", n))), new_id)
  )

  update(list(stacks = list(add = stk)))
}

# Build the sidebar body: a search box + stack-grouped block cards. Pure
# function of the current board + the active dock's live panel set, so it
# is re-rendered on each open (and, later, on board-structure change).
panel_navigator_body <- function(board, dock, ns) {

  layouts <- board_layouts(board)
  active <- active_view(layouts)
  labels <- view_names(layouts)

  shown <- shown_block_ids(dock)
  others <- other_view_tags(layouts, active, labels)

  blocks <- board_blocks(board)
  groups <- nav_groups(board, blocks)

  htmltools::attachDependencies(
    div(
      id = ns("panel_nav"),
      class = "blockr-stack-menu blockr-panel-nav",
      # JS reads this to address the multiplexed Shiny input it fires
      # (toggle / focus events). `.blockr-stack-menu` only resolves the
      # shared card design tokens; the block-browser InputBinding scopes to
      # `.blockr-block-browser`, so it never binds here.
      `data-input-id` = ns("panel_nav_event"),
      tags$input(
        type = "search",
        class = "blockr-block-browser-search blockr-panel-nav-search",
        placeholder = "Search...",
        `aria-label` = "Search blocks"
      ),
      div(
        class = "blockr-block-browser-categories blockr-panel-nav-groups",
        lapply(groups, nav_group, shown = shown, others = others,
               blocks = blocks)
      ),
      tags$button(
        type = "button",
        class = "blockr-panel-nav-add-stack",
        bsicons::bs_icon("plus-lg"),
        "New stack"
      ),
      div(
        class = "blockr-block-browser-empty",
        "No blocks match your search."
      )
    ),
    list(blockr.ui::block_browser_dep(), panel_navigator_dep())
  )
}

# Group block ids by stack, in board order, with an "Ungrouped" bucket
# (block ids in no stack) last. Each group carries its display name and
# colour (NA when none / plain white).
nav_groups <- function(board, blocks) {
  stacks <- board_stacks(board)

  grouped <- unlist(lapply(stacks, stack_blocks), use.names = FALSE)
  ungrouped <- setdiff(board_block_ids(board), grouped)

  groups <- lapply(seq_along(stacks), function(i) {
    s <- stacks[[i]]
    list(
      id = names(stacks)[[i]],
      name = coal(stack_name(s), names(stacks)[[i]]),
      color = nav_stack_color(s),
      ids = stack_blocks(s)
    )
  })

  # The Ungrouped bucket carries no stack id (`""`); dropping a card on it
  # removes the block from its stack.
  groups <- c(
    groups,
    list(list(id = "", name = "Ungrouped", color = NA_character_,
              ids = ungrouped))
  )

  groups
}

# Stack colour, or NA when it carries none (a plain core stack reports
# white) — used to decide whether to draw the heading colour dot.
nav_stack_color <- function(s) {
  col <- tryCatch(stack_color(s), error = function(e) NA_character_)
  if (!is_string(col) || !nzchar(col) || toupper(col) == "#FFFFFF") {
    return(NA_character_)
  }
  col
}

nav_group <- function(g, shown, others, blocks) {
  ids <- intersect(g$ids, names(blocks))
  n_shown <- sum(ids %in% shown)

  div(
    class = "blockr-block-browser-category blockr-panel-nav-group",
    `data-category` = g$name,
    # The stack id (`""` for Ungrouped) is the drop target: dropping a card
    # here reassigns the block to this stack (or out of any stack).
    `data-stack-id` = g$id,
    div(
      class = "blockr-panel-nav-group-head",
      if (!is.na(g$color)) {
        tags$span(
          class = "blockr-panel-nav-dot",
          style = paste0("background:", g$color, ";")
        )
      },
      tags$h3(g$name),
      tags$span(
        class = "blockr-panel-nav-count",
        paste0(n_shown, "/", length(ids), " shown")
      )
    ),
    div(
      class = "blockr-block-browser-cards",
      lapply(ids, function(bid) {
        nav_card(bid, blocks[[bid]], blocks[bid], bid %in% shown, others[[bid]])
      })
    )
  )
}

# One card row, reusing the block-browser card classes for the icon tile /
# name chrome, plus the navigator's own eye toggle and view tags. `blk` is
# the single block (for its instance name); `blk1` the length-1 blocks
# subset (for registry metadata via blks_metadata()).
nav_card <- function(bid, blk, blk1, is_shown, tags_for) {

  meta <- blks_metadata(blk1)
  name <- coal(safe_block_name(blk), as.character(meta$name[1L]))
  icon <- as.character(meta$icon[1L])
  category <- as.character(meta$category[1L])
  package <- as.character(meta$package[1L])

  div(
    class = paste(
      "blockr-block-browser-card blockr-panel-nav-card",
      if (is_shown) "pn-shown" else "pn-hidden"
    ),
    `data-block-id` = bid,
    draggable = "true",
    `data-name` = name,
    `data-package` = package,
    `data-category` = category,
    div(
      class = "blockr-block-browser-card-header",
      tags$span(
        class = "blockr-block-browser-card-icon",
        if (nzchar(icon)) htmltools::HTML(icon)
      ),
      div(
        class = "blockr-block-browser-card-body",
        div(
          class = "blockr-block-browser-card-titles",
          tags$span(class = "blockr-block-browser-card-name", name),
          if (length(tags_for)) {
            tags$span(
              class = "blockr-panel-nav-tags",
              lapply(unique(tags_for), function(t) {
                tags$span(class = "blockr-panel-nav-tag", t)
              })
            )
          },
          tags$button(
            type = "button",
            class = "blockr-panel-nav-eye",
            `aria-label` = "Toggle on this view",
            title = "Show / hide on this view",
            bsicons::bs_icon("eye-fill")
          )
        )
      )
    )
  )
}

safe_block_name <- function(blk) {
  nm <- tryCatch(block_name(blk), error = function(e) NULL)
  if (is.null(nm) || !nzchar(nm)) NULL else nm
}

# Block ids whose panel is currently shown on the ACTIVE view. Read from
# the live dock's authoritative membership set (ahead of the browser echo).
shown_block_ids <- function(dock) {
  lp <- dock$live_panels
  if (is.null(lp)) {
    return(character())
  }
  live <- tryCatch(isolate(lp()), error = function(e) character())
  block_ids_from_panels(live)
}

# For every OTHER view, the block ids it shows -> the view labels each
# block appears on. Drives the "also on X" tags.
other_view_tags <- function(layouts, active, labels) {
  res <- list()
  for (vid in names(layouts)) {
    if (identical(vid, active)) {
      next
    }
    pids <- tryCatch(
      as.character(layout_panel_ids(layouts[[vid]])),
      error = function(e) character()
    )
    for (b in block_ids_from_panels(pids)) {
      res[[b]] <- c(res[[b]], unname(labels[[vid]]))
    }
  }
  res
}

# Block-panel id strings -> block ids. The panel id format is
# "block_panel-<id>" (see maybe_block_panel_id()).
block_ids_from_panels <- function(pids) {
  pids <- as.character(pids)
  sub("^block_panel-", "", grep("^block_panel-", pids, value = TRUE))
}

panel_navigator_dep <- function() {
  htmltools::htmlDependency(
    "blockr-panel-navigator",
    pkg_version(),
    src = pkg_file("assets"),
    stylesheet = "css/panel-navigator.css",
    script = "js/panel-navigator.js"
  )
}
