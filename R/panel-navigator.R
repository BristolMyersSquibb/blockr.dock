#' Panel navigator
#'
#' A right-side sidebar listing *every* block on the board grouped by its
#' blockr.core stack (or "Ungrouped"), each block a row with a visibility
#' switch reflecting whether its panel is on the current view. It turns the
#' add-panel picker inside-out: instead of "what can I add", it shows "here
#' is everything you have, and where it is".
#'
#' This is the "Visible blocks" design (variant B) ported verbatim from the
#' blockr.ui `feat/panel-navigator-stacks` prototype: collapsible accent-
#' coloured stack bars, a per-row toggle switch, grip-drag to move / reorder
#' blocks between stacks, inline rename (double-click), and a "+ New stack"
#' control. The CSS / JS are self-contained (no block-browser dependency),
#' so the navigator lives entirely in blockr.dock and needs nothing from
#' blockr.ui beyond the sidebar shell.
#'
#' Wired by `panel_navigator_observer()` from `board_server_callback()`:
#' the navbar "Blocks" button opens the sidebar (rendered server-side from
#' live board / view state); the switch toggles a panel on the active view
#' (`show/hide_block_panel`); a row click reveals (`select_block_panel`);
#' grip-drag / rename / new-stack go through the core board `update()`
#' channel. Visibility is a view concern (the dock layout, not the core
#' board), which is exactly why the navigator is a dock feature.
#'
#' @param board Reactive board state (list with `$board`).
#' @param update Board update signal (for the core events: assign / rename /
#'   add-stack).
#' @param dock The active dock handle (`active_dock`): `proxy`,
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
  # cards between sections or add a section. Toggle / rename update the DOM
  # client-side, so they skip this to keep scroll / collapse / search state.
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

  # Follow view switches: the switches / counts / tags are all relative to
  # the ACTIVE view, so a pinned navigator must re-render when the active
  # view changes. Keyed on the active view id (not every board change) so
  # block edits and visibility toggles keep scroll / collapse / search
  # state. The render is deferred (refresh_nav -> onFlushed), so it runs
  # after reconcile has swapped `active_dock` to the now-active view.
  last_view <- reactiveVal(active_view(board_layouts(isolate(board$board))))
  observeEvent(board$board, {
    av <- active_view(board_layouts(board$board))
    if (!identical(av, isolate(last_view()))) {
      last_view(av)
      refresh_nav()
    }
  }, ignoreInit = TRUE)

  # One multiplexed input, keyed by `kind` (the navigator's InputBinding).
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
      rename = nav_rename_block(brd, update, ev$id, ev$name),
      rename_stack = nav_rename_stack(brd, update, ev$stack, ev$name),
      assign = {
        payload <- nav_reassign_payload(brd, ev$id, coal(ev$stack, ""), ev$before)
        if (!is.null(payload)) {
          update(payload)
        }
        refresh_nav()
      },
      add_stack = {
        nav_add_stack(brd, update, ev$name)
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

# Build the board-update payload for a grip drag: move `block_id` into stack
# `target` ("" = ungrouped) before block `before` (or append), or reorder
# within a stack. Pure blockr.core; the deltas carry only `blocks`, so stack
# name / colour are preserved, and stack block order serialises. Returns
# `NULL` for a no-op.
nav_reassign_payload <- function(board, block_id, target, before = NULL) {
  if (!block_id %in% board_block_ids(board)) {
    return(NULL)
  }

  all_stacks <- board_stacks(board)

  cur <- NULL
  for (sid in names(all_stacks)) {
    if (block_id %in% stack_blocks(all_stacks[[sid]])) {
      cur <- sid
      break
    }
  }

  target <- if (identical(target, "") || is.na(target)) NULL else target
  before <- if (length(before) && nzchar(before)) before else NULL

  if (!is.null(target) && !target %in% names(all_stacks)) {
    return(NULL)
  }

  insert_before <- function(vec, id, before) {
    vec <- setdiff(vec, id)
    if (is.null(before) || !before %in% vec) {
      c(vec, id)
    } else {
      append(vec, id, after = match(before, vec) - 1L)
    }
  }

  if (identical(cur, target)) {
    if (is.null(target)) {
      return(NULL)
    }
    blks <- stack_blocks(all_stacks[[target]])
    new_blks <- insert_before(blks, block_id, before)
    if (identical(new_blks, blks)) {
      return(NULL)
    }
    return(list(
      stacks = list(mod = set_names(list(list(blocks = new_blks)), target))
    ))
  }

  mod <- list()
  if (!is.null(cur)) {
    mod[[cur]] <- list(blocks = setdiff(stack_blocks(all_stacks[[cur]]), block_id))
  }
  if (!is.null(target)) {
    mod[[target]] <- list(
      blocks = insert_before(stack_blocks(all_stacks[[target]]), block_id, before)
    )
  }

  list(stacks = list(mod = mod))
}

# Create an empty stack with the user-supplied name and a fresh id; it
# renders immediately as a drop target.
nav_add_stack <- function(board, update, name) {
  name <- trimws(coal(name, ""))
  if (!nzchar(name)) {
    return(invisible())
  }

  new_id <- rand_names(old_names = board_stack_ids(board))
  stk <- as_dock_stacks(
    set_names(list(new_dock_stack(character(), name = name)), new_id)
  )

  update(list(stacks = list(add = stk)))
}

# ---- model -------------------------------------------------------------

# Shape the grouped model the renderer consumes: a list of groups, each
# `list(id, name, color, kind, entries)`, where an entry is
# `list(id, type, title, subtitle, icon, package, color, on_view, tags)`.
# Pure function of the current board + the active dock's live panel set.
nav_build_model <- function(board, dock) {
  blocks <- board_blocks(board)
  layouts <- board_layouts(board)
  active <- active_view(layouts)
  labels <- view_names(layouts)

  shown <- shown_block_ids(dock)
  others <- other_view_tags(layouts, active, labels)

  stacks <- board_stacks(board)
  grouped <- unlist(lapply(stacks, stack_blocks), use.names = FALSE)
  ungrouped <- setdiff(board_block_ids(board), grouped)

  entry <- function(bid) {
    meta <- blks_metadata(blocks[bid])
    list(
      id = bid,
      type = "block",
      title = coal(safe_block_name(blocks[[bid]]), as.character(meta$name[1L])),
      subtitle = as.character(meta$category[1L]),
      icon = as.character(meta$icon[1L]),
      package = as.character(meta$package[1L]),
      color = as.character(meta$color[1L]),
      on_view = bid %in% shown,
      tags = unique(others[[bid]])
    )
  }

  groups <- lapply(seq_along(stacks), function(i) {
    s <- stacks[[i]]
    ids <- intersect(stack_blocks(s), names(blocks))
    list(
      id = names(stacks)[[i]],
      name = coal(stack_name(s), names(stacks)[[i]]),
      color = nav_stack_accent(s),
      kind = "stack",
      entries = lapply(ids, entry)
    )
  })

  # Ungrouped carries no stack id (`""`) and stays a drop target so a block
  # can be dragged out of a stack.
  c(
    groups,
    list(list(id = "", name = "Ungrouped", color = "", kind = "ungrouped",
              entries = lapply(ungrouped, entry)))
  )
}

# Stack accent colour, or "" when it carries none (a plain core stack
# reports white) so the renderer falls back to neutral grey.
nav_stack_accent <- function(s) {
  col <- tryCatch(stack_color(s), error = function(e) "")
  if (!is_string(col) || !grepl("^#[0-9a-fA-F]{6}$", col) ||
        toupper(col) == "#FFFFFF") {
    return("")
  }
  col
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

# Block-panel id strings -> block ids ("block_panel-<id>").
block_ids_from_panels <- function(pids) {
  pids <- as.character(pids)
  sub("^block_panel-", "", grep("^block_panel-", pids, value = TRUE))
}

safe_block_name <- function(blk) {
  nm <- tryCatch(block_name(blk), error = function(e) NULL)
  if (is.null(nm) || !nzchar(nm)) NULL else nm
}

# ---- rendering (the "Visible blocks" design, variant B) ----------------

# Build the sidebar body: search + stack-grouped block rows + add-stack.
panel_navigator_body <- function(board, dock, ns) {
  model <- nav_build_model(board, dock)

  htmltools::attachDependencies(
    div(
      # Root id = the InputBinding's reported input ("panel_nav_event").
      id = ns("panel_nav_event"),
      class = "blockr-panel-navigator",
      tags$input(
        type = "search",
        class = "blockr-panel-nav-search",
        placeholder = "Search blocks...",
        `aria-label` = "Search blocks",
        autocomplete = "off",
        autocorrect = "off",
        autocapitalize = "off",
        spellcheck = "false"
      ),
      div(
        class = "blockr-panel-nav-body",
        lapply(model, nav_group_section)
      ),
      div(
        class = "blockr-panel-nav-empty",
        "No blocks match your search."
      ),
      nav_add_stack_control()
    ),
    list(panel_navigator_dep())
  )
}

# One group = one coloured stack bar (`ghd`) you click to collapse, listing
# block rows. Accent = the stack colour (via `--accent*` vars); Ungrouped
# uses a neutral grey. The "N shown" count is how many of the group's blocks
# are currently visible (on the active view).
nav_group_section <- function(group) {
  accent <- if (identical(group$kind, "stack") && nzchar(coal(group$color, ""))) {
    group$color
  } else {
    "#6b7280"
  }
  style <- sprintf(
    "--accent:%s;--accent-bg:%s;--accent-ink:%s;",
    accent, hex_alpha(accent, 0.13), darken_hex(accent, 0.72)
  )

  droppable <- group$kind %in% c("stack", "ungrouped")
  shown <- sum(vapply(group$entries, function(e) isTRUE(e$on_view), logical(1L)))

  label <- if (identical(group$kind, "stack")) {
    nav_rename("stack", group$name, "blockr-panel-nav-glab")
  } else {
    tags$span(class = "blockr-panel-nav-glab", group$name)
  }

  empty_ungrouped <- identical(group$kind, "ungrouped") &&
    length(group$entries) == 0L

  div(
    class = paste(
      "blockr-panel-nav-grp open",
      if (droppable) "blockr-panel-nav-dropzone" else "",
      if (empty_ungrouped) "blockr-panel-nav-grp-empty" else ""
    ),
    style = style,
    `data-stack-id` = if (droppable) group$id else NULL,
    `data-kind` = group$kind,
    div(
      class = "blockr-panel-nav-ghd",
      role = "button",
      tabindex = "0",
      `aria-expanded` = "true",
      nav_caret_icon(),
      label,
      tags$span(
        class = "blockr-panel-nav-gvis",
        sprintf("%d shown", shown)
      )
    ),
    div(
      class = "blockr-panel-nav-gbody",
      div(
        class = "blockr-panel-nav-rows",
        lapply(group$entries, nav_entry_card)
      )
    )
  )
}

# One block row: grip (hover) + category-tinted icon tile + renamable name
# (+ other-view tags) + the visibility switch. `.on`/`.off` reflects
# current-view membership.
nav_entry_card <- function(entry) {
  on_view <- isTRUE(entry$on_view)
  cat_color <- if (nzchar(coal(entry$color, ""))) entry$color else "#999999"

  div(
    class = paste("blockr-panel-nav-row", if (on_view) "on" else "off"),
    draggable = "true",
    `data-panel-id` = entry$id,
    `data-panel-type` = entry$type,
    `data-on-view` = if (on_view) "true" else "false",
    `data-search` = tolower(paste(entry$title, entry$subtitle, entry$package)),
    tags$span(
      class = "blockr-panel-nav-grip",
      title = "Drag to move between stacks / reorder",
      `aria-hidden` = "true",
      nav_grip_icon()
    ),
    tags$span(
      class = "blockr-panel-nav-tile",
      style = sprintf(
        "background:%s;color:%s", hex_alpha(cat_color, 0.13), cat_color
      ),
      if (nzchar(entry$icon)) htmltools::HTML(entry$icon)
    ),
    tags$span(
      class = "blockr-panel-nav-namewrap",
      nav_rename("block", entry$title, "blockr-panel-nav-name"),
      nav_tags_row(entry$tags)
    ),
    nav_switch(on_view)
  )
}

nav_tags_row <- function(tags) {
  if (!length(tags)) {
    return(NULL)
  }
  shiny::tags$span(
    class = "blockr-panel-nav-tags",
    lapply(tags, function(t) {
      shiny::tags$span(class = "blockr-panel-nav-tag", t)
    })
  )
}

# Canonical inline-rename widget: double-click the name / label to rename;
# a subtle hover highlight hints it is editable (no pencil); Enter / blur
# commit, Esc reverts; F2 is the keyboard path. `kind` is "block" or
# "stack" (the navigator commits the matching rename event).
nav_rename <- function(kind, text, extra_class = NULL) {
  tags$span(
    class = "blockr-panel-nav-rename",
    `data-rename-kind` = kind,
    tags$span(
      class = paste("blockr-panel-nav-rename-text", extra_class),
      title = "Double-click to rename",
      text
    ),
    tags$input(
      class = "blockr-panel-nav-rename-input",
      type = "text",
      value = text,
      tabindex = "-1",
      `aria-label` = "Rename",
      autocomplete = "off",
      autocorrect = "off",
      autocapitalize = "off",
      spellcheck = "false"
    )
  )
}

# The switch is the ONLY visibility toggle: a row's pill switch. role=switch
# + aria-checked; the JS toggles on click / Enter / Space.
nav_switch <- function(on) {
  tags$span(
    class = "blockr-panel-nav-switch",
    role = "switch",
    tabindex = "0",
    `aria-checked` = if (on) "true" else "false",
    `aria-label` = "Show / hide on this view"
  )
}

# A compact footer to create a new (empty) stack: a button that reveals an
# inline name input. Submitting fires an "add_stack" event.
nav_add_stack_control <- function() {
  div(
    class = "blockr-panel-nav-addstack",
    tags$button(
      type = "button",
      class = "blockr-panel-nav-addstack-btn",
      "+ New stack"
    ),
    div(
      class = "blockr-panel-nav-addstack-form",
      tags$input(
        type = "text",
        class = "blockr-panel-nav-addstack-input",
        placeholder = "Stack name, then Enter",
        autocomplete = "off",
        autocorrect = "off",
        autocapitalize = "off",
        spellcheck = "false"
      )
    )
  )
}

# ---- icons + colour helpers --------------------------------------------

nav_caret_icon <- function() {
  htmltools::HTML(paste0(
    '<svg class="blockr-panel-nav-caret" viewBox="0 0 24 24" width="15" ',
    'height="15" fill="none" stroke="currentColor" stroke-width="2" ',
    'stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">',
    '<path d="m9 18 6-6-6-6"/></svg>'
  ))
}

nav_grip_icon <- function() {
  htmltools::HTML(paste0(
    '<svg viewBox="0 0 24 24" width="14" height="14" fill="currentColor" ',
    'aria-hidden="true"><circle cx="9" cy="6" r="1.4"/>',
    '<circle cx="15" cy="6" r="1.4"/><circle cx="9" cy="12" r="1.4"/>',
    '<circle cx="15" cy="12" r="1.4"/><circle cx="9" cy="18" r="1.4"/>',
    '<circle cx="15" cy="18" r="1.4"/></svg>'
  ))
}

# rgba()/darkened helpers for the per-stack / per-category accent. Colours
# are 6-digit hex; anything else falls back unchanged.
hex_alpha <- function(hex, alpha) {
  rgb <- hex_rgb(hex)
  if (is.null(rgb)) {
    return(hex)
  }
  sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], alpha)
}

darken_hex <- function(hex, factor) {
  rgb <- hex_rgb(hex)
  if (is.null(rgb)) {
    return(hex)
  }
  sprintf("#%02x%02x%02x", round(rgb[1] * factor), round(rgb[2] * factor),
          round(rgb[3] * factor))
}

hex_rgb <- function(hex) {
  hex <- sub("^#", "", hex)
  if (!grepl("^[0-9a-fA-F]{6}$", hex)) {
    return(NULL)
  }
  c(strtoi(substr(hex, 1, 2), 16L), strtoi(substr(hex, 3, 4), 16L),
    strtoi(substr(hex, 5, 6), 16L))
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
