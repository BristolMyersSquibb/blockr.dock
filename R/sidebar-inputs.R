# The edit-inputs menu lists every incoming link to one block as an
# ordered list, so a block's inputs can be managed together rather than
# one edge at a time. A variadic block's rows are its positional slots
# (drag to reorder, rename inline, remove); a finite block's rows are its
# declared ports with their current source, shown read-only (redirect a
# single port through the edge editor). The row list is a `uiOutput` so
# the open sidebar tracks the board live - a source renamed, a link added
# or removed elsewhere refreshes the list while it stays open.

edit_inputs_menu_ui <- function(id, board, block_id) {
  stopifnot(is.character(id), length(id) == 1L, nzchar(id))

  ns <- NS(id)
  blk <- board_blocks(board)[[block_id]]

  body <- if (is.null(blk)) {
    tags$p(
      class = "blockr-block-browser-empty",
      "This block is no longer on the board."
    )
  } else {
    tags$div(
      id = ns("commit"),
      class = "blockr-inputs-menu",
      `data-block` = block_id,
      `data-variadic` = if (is.na(block_arity(blk))) "true" else "false",
      uiOutput(ns("rows"))
    )
  }

  htmltools::attachDependencies(
    tags$div(class = "blockr-inputs-edit", body),
    list(inputs_menu_dep())
  )
}

edit_inputs_menu_server <- function(id, board, block_id) {
  stopifnot(is.character(id), length(id) == 1L, nzchar(id))

  block_id_fn <- as_accessor(block_id)

  moduleServer(
    id,
    function(input, output, session) {

      output$rows <- renderUI({
        bid <- block_id_fn()
        req(length(bid) == 1L, !is.na(bid), nzchar(bid))
        brd <- board()
        req(bid %in% board_block_ids(brd))
        edit_inputs_rows(session$ns, brd, bid)
      })

      eventReactive(
        input$commit,
        {
          cmd <- input$commit
          brd <- board()
          bid <- block_id_fn()
          req(
            length(bid) == 1L, !is.na(bid), nzchar(bid),
            bid %in% board_block_ids(brd)
          )

          list(
            update = edit_inputs_update(cmd, brd, bid, session),
            nonce = cmd$nonce
          )
        },
        ignoreNULL = TRUE
      )
    }
  )
}

# Turn a committed command into a ready-to-apply core board update. Each
# variant validates its own inputs (a rejected commit notifies and
# `req()`s out, so the caller never sees an invalid update); an in-range
# no-op returns an empty list, which the action then skips.
edit_inputs_update <- function(cmd, board, block_id, session) {
  switch(
    cmd$action %||% "",
    reorder = {
      mods <- edit_inputs_reorder_delta(
        board, block_id, as.character(unlist(cmd$order))
      )
      if (length(mods)) list(links = list(mod = mods)) else list()
    },
    rename = {
      delta <- edit_inputs_rename_delta(
        board, block_id, as.character(cmd$link_id), cmd$name, session
      )
      if (length(delta)) list(links = list(mod = delta)) else list()
    },
    remove = {
      lid <- as.character(cmd$link_id)
      if (lid %in% block_incoming_links(board, block_id)$id) {
        list(links = list(rm = lid))
      } else {
        list()
      }
    },
    redirect = edit_inputs_redirect_delta(
      board, block_id, as.character(cmd$port),
      as.character(cmd$source %||% ""), session
    ),
    add = edit_inputs_add_delta(
      board, block_id, as.character(cmd$source %||% ""), session
    ),
    list()
  )
}

# Append a positional input to a variadic block: a new link from the chosen
# source, unnamed (empty input). A self-link or a source the block already
# reaches (a cycle) is rejected; the empty choice is a no-op.
edit_inputs_add_delta <- function(board, block_id, source, session) {
  if (!nzchar(source)) {
    return(list())
  }

  blocks <- board_blocks(board)

  if (!(source %in% names(blocks)) || identical(source, block_id)) {
    notify(
      "Please choose a valid source block.", type = "warning",
      session = session
    )
    req(FALSE)
  }

  df <- as.data.frame(board_links(board))

  if (source %in% descendants_of(block_id, df)) {
    notify(
      "This would create a cycle.", type = "warning", session = session
    )
    req(FALSE)
  }

  lnk <- new_link(from = source, to = block_id, input = "")
  list(links = list(add = as_links(set_names(list(lnk), seed_link_id(board)))))
}

# Point a finite block's declared port at a source (or disconnect it). The
# port already carrying a link redirects it (a `from` mod, keeping the id);
# an unwired port adds a fresh link; the empty choice removes the link. A
# self-link or a source the block already reaches (a cycle) is rejected.
# Returns a ready-to-apply update (or an empty list for a no-op).
edit_inputs_redirect_delta <- function(board, block_id, port, source, session) {
  rows <- block_incoming_links(board, block_id)
  cur <- rows[rows$input == port, , drop = FALSE]
  wired <- nrow(cur) > 0L
  link_id <- if (wired) cur$id[[1L]] else NA_character_

  if (!nzchar(source)) {
    if (wired) {
      return(list(links = list(rm = link_id)))
    }
    return(list())
  }

  blocks <- board_blocks(board)

  if (!(source %in% names(blocks)) || identical(source, block_id)) {
    notify(
      "Please choose a valid source block.", type = "warning",
      session = session
    )
    req(FALSE)
  }

  df <- as.data.frame(board_links(board))
  df_wo <- if (wired) df[df$id != link_id, , drop = FALSE] else df

  if (source %in% descendants_of(block_id, df_wo)) {
    notify(
      "This would create a cycle.", type = "warning", session = session
    )
    req(FALSE)
  }

  if (wired) {
    if (identical(source, cur$from[[1L]])) {
      return(list())
    }
    return(
      list(links = list(mod = set_names(list(list(from = source)), link_id)))
    )
  }

  lnk <- new_link(from = source, to = block_id, input = port)
  list(links = list(add = as_links(set_names(list(lnk), seed_link_id(board)))))
}

# Reorder a variadic block's positional inputs by moving each whole row -
# its source AND its name together - to its new slot. The links keep their
# ids and board positions (so no id churn and no new core verb); each fixed
# slot i takes on the `from` and `input` of whichever row now sits at visual
# position i, so a named input's name travels with its source rather than
# being left behind at the old position. `sync_dot_args()` then rebuilds
# `...args` in that order. `new_order` is the link ids in the user's new
# visual order; a stale / non-permutation order is a no-op.
edit_inputs_reorder_delta <- function(board, block_id, new_order) {
  rows <- block_incoming_links(board, block_id)
  slots <- rows$id

  if (length(new_order) != length(slots) || !setequal(new_order, slots)) {
    return(list())
  }

  from_by_id <- set_names(rows$from, rows$id)
  input_by_id <- set_names(rows$input, rows$id)

  mods <- list()

  for (i in seq_along(slots)) {
    slot <- slots[[i]]
    moved <- new_order[[i]]

    delta <- list()
    if (!identical(from_by_id[[moved]], from_by_id[[slot]])) {
      delta$from <- from_by_id[[moved]]
    }
    if (!identical(input_by_id[[moved]], input_by_id[[slot]])) {
      delta$input <- input_by_id[[moved]]
    }
    if (length(delta)) {
      mods[[slot]] <- delta
    }
  }

  mods
}

# Rename one variadic slot (positional <-> named). Empty means positional;
# a non-empty name has to be free among the block's other inputs, since
# core forbids two identically named inputs on one block. Returns the
# id-keyed `mod` delta, or an empty list when the name is unchanged.
edit_inputs_rename_delta <- function(board, block_id, link_id, name, session) {
  rows <- block_incoming_links(board, block_id)

  if (!(link_id %in% rows$id)) {
    return(list())
  }

  name <- as.character(name %||% "")
  current <- rows$input[rows$id == link_id]
  others <- rows$input[rows$id != link_id]

  if (nzchar(name) && name %in% others) {
    notify(
      "This input name is already used on the block.",
      type = "warning", session = session
    )
    req(FALSE)
  }

  if (identical(name, current)) {
    return(list())
  }

  set_names(list(list(input = name)), link_id)
}

# The block's incoming links as a data frame in board order (which, for a
# variadic target, is the positional argument order).
block_incoming_links <- function(board, block_id) {
  df <- as.data.frame(board_links(board))
  keep <- df$to == block_id

  data.frame(
    id = as.character(df$id[keep]),
    from = as.character(df$from[keep]),
    input = as.character(df$input[keep]),
    stringsAsFactors = FALSE
  )
}

inputs_display_name <- function(board, block_id) {
  blk <- board_blocks(board)[[block_id]]

  if (is.null(blk)) {
    return(block_id)
  }

  name <- block_name(blk) %||% block_id
  if (nzchar(name)) name else block_id
}

# ---- row rendering -----------------------------------------------------

edit_inputs_rows <- function(ns, board, block_id) {
  blk <- board_blocks(board)[[block_id]]
  rows <- block_incoming_links(board, block_id)

  if (is.na(block_arity(blk))) {
    # Variadic: reorderable positional slots, plus an "Add input" section
    # whose block picker appends a fresh slot - the slot count is open-ended.
    return(
      tagList(
        css_block_selectize(),
        edit_inputs_variadic_rows(board, rows),
        edit_inputs_add_section(ns, board, block_id)
      )
    )
  }

  # Finite: one row per declared port, each picking its own source. The ports
  # are the block's fixed formals, so there is nothing to add or reorder.
  edit_inputs_finite_rows(ns, board, block_id, rows)
}

edit_inputs_variadic_rows <- function(board, rows) {
  if (nrow(rows) == 0L) {
    return(
      tags$p(
        class = "blockr-block-browser-empty",
        "This block has no inputs yet."
      )
    )
  }

  tags$div(
    class = "blockr-inputs-list",
    lapply(
      seq_len(nrow(rows)),
      function(i) edit_inputs_variadic_row(board, rows[i, ])
    )
  )
}

edit_inputs_variadic_row <- function(board, row) {
  tags$div(
    class = "blockr-inputs-row",
    `data-link-id` = row$id,
    tags$span(
      class = "blockr-inputs-drag-handle",
      `aria-label` = "Drag to reorder",
      title = "Drag to reorder",
      drag_handle_icon()
    ),
    tags$div(
      class = "blockr-inputs-source",
      tags$span(
        class = "blockr-inputs-source-name",
        inputs_display_name(board, row$from)
      ),
      tags$span(class = "blockr-inputs-source-id", paste0("id: ", row$from))
    ),
    tags$input(
      type = "text",
      class = "blockr-inputs-name-input",
      value = row$input,
      placeholder = "unnamed",
      `aria-label` = "Input name"
    ),
    tags$button(
      type = "button",
      class = "blockr-inputs-remove",
      `aria-label` = "Remove input",
      title = "Remove input",
      "\u00d7"
    )
  )
}

# The "Add input" section for a variadic block: a block picker (the same
# selectize as a finite port) over the eligible sources. Picking one appends
# a positional slot - a new link, source -> block, unnamed - which shows up
# as a fresh reorderable row above.
edit_inputs_add_section <- function(ns, board, block_id) {
  tags$div(
    class = "blockr-inputs-add-section",
    tags$span(class = "blockr-inputs-add-label", "Add input"),
    tags$div(
      class = "blockr-inputs-source-picker",
      edit_inputs_source_select(
        ns("add_source"), board,
        edit_inputs_eligible_sources(board, block_id), "",
        placeholder = "Choose a source..."
      )
    )
  )
}

# Finite block: one row per declared port, each a block-browser selectize
# (the same rich picker as the "Connect ..." / edit-link menus) that chooses
# the block feeding that port. Picking a source connects or redirects the
# port; clearing it (the selectize's remove button) disconnects it.
edit_inputs_finite_rows <- function(ns, board, block_id, rows) {
  blk <- board_blocks(board)[[block_id]]
  ports <- block_inputs(blk)

  if (length(ports) == 0L) {
    return(
      tags$p(
        class = "blockr-block-browser-empty",
        "This block takes no inputs."
      )
    )
  }

  sources <- edit_inputs_eligible_sources(board, block_id)

  source_for <- function(port) {
    hit <- rows$from[rows$input == port]
    if (length(hit)) hit[[1L]] else NA_character_
  }

  tagList(
    css_block_selectize(),
    tags$div(
      class = "blockr-inputs-list",
      lapply(ports, function(port) {
        edit_inputs_finite_row(ns, board, port, source_for(port), sources)
      })
    )
  )
}

# Block ids that may feed `block_id` without closing a cycle: every block
# except the target itself and any it already reaches.
edit_inputs_eligible_sources <- function(board, block_id) {
  df <- as.data.frame(board_links(board))
  setdiff(
    names(board_blocks(board)), c(block_id, descendants_of(block_id, df))
  )
}

edit_inputs_finite_row <- function(ns, board, port, from, sources) {
  current <- if (is.null(from) || is.na(from)) "" else as.character(from)
  choices <- union(sources, if (nzchar(current)) current else character())

  tags$div(
    class = "blockr-inputs-row",
    `data-port` = port,
    tags$span(class = "blockr-inputs-port-label", port),
    tags$div(
      class = "blockr-inputs-source-picker",
      edit_inputs_source_select(
        ns(paste0("src_", port)), board, choices, current
      )
    )
  )
}

# A single-select block picker over the eligible sources, rendered with the
# block-browser selectize (icon / name / package badge / search). The remove
# button lets a wired port be cleared (disconnected). Its change is captured
# by the menu binding as a `redirect` command, so the port selectize needs no
# server observer of its own.
edit_inputs_source_select <- function(id, board, sources, current,
                                      placeholder = "Choose a source...") {
  selectizeInput(
    id,
    label = NULL,
    choices = NULL,
    options = list(
      options = build_block_options(board, sources),
      items = if (nzchar(current)) list(current) else list(),
      maxItems = 1L,
      valueField = "value",
      labelField = "label",
      searchField = c("label", "description", "searchtext"),
      render = js_blk_selectize_render(),
      plugins = list("remove_button"),
      placeholder = placeholder,
      # Render the dropdown on <body> so the sidebar's scroll container does
      # not clip it (mirrors ext-edit.R); the CSS lifts it above the sidebar.
      dropdownParent = "body"
    )
  )
}

# ---- assets ------------------------------------------------------------

inputs_menu_dep <- function() {
  htmltools::htmlDependency(
    name = "sidebar-inputs",
    version = utils::packageVersion("blockr.dock"),
    package = "blockr.dock",
    src = "assets",
    stylesheet = "css/sidebar-inputs.css",
    script = "js/sidebar-inputs.js",
    all_files = FALSE
  )
}

drag_handle_icon <- function() {
  tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    viewBox = "0 0 16 16",
    width = "16",
    height = "16",
    fill = "currentColor",
    `aria-hidden` = "true",
    tags$path(
      d = paste0(
        "M6 3.5a1 1 0 1 1-2 0 1 1 0 0 1 2 0zm0 4.5a1 1 0 1 1-2 0 1 1 0 0 ",
        "1 2 0zm0 4.5a1 1 0 1 1-2 0 1 1 0 0 1 2 0zM12 3.5a1 1 0 1 1-2 0 1 ",
        "1 0 0 1 2 0zm0 4.5a1 1 0 1 1-2 0 1 1 0 0 1 2 0zm0 4.5a1 1 0 1 1-2 ",
        "0 1 1 0 0 1 2 0z"
      )
    )
  )
}
