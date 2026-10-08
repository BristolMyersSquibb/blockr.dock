# What the link menus (action-link.R) know about links: which blocks can be
# linked without a cycle, which inputs are free, and the checks an edit to a
# link passes before it is applied.

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

link_eligible_pools <- function(board, anchor) {
  stopifnot(is.character(anchor), length(anchor) == 1L, nzchar(anchor))
  validate_anchor(board, anchor)

  blocks <- board_blocks(board)
  links_df <- as.data.frame(board_links(board))

  outgoing <- compute_outgoing_targets(blocks, anchor, links_df)
  incoming <- compute_incoming_sources(blocks, anchor, links_df)

  # Per-target free named inputs, keyed by target block id: an outgoing
  # target is the other block, an incoming one is the anchor.
  targets <- unique(c(outgoing, if (length(incoming)) anchor))
  free <- set_names(
    lapply(targets, function(id) free_named_inputs(blocks[[id]], id, links_df)),
    targets
  )
  list(outgoing = outgoing, incoming = incoming, free_inputs = free)
}


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


seed_link_id <- function(board) {
  out <- seed_ids(board_link_ids(board), 1L)
  if (length(out) == 0L) "" else out
}


# Reject a self-link, a redirect that closes a cycle, or an input slot
# that is taken / out of range - mirroring the eligibility Connect
# enforces up front by filtering. Each failure notifies and
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
