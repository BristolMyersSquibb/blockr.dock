#' Bridge links across removed blocks
#'
#' When a block with exactly one incoming link is removed, its parent (the
#' `from` of that link) takes over each of the removed block's outgoing links,
#' into the same target inputs. A block with no incoming link, or with two or
#' more, is not bridged: its links are dropped with it.
#'
#' `bridge_links()` computes the links to add alongside a removal. When several
#' blocks go in one update, it bridges through chains: if the parent is itself
#' being removed, it walks up through removed blocks that each have exactly one
#' incoming link until it reaches a surviving block. If the walk hits a removed
#' block with zero or several incoming links, that output is not bridged. A
#' link that already exists (same `from`, `to` and `input`) is not added again,
#' and no self-link is created.
#'
#' The result is meant to go into the same board update as the removal, as in
#' `list(blocks = list(rm = ids), links = list(add = bridge_links(board, ids)))`.
#' blockr.core drops the links incident to the removed blocks in that update,
#' which frees the target inputs the bridge links go into.
#'
#' `bridges_block()` tells whether removing `id` on its own bridges it, i.e.
#' whether the block has exactly one incoming link.
#'
#' @param board A blockr board.
#' @param ids Character vector of ids of the blocks about to be removed.
#' @param id A single block id.
#'
#' @return `bridge_links()` returns a `links` object (possibly empty) of the
#' links to add. `bridges_block()` returns `TRUE` or `FALSE`.
#'
#' @examples
#' brd <- blockr.core::new_board(
#'   blocks = c(
#'     a = blockr.core::new_dataset_block("iris"),
#'     b = blockr.core::new_head_block(),
#'     c = blockr.core::new_head_block()
#'   ),
#'   links = blockr.core::links(
#'     from = c("a", "b"),
#'     to = c("b", "c"),
#'     input = c("data", "data")
#'   )
#' )
#'
#' bridges_block(brd, "b")
#' bridge_links(brd, "b")
#'
#' @export
bridge_links <- function(board, ids) {

  stopifnot(is_board(board), is.character(ids))

  lnks <- board_links(board)

  plan <- bridge_plan(
    from = lnks$from,
    to = lnks$to,
    input = lnks$input,
    ids = ids
  )

  if (!nrow(plan)) {
    return(links())
  }

  as_links(
    map(new_link, from = plan$from, to = plan$to, input = plan$input)
  )
}

#' @rdname bridge_links
#' @export
bridges_block <- function(board, id) {

  stopifnot(is_board(board), is_string(id))

  sum(board_links(board)$to == id) == 1L
}

# The bridging rule on plain link vectors: returns a data frame with columns
# `from`, `to` and `input`, one row per link to add. For every link leaving a
# removed block into a surviving one, find the nearest surviving ancestor
# through removed single-input blocks and point it at the same target input.
bridge_plan <- function(from, to, input, ids) {

  res <- data.frame(
    from = character(),
    to = character(),
    input = character(),
    stringsAsFactors = FALSE
  )

  ids <- unique(ids)

  if (!length(ids) || !length(from)) {
    return(res)
  }

  surviving <- !from %in% ids & !to %in% ids

  # Walk up from a removed block to its nearest surviving ancestor. NA when a
  # block on the way has zero or several incoming links. `seen` guards against
  # a cycle, which a valid board cannot have.
  ancestor <- function(id, seen = character()) {

    inc <- which(to == id)

    if (length(inc) != 1L) {
      return(NA_character_)
    }

    parent <- from[inc]

    if (!nzchar(parent) || parent %in% seen) {
      return(NA_character_)
    }

    if (!parent %in% ids) {
      return(parent)
    }

    ancestor(parent, c(seen, id))
  }

  outs <- which(from %in% ids & !to %in% ids & nzchar(to))

  for (i in outs) {

    parent <- ancestor(from[i])

    if (is.na(parent) || identical(parent, to[i])) {
      next
    }

    exists <- any(
      surviving & from == parent & to == to[i] & input == input[i]
    ) || any(
      res$from == parent & res$to == to[i] & res$input == input[i]
    )

    if (exists) {
      next
    }

    res[nrow(res) + 1L, ] <- list(parent, to[i], input[i])
  }

  res
}

# The board update for removing `ids`: the removal itself plus the bridge
# links. Every dock path that removes blocks goes through here.
remove_blocks_update <- function(board, ids) {

  res <- list(blocks = list(rm = ids))

  add <- bridge_links(board, ids)

  if (length(add)) {
    res$links <- list(add = add)
  }

  res
}
