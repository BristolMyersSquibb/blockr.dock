#' @section `block_input_select()`:
#' Determine input options for a block by removing inputs that are already used
#' and also takes into account some edge-cases, such as variadic blocks. If
#' `mode` is set as "inputs", this will return a character vector, for
#' "create", the return value of a [shiny::selectizeInput()] call and for
#' "update", the return value of a [shiny::updateSelectizeInput()] call.
#'
#' @param block Block object
#' @param block_id Block ID
#' @param links Links object
#' @param mode Switch for determining the return object
#' @param ... Forwarded to other methods
#'
#' @return For utilities `block_input_select()` and `board_block_select()`,
#' see the respective sections.
#'
#' @rdname action
#' @export
block_input_select <- function(block = NULL, block_id = NULL, links = NULL,
                               mode = c("create", "update", "inputs"), ...) {

  mode <- match.arg(mode)

  if (mode == "inputs") {
    stopifnot(
      ...length() == 0L,
      not_null(block),
      not_null(block_id),
      not_null(links)
    )
  }

  if (is.null(block_id) && is.null(links)) {
    curr <- character()
  } else {
    stopifnot(is_links(links), is_string(block_id))
    curr <- links[links$to == block_id]$input
  }

  if (is.null(block)) {

    stopifnot(is.null(block_id), is.null(links))

    inps <- c(`Select a block to populate options` = "")
    opts <- list()

  } else {

    stopifnot(is_block(block))

    inps <- setdiff(block_inputs(block), curr)

    if (is.na(block_arity(block))) {

      inps <- c(inps, "")

      opts <- list(create = TRUE)

    } else if (length(inps)) {

      opts <- list()

    } else {

      if (mode == "inputs") {
        return(character())
      } else {
        return(NULL)
      }
    }
  }

  if (mode == "create") {
    return(
      selectizeInput(..., choices = inps, options = opts)
    )
  }

  if (mode == "update") {
    updateSelectizeInput(..., choices = inps, options = opts)
  }

  inps
}

#' @section `board_block_select()`:
#' Block selection UI over the blocks of a board is available as
#' `board_block_select()`, which returns an object inheriting from
#' `shiny.tag.list`: the result of a [shiny::selectizeInput()] call together
#' with the styling its option rendering requires. This is the picker the
#' board itself uses wherever a block is chosen, listing each block by icon,
#' name, ID and defining package, and searchable over all of those.
#'
#' @param id Input ID
#' @param board Board object
#' @param blk_ids Character vector of block IDs to offer for selection
#' @param selected Character vector of pre-selected block IDs
#' @param max_items Maximum number of blocks that can be selected at once, or
#' `NULL` for no limit
#' @param label Input label
#' @param options Passed to [shiny::selectizeInput()] as `options`, merged
#' over the defaults that make up the picker
#'
#' @rdname action
#' @export
board_block_select <- function(id, board, blk_ids = board_block_ids(board),
                               selected = NULL, max_items = 1L, label = NULL,
                               options = list()) {

  blk_selectize(
    id,
    build_block_options(board, blk_ids),
    selected,
    max_items,
    label,
    options
  )
}
