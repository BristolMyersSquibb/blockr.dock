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
#' @return For utilities `block_input_select()`, `block_registry_selectize()`
#' and `board_select`, see the respective sections.
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

      num <- suppressWarnings(as.integer(curr))
      nna <- is.na(num)

      if (all(nna)) {
        inps <- c(inps, "1")
      } else {
        num <- num[!nna]
        max <- max(num)
        mis <- setdiff(seq_len(max), num)
        if (length(mis)) {
          inps <- c(inps, as.character(min(mis)))
        } else {
          inps <- c(inps, as.character(max + 1L))
        }
      }

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

#' @section `block_registry_selectize()`:
#' This creates UI for a block registry selector via [shiny::selectizeInput()]
#' and returns an object that inherits from `shiny.tag`.
#'
#' @param id Input ID
#' @param blocks Character vector of block registry IDs
#'
#' @rdname action
#' @export
block_registry_selectize <- function(id, blocks = list_blocks()) {
  options_data <- apply(
    block_metadata(blocks),
    1L,
    function(blk) {
      list(
        value = unname(blk["id"]),
        label = unname(blk["name"]),
        description = unname(blk["description"]),
        category = unname(blk["category"]),
        package = unname(blk["package"]),
        icon = unname(blk["icon"]),
        color = blk_color(blk["category"]),
        searchtext = paste(blk["name"], blk["description"], blk["package"])
      )
    }
  )

  tagList(
    css_block_selectize(),
    selectizeInput(
      id,
      label = "Select block to add",
      choices = NULL,
      options = list(
        options = options_data,
        valueField = "value",
        labelField = "label",
        searchField = c("label", "description", "searchtext"),
        placeholder = "Type to search",
        openOnFocus = FALSE,
        render = js_blk_selectize_render()
      )
    )
  )
}

#' @section `board_select()`:
#' Block selection UI, enumerating all blocks in a board is available as
#' `board_select()`. An object that inherits from `shiny.tag` is returned, which
#' contains the result from a [shiny::selectizeInput()] call.
#'
#' @param selected Character vector of pre-selected block (registry) IDs
#'
#' @rdname action
#' @export
board_select <- function(id, blocks, selected = NULL, ...) {

  meta <- blks_metadata(blocks)

  bid <- names(blocks)
  bnm <- chr_ply(blocks, block_name)

  options_data <- map(
    list,
    value = bid,
    label = bnm,
    block_id = bid,
    block_name = bnm,
    block_type = meta$name,
    package = meta$package,
    category = meta$category,
    icon = meta$icon,
    color = meta$color,
    searchtext = paste(bnm, bid, meta$package)
  )

  options <- list(
    options = options_data,
    valueField = "value",
    labelField = "label",
    searchField = c("label", "description", "searchtext"),
    placeholder = "Type to search blocks...",
    openOnFocus = FALSE,
    plugins = list("remove_button", "drag_drop"),
    render = js_blk_selectize_render()
  )

  if (!is.null(selected) && length(selected) > 0) {
    options$items <- as.list(selected)
  }

  tagList(
    css_block_selectize(),
    tags$style(
      HTML(
        "/* Style the remove button */
        .selectize-input .remove {
          text-decoration: none !important;
          color: #6c757d !important;
          font-weight: normal !important;
          border: none !important;
          margin-left: 8px !important;
          padding: 2px 6px !important;
          border-radius: 3px !important;
          transition: background-color 0.2s ease, color 0.2s ease;
        }
        .selectize-input .remove:hover {
          background-color: rgba(108, 117, 125, 0.1) !important;
          color: #495057 !important;
        }"
      )
    ),
    selectizeInput(
      id,
      ...,
      selected = selected,
      options = options
    ),
    if (!is.null(selected) && length(selected) > 0) {
      tags$script(
        HTML(
          sprintf(
            "$(document).ready(
              function() {
                function setSelectedValues() {
                  var $select = $('#%s');
                  if ($select.length) {
                    var selectize = $select[0].selectize;
                    if (selectize) {
                      try {
                        selectize.setValue(%s, true);
                        return true;
                      } catch(e) {
                        console.log('Error setting values:', e);
                        return false;
                      }
                    }
                  }
                  return false;
                }
                // Try multiple times with increasing delays
                var attempts = 0;
                var maxAttempts = 10;
                var interval = setInterval(
                  function() {
                    attempts++;
                    if (setSelectedValues() || attempts >= maxAttempts) {
                      clearInterval(interval);
                    }
                  },
                  300
                );
              }
            );",
            id,
            paste0("['", paste(selected, collapse = "','"), "']")
          )
        )
      )
    }
  )
}
