#' @export
block_ui.dock_board <- function(id, x, blocks = NULL, ...) {

  block_panel <- function(x, id, ns) {

    blk_id <- ns(paste0("block_", id))
    blk_info <- get_block_metadata(x)

    card_tag <- div(
      class = "card",
      width = "100%",
      id = ns(as_block_handle_id(id)),
      div(
        class = c("row", "g-0", "px-4"),
        div(
          class = c(
            "col-sm-2",
            "col-md-1",
            "col-lg-1",
            "d-flex",
            "align-items-center",
            "justify-content-start"
          ),
          blk_icon(blk_info$category, class = "fa-3x")
        ),
        div(
          class = c("col-sm-10", "col-md-11", "col-lg-11"),
          div(
            class = "card-body",
            div(
              class = c(
                "d-flex",
                "align-items-center",
                "justify-content-start",
                "card-title gap-2"
              ),
              bslib::tooltip(
                icon("info-circle"),
                p(icon("lightbulb"), "How to use this block?"),
                p(blk_info$description, ".")
              )
            ),
            div(
              class = c("card-subtitle", "mb-2", "text-body-secondary"),
              span(
                class = c("badge", "bg-secondary"),
                "Type:",
                blk_info$category
              ),
              span(
                class = c("badge", "bg-secondary"),
                "Package:",
                blk_info$package
              )
            )
          )
        )
      ),
      bslib::accordion(
        id = ns(paste0("accordion-", id)),
        multiple = TRUE,
        class = "accordion-flush",
        open = c("inputs", "outputs", "state"),
        bslib::accordion_panel(
          icon = icon("sliders"),
          title = "Block inputs",
          value = "inputs",
          expr_ui(blk_id, x)
        ),
        bslib::accordion_panel(
          icon = icon("chart-simple"),
          title = "Block output(s)",
          value = "outputs",
          style = "max-width: 100%; overflow-x: auto;",
          block_ui(blk_id, x)
        )
      )
    )

    tagAppendAttributes(
      card_tag,
      class = "border border-0 shadow-none"
    )
  }

  stopifnot(is_string(id))

  if (is.null(blocks)) {
    blocks <- board_blocks(x)
  } else if (is.character(blocks)) {
    blocks <- board_blocks(x)[blocks]
  }

  stopifnot(is_blocks(blocks))

  res <- map(
    block_panel,
    blocks,
    names(blocks),
    MoreArgs = list(ns = NS(id))
  )

  if (length(blocks) == 1L) {
    return(res[[1L]])
  }

  res
}

show_hide_block_dep <- function() {
  htmltools::htmlDependency(
    "show-hide-block",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "show-hide-block.js"
  )
}

# Track active group changes in dockview
#
# Problem: When a user clicks a group then triggers an action, Shiny's flush cycle
# means we can't reliably know which group was clicked before the action.
#
# Solution: Capture group changes in JavaScript immediately (before flush cycle),
# maintain history of current + previous group IDs, send to R via Shiny input.
#
# Usage in extensions:
#   history <- context$group_history()
#   target_group <- history$previous  # Group clicked before this action
#
# Key insight: Use HTMLWidgets.find().getWidget() to access dockview API
track_active_group_dep <- function() {
  htmltools::htmlDependency(
    "track-active-group",
    pkg_version(),
    src = character(0),
    head = htmltools::tags$script(htmltools::HTML("
      $(function () {
        let prev = null;

        function init() {
          const el = document.querySelector('[id*=\"dock\"]');
          const api = el && window.HTMLWidgets?.find('#' + el.id)?.getWidget?.();

          if (!api) return setTimeout(init, 100);

          api.onDidActiveGroupChange?.((e) => {
            Shiny.setInputValue(el.id + '_groupChange', {
              current: e?.id,
              previous: prev,
              timestamp: Date.now()
            }, {priority: 'event'});
            prev = e?.id;
          });
        }

        init();
      });
    "))
  )
}

#' @export
remove_block_ui.dock_board <- function(id, x, blocks = NULL, ...,
                                       session = get_session()) {

  if (is.null(blocks)) {
    blocks <- board_block_ids(x)
  }

  if (is_blocks(blocks)) {
    blocks <- names(blocks)
  }

  stopifnot(is.character(blocks), all(blocks %in% board_block_ids(x)))

  proxy <- dock_proxy(session)

  for (blk in blocks) {

    if (as_block_panel_id(blk) %in% block_panel_ids(proxy)) {
      hide_block_panel(blk, proxy = proxy)
    }

    removeUI(
      paste0("#", id, "-", blk),
      immediate = TRUE,
      session = session
    )
  }

  invisible(x)
}

#' @export
insert_block_ui.dock_board <- function(id, x, blocks = NULL, ...,
                                       session = get_session()) {

  if (is.null(blocks)) {
    blocks <- board_blocks(x)
  }

  if (is.character(blocks)) {
    blocks <- board_blocks(x)[blocks]
  }

  for (i in names(blocks)) {

    insertUI(
      paste0("#", id, "-blocks_offcanvas"),
      "beforeEnd",
      block_ui(id, x, blocks[i], ...),
      immediate = TRUE,
      session = session
    )

    show_block_panel(i, proxy = dock_proxy(session))
  }

  invisible(x)
}

show_block_panel <- function(id, add_panel = TRUE, proxy = dock_proxy()) {

  if (isTRUE(add_panel)) {
    add_block_panel(id, proxy)
  } else {
    select_block_panel(id, proxy)
  }

  show_block_ui(id, proxy$session)

  invisible(NULL)
}

hide_block_panel <- function(id, rm_panel = TRUE, proxy = dock_proxy()) {

  hide_block_ui(id, proxy$session)

  if (isTRUE(rm_panel)) {
    remove_block_panel(id, proxy)
  }

  invisible(NULL)
}

hide_block_ui <- function(id, session) {

  ns <- session$ns

  bid <- ns(as_block_handle_id(id))
  oid <- paste0(ns("blocks_offcanvas"), " .offcanvas-body")

  log_debug("hiding block {bid} in {oid}")

  move_dom_element(paste0("#", bid), paste0("#", oid), session)
}

show_block_ui <- function(id, session) {

  ns <- session$ns

  bid <- ns(as_block_handle_id(id))
  pid <- paste(dock_id(ns), as_block_panel_id(id), sep = "-")

  log_debug("showing block {bid} in panel {pid}")

  move_dom_element(paste0("#", bid), paste0("#", pid), session)
}
