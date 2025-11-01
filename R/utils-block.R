#' @keywords internal
toggle_blk_section <- function(blk, session) {
  id <- attr(blk, "uid")
  accordion_id <- paste0("accordion-", id)

  observeEvent(
    session$input[[sprintf("collapse-blk-section-%s", id)]],
    {
      selected_sections <- session$input[[sprintf(
        "collapse-blk-section-%s",
        id
      )]]
      if (length(selected_sections) == 0) {
        selected_sections <- ""
      }

      bslib::accordion_panel_set(
        accordion_id,
        selected_sections
      )
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE
  )
}

#' @keywords internal
create_issues_ui <- function(id, statuses, ns) {
  # Generate unique collapse ID
  collapse_id <- ns(paste0("outputs-issues-collapse-", id))

  # Create the issues UI component
  div(
    id = ns(sprintf("outputs-issues-%s", id)),
    tags$button(
      class = paste(
        "btn btn-sm btn-outline-secondary",
        "mt-2 mb-2 position-relative"
      ),
      type = "button",
      `data-bs-toggle` = "collapse",
      `data-bs-target` = sprintf("#%s", collapse_id),
      `aria-expanded` = "false",
      `aria-controls` = collapse_id,
      "View issues",
      span(
        class = paste(
          "position-absolute top-0 start-100",
          "translate-middle badge rounded-pill bg-danger"
        ),
        length(statuses)
      )
    ),
    collapse_container(
      id = collapse_id,
      statuses
    )
  )
}

#' @keywords internal
update_blk_state_ui <- function(blk, session) {
  conds <- names(blk$server$cond)
  ns <- session$ns
  id <- attr(blk, "uid")

  lapply(conds, function(nme) {
    observeEvent(blk$server$cond[[nme]], {
      cond <- blk$server$cond[[nme]]
      weak_conds <- c("warning", "message")
      statuses <- drop_nulls(lapply(weak_conds, function(status) {
        cl <- switch(
          status,
          "warning" = "warning",
          "message" = "light"
        )

        msgs <- NULL
        if (length(cond[[status]])) {
          msgs <- tags$div(
            class = sprintf("alert alert-%s", cl),
            HTML(cli::ansi_html(paste(
              unlist(cond[[status]]),
              collapse = "\n"
            )))
          )
        }
        msgs
      }))

      removeUI(paste0(
        "#",
        ns(sprintf("outputs-issues-%s", id))
      ))
      if (length(statuses)) {
        issues_ui <- create_issues_ui(id, statuses, ns)
        insertUI(
          selector = sprintf(
            "#%s",
            ns(sprintf("outputs-issues-wrapper-%s", id))
          ),
          ui = issues_ui
        )
      }

      msgs <- NULL
      removeUI(paste0(
        "#",
        ns(sprintf("errors-block-%s .alert", id))
      ))

      if (length(cond[["error"]])) {
        # Stack error messages
        msgs <- tags$div(
          class = sprintf("alert alert-danger"),
          HTML(cli::ansi_html(paste(
            unlist(cond[["error"]]),
            collapse = "\n"
          )))
        )
        insertUI(
          paste0("#", ns(sprintf("errors-block-%s", id))),
          ui = msgs
        )
      }
    })
  })
}

#' @keywords internal
handle_block_actions <- function(blk, update, session) {
  id <- attr(blk, "uid")
  ns <- session$ns

  observeEvent(
    session$input[[sprintf("append-%s", id)]],
    {
      # Reselect node if node is not selected in the graph
      if (is.null(parent$selected_block)) {
        parent$selected_block <- id
      }
      parent$scoutbar$trigger <- "links"
      if (isFALSE(parent$append_block)) {
        parent$append_block <- TRUE
      }
    }
  )

  observeEvent(
    session$input[[sprintf("delete-%s", id)]],
    {
      parent$removed_block <- id
    }
  )
}

#' Update some pieces of the block UI
#'
#' Some elements of the block UI require server
#' elements to be available. This can't be done
#' from the board_ui as this one is done once.
#'
#' @keywords internal
#' @rdname handlers-utils
update_block_ui <- function(board, update, session) {
  input <- session$input
  ns <- session$ns

  # Register update block UI callbacks for existing blocks
  observeEvent(
    req(length(board$blocks) > 0),
    {
      lapply(
        names(board$blocks),
        function(id) {
          blk <- board$blocks[[id]]
          attr(blk, "uid") <- id
          update_blk_state_ui(blk, session)
          toggle_blk_section(blk, session)
          handle_block_actions(blk, update, session)
        }
      )
    },
    once = TRUE
  )

  # Each time a block is added, we should register the observers above
  observeEvent(
    update()$blocks$add,
    {
      update_blk_state_ui(update()$blocks$add, session)
      toggle_blk_section(update()$blocks$add, session)
      handle_block_actions(update()$blocks$add, update, session)
    }
  )
}
