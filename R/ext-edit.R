#' Edit board extension
#'
#' A simplistic example of an extension which can be used for manipulating
#' the board via a table-based UI. Mainly relevant for testing purposes.
#'
#' @param ... Forwarded to `new_dock_extension()`
#'
#' @examples
#' ext <- new_edit_board_extension()
#' is_dock_extension(ext)
#'
#' @return A board extension object that additionally inherits from
#' `edit_board_extension`.
#'
#' @rdname edit
#' @export
new_edit_board_extension <- function(...) {
  new_dock_extension(
    blk_ext_srv,
    blk_ext_ui,
    name = "Edit board",
    class = "edit_board_extension",
    ...
  )
}

blk_ext_ui <- function(id, board) {
  tagList(
    fluidRow(
      column(
        4,
        selectInput(
          NS(id, "registry_select"),
          "Select block from registry",
          choices = list_blocks()
        )
      ),
      column(
        4,
        textInput(
          inputId = NS(id, "block_id"),
          label = "Block ID",
          value = rand_names(board_block_ids(board))
        )
      ),
      column(
        4,
        actionButton(
          NS(id, "confirm_add"),
          "Add block",
          class = "btn-success"
        )
      )
    ),
    fluidRow(
      column(
        4,
        selectInput(
          NS(id, "block_select"),
          "Select block(s) from board",
          choices = board_block_ids(board),
          multiple = TRUE
        )
      ),
      column(
        4,
        actionButton(
          NS(id, "confirm_rm"),
          "Remove block",
          class = "btn-danger"
        )
      )
    ),
    DT::dataTableOutput(NS(id, "links_dt")),
    fluidRow(
      column(
        3,
        textInput(
          NS(id, "new_link_id"),
          "Next link ID",
          value = rand_names(board_link_ids(board))
        )
      ),
      column(
        3,
        actionButton(
          NS(id, "add_link"),
          "Add link row",
          icon = icon("plus")
        )
      ),
      column(
        3,
        actionButton(
          NS(id, "rm_link"),
          "Remove row(s)",
          icon = icon("minus")
        )
      ),
      column(
        3,
        actionButton(
          NS(id, "modify_links"),
          "Apply changes",
          class = "btn-success"
        )
      )
    )
  )
}

blk_ext_srv <- function(id, board, update, ...) {

  moduleServer(
    id,
    function(input, output, session) {

      add_block_observer(input, board, update, session)
      remove_block_observer(input, board, update, session)
      update_block_select_observer(board, session)

      upd <- reactiveValues(
        add = links(),
        rm = character(),
        curr = isolate(board_links(board$board)),
        obs = list(),
        edit = NULL
      )

      observeEvent(
        board_links(board$board),
        {
          upd$curr <- board_links(board$board)
        }
      )

      output$links_dt <- DT::renderDataTable(
        link_dt(isolate(upd$curr), session$ns, isolate(board$board)),
        server = TRUE
      )

      links_proxy <- DT::dataTableProxy("links_dt", session)

      create_link_obs_observer(input, board, upd, session, links_proxy)
      edit_link_observer(upd, board)
      add_link_observer(input, board, upd, session)
      rm_link_observer(input, board, upd, session)
      modify_link_observer(input, board, upd, session, links_proxy, update)

      NULL
    }
  )
}

add_block_observer <- function(input, board, update, session) {
  observeEvent(
    input$confirm_add,
    {
      sel <- input$registry_select
      bid <- input$block_id

      if (!validate_block_addition(sel, bid, board$board, session)) {
        return()
      }

      blk <- create_block(sel, name = id_to_sentence_case(bid))
      add <- as_blocks(set_names(list(blk), bid))

      updateTextInput(
        session,
        "block_id",
        value = rand_names(
          c(board_block_ids(board$board), names(add))
        )
      )

      update(
        list(blocks = list(add = add))
      )
    }
  )
}

remove_block_observer <- function(input, board, update, session) {
  observeEvent(
    input$confirm_rm,
    {
      sel <- input$block_select

      if (!length(sel)) {

        notify(
          "Please choose at least one block.",
          type = "warning",
          session = session
        )

        return()
      }

      if (!all(sel %in% board_block_ids(board$board))) {

        notify(
          "Please choose valid block IDs.",
          type = "warning",
          session = session
        )

        return()
      }

      update(
        list(blocks = list(rm = sel))
      )
    }
  )
}

update_block_select_observer <- function(board, session) {
  observeEvent(
    board_block_ids(board$board),
    updateSelectInput(
      session,
      "block_select",
      choices = board_block_ids(board$board)
    )
  )
}

validate_block_addition <- function(block, id, board, session) {

  if (nchar(id) == 0L || !is_string(id)) {

    notify(
      "Please choose a valid block ID.",
      type = "warning",
      session = session
    )

    return(FALSE)
  }

  if (id %in% board_block_ids(board)) {

    notify(
      "Please choose a unique block ID.",
      type = "warning",
      session = session
    )

    return(FALSE)
  }

  if (!is_string(block) || !block %in% list_blocks()) {

    notify(
      "Please choose a valid block type.",
      type = "warning",
      session = session
    )

    return(FALSE)
  }

  TRUE
}

link_dt <- function(dat, ns, board) {
  res <- DT::datatable(
    dt_board_link(dat, ns, board),
    options = list(
      pageLength = 5,
      preDrawCallback = DT::JS(
        "function() { Shiny.unbindAll(this.api().table().node()); }"
      ),
      drawCallback = DT::JS(
        "function() { Shiny.bindAll(this.api().table().node()); }"
      ),
      dom = "tp",
      ordering = FALSE,
      columnDefs = list(
        list(targets = 0, width = "125px")
      )
    ),
    rownames = FALSE,
    escape = FALSE
  )

  DT::formatStyle(res, 1L, `vertical-align` = "middle")
}

dt_board_link <- function(lnk, ns, board) {

  blks <- board_blocks(board)
  arity <- int_ply(blks, block_arity, use_names = TRUE)

  from_ids <- rep(list(names(blks)), length(lnk))

  to_avail <- arity
  cnt_to <- c(table(filter_empty(lnk$to)))

  to_avail[names(cnt_to)] <- int_mply(`-`, to_avail[names(cnt_to)], cnt_to)
  to_avail[is.na(to_avail)] <- 1L

  to_avail <- lapply(
    lapply(lnk$to, filter_empty),
    union,
    names(to_avail)[to_avail > 0L]
  )

  cnt_to <- table(lnk$to)[names(blks[is.na(arity)])]
  cnt_to[is.na(cnt_to)] <- 0L

  cur_inp <- split(lnk$input, lnk$to)

  in_avail <- c(
    lapply(blks[!is.na(arity)], block_inputs),
    Map(
      union,
      lapply(lapply(cnt_to, seq_len), as.character),
      cur_inp[names(cnt_to)]
    )
  )

  rm_inp <- lapply(
    seq_along(lnk),
    function(i) split(lnk$input[-i], lnk$to[-i])[[lnk$to[i]]]
  )

  data.frame(
    ID = lnk$id,
    From = chr_mply(
      dt_selectize,
      lapply(paste0(lnk$id, "_from"), ns),
      lnk$from,
      Map(setdiff, from_ids, lnk$to)
    ),
    To = chr_mply(
      dt_selectize,
      lapply(paste0(lnk$id, "_to"), ns),
      lnk$to,
      Map(setdiff, to_avail, lnk$from)
    ),
    Input = chr_mply(
      dt_selectize,
      lapply(paste0(lnk$id, "_input"), ns),
      lnk$input,
      Map(setdiff, in_avail[lnk$to], rm_inp),
      is.na(arity[lnk$to])
    )
  )
}

dt_selectize <- function(id, val, choices, create = FALSE) {

  if (isTRUE(create)) {
    opts <- list(create = TRUE)
  } else {
    opts <- NULL
  }

  res <- selectizeInput(id, label = NULL, choices = c("", choices),
                        selected = val, options = opts)

  res <- htmltools::tagQuery(
    res
  )$addAttrs(
    style = "width: 175px; margin-bottom: 0;"
  )$allTags()

  as.character(res)
}

create_dt_link_obs <- function(ids, upd, ...) {

  create_obs <- function(col, row, upd, input, rv, sess) {

    to_choices <- function(blks, upd, new) {

      to_avail <- int_ply(blks, block_arity, use_names = TRUE)

      cnt <- c(table(filter_empty(upd$curr$to)))

      to_avail[names(cnt)] <- int_mply(`-`, to_avail[names(cnt)], cnt)
      to_avail[is.na(to_avail)] <- 1L

      to_avail <- c(
        upd$curr[[row]][["to"]],
        names(to_avail)[to_avail > 0L]
      )

      c("", setdiff(to_avail, new))
    }

    to_selected <- function(upd) {
      upd$curr[[row]][["to"]]
    }

    from_choices <- function(blks, new) {
      ids <- names(blks)
      c("", setdiff(ids, new))
    }

    from_selected <- function(upd) {
      upd$curr[[row]][["from"]]
    }

    inp <- paste0(row, "_", col)

    log_debug("creating link DT observers ", inp)

    obs1 <- observeEvent(
      input[[inp]],
      {
        blks <- board_blocks(rv$board)

        new <- input[[inp]]
        cur <- upd$curr[[row]][[col]]

        if (new == cur || new == "") {
          return()
        }

        if (col == "from") {

          updateSelectizeInput(
            sess,
            inputId = paste0(row, "_to"),
            choices = to_choices(blks, upd, new),
            selected = to_selected(upd)
          )

        } else if (col == "to") {

          updateSelectizeInput(
            sess,
            inputId = paste0(row, "_from"),
            choices = from_choices(blks, new),
            selected = from_selected(upd)
          )

          if (identical(new, "")) {

            updateSelectizeInput(
              sess,
              inputId = paste0(row, "_input"),
              choices = list()
            )

          } else {

            blk <- blks[[new]]
            ary <- block_arity(blk)
            hit <- upd$curr$to == new

            if (is.na(ary)) {
              inp <- as.character(sum(hit) + 1L)
              opt <- list(create = TRUE)
            } else {
              inp <- setdiff(block_inputs(blk), upd$curr$input[hit])
              opt <-  NULL
            }

            updateSelectizeInput(
              sess,
              inputId = paste0(row, "_input"),
              choices = c("", inp),
              selected = upd$curr[[row]][["input"]],
              options = opt
            )
          }

        } else if (col != "input") {

          blockr_abort(
            "Unexpected input: column {col}.",
            class = "unexpected_link_col_input"
          )
        }

        upd$edit <- list(row = row, col = col, val = new)
      },
      ignoreInit = TRUE
    )

    res <- list(obs1)

    if (col %in% c("from", "to")) {

      obs2 <- observeEvent(
        {
          req(
            row %in% board_link_ids(rv$board),
            row %in% names(upd$curr)
          )
          board_block_ids(rv$board)
        },
        {
          blks <- board_blocks(rv$board)

          new <- input[[inp]]

          updateSelectizeInput(
            sess,
            inputId = paste0(row, "_to"),
            choices = to_choices(blks, upd, new),
            selected = to_selected(upd)
          )

          updateSelectizeInput(
            sess,
            inputId = paste0(row, "_from"),
            choices = from_choices(blks, new),
            selected = from_selected(upd)
          )
        }
      )

      res <- c(res, list(obs2))
    }

    res
  }

  create_obs_for_id <- function(id, ...) {
    lapply(
      set_names(nm = c("from", "to", "input")),
      create_obs,
      id,
      ...
    )
  }

  upd$obs[ids] <- lapply(ids, create_obs_for_id, upd, ...)

  upd
}

destroy_dt_link_obs <- function(ids, update) {

  for (row in ids) {
    for (col in c("from", "to", "input")) {
      log_debug("destroying link DT observer ", row, " ", col)
      for (obs in update$obs[[row]][[col]]) {
        obs$destroy()
      }
    }
    update$obs[[row]] <- NULL
  }

  update
}

create_link_obs_observer <- function(input, rv, upd, session, proxy) {

  observeEvent(
    names(upd$curr),
    {
      ids <- names(upd$curr)

      DT::replaceData(
        proxy,
        dt_board_link(upd$curr, session$ns, rv$board),
        rownames = FALSE
      )

      upd <- create_dt_link_obs(setdiff(ids, names(upd$obs)), upd, input,
                                rv, session)
      upd <- destroy_dt_link_obs(setdiff(names(upd$obs), ids), upd)
    }
  )
}

edit_link_observer <- function(upd, rv) {

  observeEvent(
    upd$edit,
    {
      row <- upd$edit$row
      col <- upd$edit$col

      if (!row %in% upd$rm && row %in% board_link_ids(rv$board)) {
        upd$rm <- c(upd$rm, row)
      }

      new <- do.call(
        `$<-`,
        list(upd$curr[row], col, coal(upd$edit$val, ""))
      )

      upd$curr[row] <- new

      if (row %in% names(upd$add)) {
        upd$add[row] <- new
      } else {
        upd$add <- c(upd$add, new)
      }
    }
  )
}

add_link_observer <- function(input, rv, upd, sess) {

  observeEvent(
    input$add_link,
    {
      total <- sum(block_arity(rv$board))

      if (is.na(total) || length(upd$curr) < total) {

        new <- new_link(from = "", to = "", input = "")

        if (length(input$new_link_id) && nchar(input$new_link_id)) {

          updateTextInput(
            session = sess,
            inputId = "new_link_id",
            value = rand_names(c(names(upd$curr), input$new_link_id))
          )

          if (input$new_link_id %in% names(upd$curr)) {

            notify(
              "Please choose a unique link ID.",
              type = "warning",
              session = sess
            )

            return()
          }

          new <- set_names(list(new), input$new_link_id)
        }

        upd$curr <- c(upd$curr, new)
        upd$add <- c(upd$add, upd$curr[length(upd$curr)])

      } else {

        notify(
          "No new links can be added. Remove a link(s) or add block(s) first.",
          type = "warning",
          session = sess
        )
      }
    }
  )
}

rm_link_observer <- function(input, rv, upd, sess) {

  observeEvent(
    input$rm_link,
    {
      sel <- input$links_dt_rows_selected

      if (length(sel)) {

        ids <- names(upd$curr[sel])

        upd$rm <- c(upd$rm, ids[ids %in% board_link_ids(rv$board)])

        upd$add <- upd$add[setdiff(names(upd$add), ids)]
        upd$curr <- upd$curr[setdiff(names(upd$curr), ids)]

      } else {

        notify("No row selected", type = "warning", session = sess)
      }
    }
  )
}

modify_link_observer <- function(input, rv, upd, session, proxy, res) {

  observeEvent(
    input$modify_links,
    {
      if (!length(upd$add) && !length(upd$rm)) {

        notify(
          "No changes specified.",
          type = "warning",
          session = session
        )

        return()
      }

      new <- tryCatch(
        modify_board_links(rv$board, upd$add, upd$rm),
        warning = function(e) {
          notify(conditionMessage(e), duration = NULL, type = "warning",
                 session = session)
        },
        error = function(e) {
          notify(conditionMessage(e), duration = NULL, type = "error",
                 session = session)
        }
      )

      res(
        list(
          links = list(
            add = if (length(upd$add)) upd$add,
            rm = if (length(upd$rm)) upd$rm
          )
        )
      )

      upd$add <- links()
      upd$rm <- character()
      upd$curr <- board_links(new)

      DT::replaceData(
        proxy,
        dt_board_link(upd$curr, session$ns, rv$board),
        rownames = FALSE
      )
    }
  )
}
