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

  div(
    class = "blockr-ext-edit",
    div(
      class = "blockr-ext-edit-section",
      div("Blocks", class = "blockr-ext-edit-title"),
      div(
        class = "blockr-ext-edit-row",
        div(
          class = "flex-grow-1",
          selectInput(
            NS(id, "registry_select"),
            "Select block from registry",
            choices = list_blocks(),
            width = "100%"
          )
        ),
        div(
          class = "flex-grow-1",
          textInput(
            NS(id, "block_id"),
            "Block ID",
            value = rand_names(board_block_ids(board)),
            width = "100%"
          )
        ),
        actionButton(
          NS(id, "confirm_add"),
          "Add block",
          icon = icon("plus"),
          class = "btn-primary"
        )
      ),
      div(
        class = "blockr-ext-edit-row",
        div(
          class = "flex-grow-1",
          selectInput(
            NS(id, "block_select"),
            "Select block(s) from board",
            choices = board_block_ids(board),
            multiple = TRUE,
            width = "100%"
          )
        ),
        actionButton(
          NS(id, "confirm_rm"),
          "Remove block",
          icon = icon("trash"),
          class = "btn-danger"
        )
      )
    ),
    div(
      class = "blockr-ext-edit-section",
      div("Links", class = "blockr-ext-edit-title"),
      div(
        class = "blockr-ext-edit-links",
        DT::dataTableOutput(NS(id, "links_dt"))
      ),
      div(
        class = "blockr-ext-edit-row",
        div(
          class = "flex-grow-1",
          textInput(
            NS(id, "new_link_id"),
            "Next link ID",
            value = rand_names(board_link_ids(board)),
            width = "100%"
          )
        ),
        actionButton(
          NS(id, "add_link"),
          "Add link row",
          icon = icon("plus"),
          class = "btn-primary"
        ),
        actionButton(
          NS(id, "rm_link"),
          "Remove row(s)",
          icon = icon("minus"),
          class = "btn-danger"
        )
      )
    ),
    div(
      class = "blockr-ext-edit-section",
      div("Stacks", class = "blockr-ext-edit-title"),
      div(
        class = "blockr-ext-edit-stacks",
        DT::dataTableOutput(NS(id, "stacks_dt"))
      ),
      div(
        class = "blockr-ext-edit-row",
        div(
          class = "flex-grow-1",
          textInput(
            NS(id, "new_stack_id"),
            "Next stack ID",
            value = rand_names(board_stack_ids(board)),
            width = "100%"
          )
        ),
        actionButton(
          NS(id, "add_stack"),
          "Add stack row",
          icon = icon("plus"),
          class = "btn-primary"
        ),
        actionButton(
          NS(id, "rm_stack"),
          "Remove row(s)",
          icon = icon("minus"),
          class = "btn-danger"
        )
      )
    ),
    div(
      class = "blockr-ext-edit-footer",
      actionButton(
        NS(id, "apply_changes"),
        "Apply changes",
        icon = icon("check"),
        class = "btn-success"
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
      toggle_remove_block_observer(input, session)

      upd <- reactiveValues(
        add = links(),
        rm = character(),
        curr = isolate(board_links(board$board)),
        obs = list(),
        edit = NULL
      )

      applied_links <- deduped_board_reactive(board, board_links)

      observeEvent(
        applied_links(),
        {
          upd$curr <- merge_staged_links(applied_links(), upd$add, upd$rm)
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

      toggle_remove_link_observer(input, session)

      stk <- reactiveValues(
        add = stacks(),
        rm = character(),
        mod = stacks(),
        curr = isolate(board_stacks(board$board)),
        obs = list(),
        edit = NULL
      )

      applied_stacks <- deduped_board_reactive(board, board_stacks)

      observeEvent(
        applied_stacks(),
        {
          stk$curr <- merge_staged_stacks(
            applied_stacks(), stk$add, stk$rm, stk$mod
          )
        }
      )

      output$stacks_dt <- DT::renderDataTable(
        stack_dt(isolate(stk$curr), session$ns, isolate(board$board)),
        server = TRUE
      )

      stacks_proxy <- DT::dataTableProxy("stacks_dt", session)

      create_stack_obs_observer(input, board, stk, session, stacks_proxy)
      edit_stack_observer(stk, board)
      add_stack_observer(input, board, stk, session)
      rm_stack_observer(input, board, stk, session)

      toggle_remove_stack_observer(input, session)

      apply_changes_observer(input, board, upd, stk, session, links_proxy,
                             stacks_proxy, update)
      toggle_apply_changes_observer(upd, stk, session)

      NULL
    }
  )
}

deduped_board_reactive <- function(board, accessor) {

  val <- reactiveVal(isolate(accessor(board$board)))

  observe(
    {
      new <- accessor(board$board)

      if (!identical(new, isolate(val()))) {
        val(new)
      }
    }
  )

  val
}

add_block_observer <- function(input, board, update, session) {
  observeEvent(
    input$confirm_add,
    {
      log_debug("add block confirm")

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
      log_debug("rm block confirm")

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

toggle_remove_block_observer <- function(input, session) {
  observe(
    updateActionButton(
      session,
      "confirm_rm",
      disabled = !length(input$block_select)
    )
  )
}

toggle_remove_link_observer <- function(input, session) {
  observe(
    updateActionButton(
      session,
      "rm_link",
      disabled = !length(input$links_dt_rows_selected)
    )
  )
}

toggle_apply_changes_observer <- function(upd, stk, session) {
  observe(
    updateActionButton(
      session,
      "apply_changes",
      disabled = !length(upd$add) && !length(upd$rm) &&
        !length(stk$add) && !length(stk$rm) && !length(stk$mod)
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
      autoWidth = FALSE,
      columnDefs = list(
        list(targets = 0, width = "90px")
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

  in_avail <- c(
    lapply(blks[!is.na(arity)], block_inputs),
    set_names(
      rep(list(character()), sum(is.na(arity))),
      names(arity)[is.na(arity)]
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

  # Render the dropdown on <body> so it is not clipped by the table wrapper's
  # overflow (overflow-x: auto forces overflow-y to auto, which would otherwise
  # cut the menu off at the table's lower edge).
  opts <- list(dropdownParent = "body")

  if (isTRUE(create)) {
    opts$create <- TRUE
  }

  res <- selectizeInput(id, label = NULL, choices = c("", choices),
                        selected = val, options = opts)

  res <- htmltools::tagQuery(
    res
  )$addAttrs(
    style = "min-width: 120px; width: 100%; margin-bottom: 0;"
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
              inp <- character()
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

      if (setequal(ids, names(upd$obs))) {
        return()
      }

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

merge_staged_links <- function(applied, add, rm) {

  keep <- setdiff(names(applied), setdiff(rm, names(add)))
  out <- applied[keep]

  edited <- intersect(keep, names(add))

  if (length(edited)) {
    out[edited] <- add[edited]
  }

  c(out, add[setdiff(names(add), names(applied))])
}

add_link_observer <- function(input, rv, upd, sess) {

  observeEvent(
    input$add_link,
    {
      log_debug("add link")

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
      log_debug("rm link")

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

apply_changes_observer <- function(input, rv, upd, stk, session, links_proxy,
                                   stacks_proxy, res) {

  observeEvent(
    input$apply_changes,
    {
      log_debug("apply changes")

      has_links <- length(upd$add) || length(upd$rm)
      has_stacks <- length(stk$add) || length(stk$rm) || length(stk$mod)

      if (!has_links && !has_stacks) {

        notify(
          "No changes specified.",
          type = "warning",
          session = session
        )

        return()
      }

      new <- tryCatch(
        withCallingHandlers(
          {
            b <- rv$board

            if (has_links) {
              b <- modify_board_links(b, upd$add, upd$rm)
            }

            if (has_stacks) {
              b <- modify_board_stacks(b, stk$add, stk$rm, stk$mod)
            }

            b
          },
          warning = function(e) {
            notify(conditionMessage(e), duration = NULL, type = "warning",
                   session = session)
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) {
          notify(conditionMessage(e), duration = NULL, type = "error",
                 session = session)
          NULL
        }
      )

      if (is.null(new)) {
        return()
      }

      delta <- list()

      if (has_links) {
        delta$links <- list(
          add = if (length(upd$add)) upd$add,
          rm = if (length(upd$rm)) upd$rm
        )
      }

      if (has_stacks) {
        delta$stacks <- list(
          add = if (length(stk$add)) stk$add,
          rm = if (length(stk$rm)) stk$rm,
          mod = if (length(stk$mod)) {
            lapply(
              stk$mod,
              function(s) {
                list(
                  name = stack_name(s),
                  blocks = stack_blocks(s),
                  color = stack_color(s)
                )
              }
            )
          }
        )
      }

      res(delta)

      if (has_links) {
        upd$add <- links()
        upd$rm <- character()
        upd$curr <- board_links(new)

        DT::replaceData(
          links_proxy,
          dt_board_link(upd$curr, session$ns, rv$board),
          rownames = FALSE
        )
      }

      if (has_stacks) {
        stk$add <- stacks()
        stk$rm <- character()
        stk$mod <- stacks()
        stk$curr <- board_stacks(new)

        DT::replaceData(
          stacks_proxy,
          dt_board_stack(stk$curr, session$ns, rv$board),
          rownames = FALSE
        )
      }
    }
  )
}

stack_dt <- function(dat, ns, board) {

  res <- DT::datatable(
    dt_board_stack(dat, ns, board),
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
      autoWidth = FALSE,
      columnDefs = list(
        list(targets = 0, width = "90px")
      )
    ),
    rownames = FALSE,
    escape = FALSE
  )

  DT::formatStyle(res, 1L, `vertical-align` = "middle")
}

dt_board_stack <- function(stk, ns, board) {

  ids <- names(stk)

  data.frame(
    ID = ids,
    Name = chr_mply(
      dt_text,
      lapply(paste0(ids, "_name"), ns),
      chr_ply(stk, stack_name)
    ),
    Color = chr_mply(
      dt_color,
      lapply(paste0(ids, "_color"), ns),
      chr_ply(stk, stack_color)
    ),
    Blocks = chr_mply(
      dt_select,
      lapply(paste0(ids, "_blocks"), ns),
      lapply(stk, as.character),
      MoreArgs = list(
        rem = free_stack_blocks(stk, board_block_ids(board))
      )
    )
  )
}

free_stack_blocks <- function(stk, blocks) {
  blocks[!blocks %in% unlst(lapply(stk, as.character))]
}

dt_text <- function(id, val) {

  res <- textInput(id, label = NULL, value = val)

  res <- htmltools::tagQuery(
    res
  )$addAttrs(
    style = "min-width: 140px; width: 100%; margin-bottom: 0;"
  )$allTags()

  as.character(res)
}

dt_select <- function(id, val, rem) {

  # selectizeInput (rather than selectInput) so the dropdown can render on
  # <body> and escape the table wrapper's overflow clipping (see dt_selectize).
  res <- selectizeInput(id, label = NULL, choices = c("", val, rem),
                        selected = val, multiple = TRUE,
                        options = list(dropdownParent = "body"))

  res <- htmltools::tagQuery(
    res
  )$addAttrs(
    style = "min-width: 180px; width: 100%; margin-bottom: 0;"
  )$allTags()

  as.character(res)
}

dt_color <- function(id, val) {

  # A native colour input that reports straight through Shiny.setInputValue: it
  # carries no Shiny input binding, so it survives the DataTables redraw and the
  # Shiny.bindAll() that re-binds the table's textInput/selectInput cells. The
  # form-control-color class matches the radius and swatch rounding of the
  # inputs beside it; the inline height matches this theme's taller form-control
  # (form-control-color otherwise hard-codes Bootstrap's shorter input height).
  res <- tags$input(
    type = "color",
    id = id,
    value = val,
    class = "form-control form-control-color",
    onchange = paste0("Shiny.setInputValue('", id, "', this.value);")
  )

  res <- htmltools::tagQuery(
    res
  )$addAttrs(
    style = "width: 100%; min-width: 56px; height: calc(1.5em + 1.25rem + 2px)"
  )$allTags()

  as.character(res)
}

create_dt_stack_obs <- function(ids, upd, ...) {

  create_obs <- function(col, row, upd, input, blks, sess) {

    inp <- paste0(row, "_", col)

    log_debug("creating stack DT observer ", inp)

    observeEvent(
      input[[inp]],
      {
        new <- input[[inp]]

        if (col == "blocks" && setequal(stack_blocks(upd$curr[[row]]), new)) {
          return()
        }

        if (col == "name" && identical(stack_name(upd$curr[[row]]), new)) {
          return()
        }

        if (col == "color" && identical(stack_color(upd$curr[[row]]), new)) {
          return()
        }

        if (col == "blocks") {

          if (is.null(new)) {
            new <- character()
          }

          rem <- setdiff(names(upd$curr), row)
          ava <- free_stack_blocks(c(upd$curr[rem], new), names(blks))

          for (i in rem) {

            cur <- stack_blocks(upd$curr[[i]])

            updateSelectInput(
              sess,
              paste0(i, "_blocks"),
              label = NULL,
              choices = c(cur, ava),
              selected = cur
            )
          }

        } else if (!col %in% c("name", "color")) {

          blockr_abort(
            "Unexpected input: column {col}.",
            class = "unexpected_stack_col_input"
          )
        }

        upd$edit <- list(row = row, col = col, val = new)
      },
      ignoreInit = TRUE,
      ignoreNULL = FALSE
    )
  }

  create_obs_for_id <- function(id, ...) {
    lapply(
      set_names(nm = c("name", "color", "blocks")),
      create_obs,
      id,
      ...
    )
  }

  upd$obs[ids] <- lapply(ids, create_obs_for_id, upd, ...)

  upd
}

destroy_dt_stack_obs <- function(ids, update) {

  for (row in ids) {

    for (col in c("name", "color", "blocks")) {
      log_debug("destroying stack DT observer ", row, " ", col)
      update$obs[[row]][[col]]$destroy()
    }

    update$obs[[row]] <- NULL
  }

  update
}

create_stack_obs_observer <- function(input, rv, upd, session, proxy) {

  observeEvent(
    names(upd$curr),
    {
      ids <- names(upd$curr)

      if (setequal(ids, names(upd$obs))) {
        return()
      }

      DT::replaceData(
        proxy,
        dt_board_stack(upd$curr, session$ns, rv$board),
        rownames = FALSE
      )

      upd <- create_dt_stack_obs(setdiff(ids, names(upd$obs)), upd, input,
                                 board_blocks(rv$board), session)
      upd <- destroy_dt_stack_obs(setdiff(names(upd$obs), ids), upd)
    }
  )
}

edit_stack_observer <- function(upd, rv) {

  observeEvent(
    upd$edit,
    {
      row <- upd$edit$row
      col <- upd$edit$col
      val <- upd$edit$val

      if (col == "color" && !is_hex_color(val)) {
        return()
      }

      if (col == "name") {
        stack_name(upd$curr[[row]]) <- val
      } else if (col == "color") {
        stack_color(upd$curr[[row]]) <- val
      } else if (col == "blocks") {
        stack_blocks(upd$curr[[row]]) <- val
      }

      if (row %in% board_stack_ids(rv$board)) {

        if (row %in% names(upd$mod)) {
          upd$mod[row] <- upd$curr[row]
        } else {
          upd$mod <- c(upd$mod, upd$curr[row])
        }

      } else {

        if (row %in% names(upd$add)) {
          upd$add[row] <- upd$curr[row]
        } else {
          upd$add <- c(upd$add, upd$curr[row])
        }
      }
    }
  )
}

merge_staged_stacks <- function(applied, add, rm, mod) {

  keep <- setdiff(names(applied), rm)
  out <- applied[keep]

  modded <- intersect(keep, names(mod))

  if (length(modded)) {
    out[modded] <- mod[modded]
  }

  c(out, add[setdiff(names(add), names(applied))])
}

add_stack_observer <- function(input, rv, upd, sess) {

  observeEvent(
    input$add_stack,
    {
      log_debug("add stack")

      new_id <- input$new_stack_id

      if (length(new_id) && nchar(new_id)) {

        updateTextInput(
          session = sess,
          inputId = "new_stack_id",
          value = rand_names(c(names(upd$curr), new_id))
        )

        if (new_id %in% names(upd$curr)) {

          notify(
            "Please choose a unique stack ID.",
            type = "warning",
            session = sess
          )

          return()
        }

      } else {

        new_id <- rand_names(names(upd$curr))
      }

      new <- set_names(
        list(
          new_dock_stack(
            name = id_to_sentence_case(new_id),
            color = suggest_new_colors(stack_color(upd$curr))
          )
        ),
        new_id
      )

      upd$curr <- c(upd$curr, new)
      upd$add <- c(upd$add, upd$curr[length(upd$curr)])
    }
  )
}

rm_stack_observer <- function(input, rv, upd, sess) {

  observeEvent(
    input$rm_stack,
    {
      log_debug("rm stack")

      sel <- input$stacks_dt_rows_selected

      if (length(sel)) {

        ids <- names(upd$curr[sel])

        upd$rm <- c(upd$rm, ids[ids %in% board_stack_ids(rv$board)])

        upd$add <- upd$add[setdiff(names(upd$add), ids)]
        upd$mod <- upd$mod[setdiff(names(upd$mod), ids)]
        upd$curr <- upd$curr[setdiff(names(upd$curr), ids)]

      } else {

        notify("No row selected", type = "warning", session = sess)
      }
    }
  )
}

toggle_remove_stack_observer <- function(input, session) {
  observe(
    updateActionButton(
      session,
      "rm_stack",
      disabled = !length(input$stacks_dt_rows_selected)
    )
  )
}
