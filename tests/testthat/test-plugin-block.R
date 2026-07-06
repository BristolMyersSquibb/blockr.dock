test_that("edit block server", {

  testServer(
    edit_block_server(),
    {
      expect_null(update())

      session$setInputs(
        block_name_in = "Test block name"
      )

      upd <- update()

      expect_length(upd, 1L)
      expect_type(upd, "list")
      expect_named(upd, "blocks")

      expect_length(upd$blocks, 1L)
      expect_named(upd$blocks, "mod")

      expect_length(upd$blocks$mod, 1L)
      expect_named(upd$blocks$mod, "a")
      expect_type(upd$blocks$mod, "list")

      expect_identical(
        upd$blocks$mod$a,
        list(block_name = "Test block name")
      )
    },
    args = list(
      block_id = "a",
      board = board_args(blocks = c(a = new_dataset_block())),
      update = reactiveVal()
    )
  )

  expect_error(
    testServer(
      edit_block_server(
        list(function(...) TRUE)
      ),
      NULL,
      args = list(
        block_id = "a",
        board = board_args(blocks = c(a = new_dataset_block())),
        update = reactiveVal()
      )
    ),
    class = "invalid_edit_block_server_callback_result"
  )
})

test_that("renaming a block does not loop (#181)", {

  pushed <- character()

  local_mocked_bindings(
    updateTextInput = function(session, input_id, label, value) {
      pushed <<- c(pushed, value)
    }
  )

  board_named <- function(nm) {
    blk <- new_dataset_block()
    block_name(blk) <- nm
    new_dock_board(blocks = c(a = blk))
  }

  board <- reactiveValues(board = board_named("Dataset"))

  testServer(
    edit_block_server(),
    {
      session$flushReact()

      expect_identical(pushed, "Dataset")

      n_before <- length(pushed)

      session$setInputs(block_name_in = "Renamed block")
      session$flushReact()

      expect_identical(
        update()$blocks$mod$a,
        list(block_name = "Renamed block")
      )

      # The input sync reads the applied board, not the pending request.
      # Core applies updates last (priority = -Inf), so board$board still
      # holds the old name here -> the sync does not fire and no stale name
      # is pushed back to loop (#181).
      expect_length(pushed, n_before)

      board$board <- board_named("Renamed block")
      session$flushReact()

      # Applied name now matches the input -> still no spurious push.
      expect_length(pushed, n_before)

      board$board <- board_named("External")
      session$flushReact()

      # A rename originating elsewhere still flows into the text input.
      expect_identical(pushed, c("Dataset", "External"))
    },
    args = list(
      block_id = "a",
      board = board,
      update = reactiveVal()
    )
  )
})

test_that("condition ui test", {

  insert_count <- 0L
  remove_count <- 0L

  local_mocked_bindings(
    insert_ui = function(..., ui) {
      insert_count <<- insert_count + 1L
      expect_s3_class(ui, "shiny.tag")
    },
    remove_ui = function(...) remove_count <<- remove_count + 1L
  )

  with_mock_session(
    {
      cond <- reactiveVal()

      update_blk_cond_observer(cond)

      expect_identical(insert_count, 0L)
      expect_identical(remove_count, 0L)

      session$flushReact()

      expect_identical(insert_count, 0L)
      expect_identical(remove_count, 0L)

      cond(
        list(message = "hello", warning = list("world", "bye"))
      )

      session$flushReact()

      expect_identical(insert_count, 1L)
      expect_identical(remove_count, 2L)

      cond(list(error = "oh no"))

      session$flushReact()

      expect_identical(insert_count, 2L)
      expect_identical(remove_count, 4L)
    }
  )
})

test_that("locked dock keeps block_card_toggles hidden (#122)", {

  blk <- new_dataset_block()
  attr(blk, "visible") <- "outputs"

  # Unlocked: widget renders visible, with `selected` matching saved attr
  unlocked <- withr::with_options(
    list(blockr.dock_is_locked = NULL),
    block_card_toggles(blk, NS("x"))
  )
  expect_s3_class(unlocked, "shiny.tag.list")
  unlocked_html <- as.character(htmltools::tagList(unlocked))
  expect_match(unlocked_html, 'value="outputs".*checked', fixed = FALSE)
  expect_false(grepl("display: none", unlocked_html, fixed = TRUE))

  # Locked: widget still renders (so input$collapse_blk_sections seeds
  # accordion_panel_set) but is hidden and the tooltip script is dropped
  locked <- withr::with_options(
    list(blockr.dock_is_locked = TRUE),
    block_card_toggles(blk, NS("x"))
  )
  expect_s3_class(locked, "shiny.tag")
  locked_html <- as.character(locked)
  expect_match(locked_html, "display: none", fixed = TRUE)
  expect_match(locked_html, 'value="outputs".*checked', fixed = FALSE)
  expect_false(grepl("<script", locked_html, fixed = TRUE))
})

test_that("block_cond_buckets drops status-phase rows from warnings (#290)", {

  df <- data.frame(
    block = "a",
    phase = c("status", "eval", "data"),
    severity = c("warning", "warning", "error"),
    message = c("waiting note", "real warning", "real error"),
    id = c("s1", "w1", "e1"),
    stringsAsFactors = FALSE
  )

  buckets <- block_cond_buckets(df)

  expect_named(buckets, c("error", "warning", "message"))

  # The status-phase explanation is not painted as a warning ...
  expect_identical(buckets$warning, "real warning")

  # ... while genuine warnings and errors still surface.
  expect_identical(buckets$error, "real error")
  expect_identical(buckets$message, character(0))
})

test_that("block_status_style is the shared status-dot spec (#290)", {

  waiting <- block_status_style("waiting")
  expect_identical(waiting$color, "#f59e0b")
  expect_identical(waiting$size, 8L)
  expect_identical(waiting$placement, "right-bottom")
  expect_identical(waiting$label, "Waiting for a data input")

  expect_identical(block_status_style("unset")$color, "#eab308")
  expect_identical(block_status_style("failed")$color, "#dc2626")

  # `ready`, `dormant` and non-strings carry no indicator.
  for (st in list("ready", "dormant", NULL, character(), c("a", "b"))) {
    expect_null(block_status_style(st))
  }
})

test_that("block status indicator + note reflect eval status (#290)", {

  waiting_dot <- block_status_indicator("waiting")
  expect_s3_class(waiting_dot, "shiny.tag")
  expect_match(as.character(waiting_dot), "blockr-status-dot", fixed = TRUE)
  expect_match(as.character(waiting_dot), "#f59e0b", fixed = TRUE)
  expect_match(as.character(waiting_dot), "Waiting for a data input")

  expect_match(
    as.character(block_status_indicator("unset")),
    "#eab308",
    fixed = TRUE
  )
  expect_match(
    as.character(block_status_indicator("failed")),
    "#dc2626",
    fixed = TRUE
  )

  expect_match(
    as.character(block_status_note("waiting")),
    "Waiting for a data input"
  )
  expect_match(
    as.character(block_status_note("unset")),
    "Set this block's inputs",
    fixed = TRUE
  )

  # `failed` keeps the error styling, so no placeholder note.
  expect_null(block_status_note("failed"))

  # `ready`, `dormant` and an absent status carry no affordance.
  for (st in list("ready", "dormant", NULL, character(), c("a", "b"))) {
    expect_null(block_status_indicator(st))
    expect_null(block_status_note(st))
  }
})

test_that("edit block server surfaces eval status reactively (#290)", {

  status <- reactiveVal("waiting")

  board <- reactiveValues(
    board = new_dock_board(blocks = c(a = new_dataset_block())),
    eval = list(a = reactive(status()))
  )

  testServer(
    edit_block_server(),
    {
      html <- function(out) paste0(as.character(out$html), collapse = "")

      session$flushReact()

      expect_identical(blk_status(), "waiting")
      expect_match(html(output$status_indicator), "#f59e0b", fixed = TRUE)
      expect_match(html(output$status_note), "Waiting for a data input")

      status("failed")
      session$flushReact()

      # `failed` shows the dot but leaves the note empty -- the raised error
      # uses the error styling instead of a status placeholder.
      expect_identical(blk_status(), "failed")
      expect_match(html(output$status_indicator), "#dc2626", fixed = TRUE)
      expect_identical(html(output$status_note), "")

      status("ready")
      session$flushReact()

      # A ready block carries no status affordance at all.
      expect_identical(blk_status(), "ready")
      expect_identical(html(output$status_indicator), "")
      expect_identical(html(output$status_note), "")
    },
    args = list(
      block_id = "a",
      board = board,
      update = reactiveVal()
    )
  )
})
