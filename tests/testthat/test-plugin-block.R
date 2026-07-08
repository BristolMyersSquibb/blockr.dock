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

test_that("block_cond_buckets keys messages by condition id (#36)", {

  df <- data.frame(
    block = "a",
    phase = c("data", "eval", "eval", "status", "eval"),
    severity = c("warning", "warning", "message", "message", "error"),
    message = c("w one", "w two", "m one", "status note", "boom"),
    id = c("id_w1", "id_w2", "id_m1", "id_status", "id_e1"),
    stringsAsFactors = FALSE
  )

  buckets <- block_cond_buckets(df)

  expect_named(buckets, c("error", "warning", "message"))

  expect_false("status note" %in% buckets$message)

  expect_identical(buckets$warning, c(id_w1 = "w one", id_w2 = "w two"))
  expect_identical(buckets$message, c(id_m1 = "m one"))
  expect_identical(buckets$error, c(id_e1 = "boom"))
})

test_that("block_cond_buckets de-duplicates repeated conditions (#36)", {

  df <- data.frame(
    block = "a",
    phase = c("data", "eval"),
    severity = "warning",
    message = "same warning",
    id = "dup_id",
    stringsAsFactors = FALSE
  )

  expect_identical(
    block_cond_buckets(df)$warning,
    c(dup_id = "same warning")
  )
})

test_that("condition UI updates surgically by condition id (#36)", {

  inserted <- character()
  removed <- character()

  local_mocked_bindings(
    insert_ui = function(selector, ..., ui) {
      id <- ui$attribs$id
      if (is.null(id)) id <- NA_character_
      inserted[[length(inserted) + 1L]] <<- id
      expect_s3_class(ui, "shiny.tag")
    },
    remove_ui = function(selector, ...) {
      removed[[length(removed) + 1L]] <<- selector
    }
  )

  with_mock_session(
    {
      cond <- reactiveVal()

      update_blk_cond_observer(cond)

      session$flushReact()

      expect_length(inserted, 0L)
      expect_length(removed, 0L)

      cond(
        list(
          error = character(),
          warning = c(a1 = "first warning", b2 = "second warning"),
          message = c(c3 = "a message")
        )
      )

      session$flushReact()

      expect_length(inserted, 3L)
      expect_length(removed, 0L)
      expect_true(any(grepl("cond_warning_a1", inserted, fixed = TRUE)))
      expect_true(any(grepl("cond_message_c3", inserted, fixed = TRUE)))

      cond(
        list(
          error = c(d4 = "boom"),
          warning = c(a1 = "first warning"),
          message = c(c3 = "a message")
        )
      )

      session$flushReact()

      expect_length(inserted, 4L)
      expect_length(removed, 1L)
      expect_true(any(grepl("cond_error_d4", inserted, fixed = TRUE)))
      expect_match(removed[[1L]], "cond_warning_b2")
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
  expect_identical(buckets$warning, c(w1 = "real warning"))

  # ... while genuine warnings and errors still surface, keyed by id.
  expect_identical(buckets$error, c(e1 = "real error"))
  expect_length(buckets$message, 0L)
})

test_that("block_status_style is the shared status-dot spec (#290)", {

  waiting <- block_status_style("waiting")
  expect_identical(waiting$color, "#f59e0b")
  expect_identical(waiting$size, 8L)
  expect_identical(waiting$ring, 2L)
  expect_identical(waiting$ring_color, "#ffffff")
  expect_identical(waiting$label, "Waiting for a data input")

  expect_identical(block_status_style("unset")$color, "#eab308")
  expect_identical(block_status_style("failed")$color, "#dc2626")

  # `ready`, `dormant` and non-strings carry no indicator.
  for (st in list("ready", "dormant", NULL, character(), c("a", "b"))) {
    expect_null(block_status_style(st))
  }
})

test_that("block_status_badge is the shared badge derivation (#314)", {

  # Attention states pass through to the shared style spec.
  expect_identical(block_status_badge("waiting"), block_status_style("waiting"))
  expect_identical(block_status_badge("unset"), block_status_style("unset"))
  expect_identical(block_status_badge("failed"), block_status_style("failed"))

  # An error condition promotes the badge to `failed`, catching a render-phase
  # error that leaves the eval status `ready`.
  expect_identical(
    block_status_badge("ready", 2L),
    block_status_style("failed")
  )

  # `ready` and an absent status carry no badge.
  expect_null(block_status_badge("ready"))
  expect_null(block_status_badge(NULL))

  # `dormant` is indeterminate: `NA` tells a persistent renderer to keep the
  # existing badge rather than clear it.
  expect_identical(block_status_badge("dormant"), NA)
})

test_that("block status indicator + note reflect eval status (#290)", {

  waiting_dot <- block_status_indicator("waiting")
  expect_s3_class(waiting_dot, "shiny.tag")
  expect_match(as.character(waiting_dot), "blockr-status-dot", fixed = TRUE)
  expect_match(as.character(waiting_dot), "#f59e0b", fixed = TRUE)
  expect_match(as.character(waiting_dot), "Waiting for a data input")
  # The white ring is carried inline from the shared spec, not the CSS.
  expect_match(as.character(waiting_dot), "0 0 0 2px #ffffff", fixed = TRUE)

  # An error condition reddens the dot even when the eval status is `ready`,
  # matching the DAG node badge.
  expect_match(
    as.character(block_status_indicator("ready", 2L)),
    "#dc2626",
    fixed = TRUE
  )
  expect_null(block_status_indicator("ready"))

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

  empty_conds <- data.frame(
    block = character(), phase = character(), severity = character(),
    message = character(), id = character()
  )
  block_conds <- reactiveVal(empty_conds)

  board <- reactiveValues(
    board = new_dock_board(blocks = c(a = new_dataset_block())),
    eval = list(a = reactive(status())),
    blocks = list(a = list(server = list(conditions = block_conds)))
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

      # A ready block with no conditions carries no status affordance at all.
      expect_identical(blk_status(), "ready")
      expect_identical(html(output$status_indicator), "")
      expect_identical(html(output$status_note), "")

      # A render-phase error leaves the eval status `ready` but still reddens
      # the dot, matching the DAG node badge (the note stays empty).
      block_conds(
        data.frame(
          block = "a", phase = "render", severity = "error",
          message = "boom", id = "e1", stringsAsFactors = FALSE
        )
      )
      session$flushReact()

      expect_identical(blk_status(), "ready")
      expect_match(html(output$status_indicator), "#dc2626", fixed = TRUE)
      expect_identical(html(output$status_note), "")
    },
    args = list(
      block_id = "a",
      board = board,
      update = reactiveVal()
    )
  )
})
