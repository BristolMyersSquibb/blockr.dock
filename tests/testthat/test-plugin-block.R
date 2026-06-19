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
    list(blockr.locked = NULL),
    block_card_toggles(blk, NS("x"))
  )
  expect_s3_class(unlocked, "shiny.tag.list")
  unlocked_html <- as.character(htmltools::tagList(unlocked))
  expect_match(unlocked_html, 'value="outputs".*checked', fixed = FALSE)
  expect_false(grepl("display: none", unlocked_html, fixed = TRUE))

  # Locked: widget still renders (so input$collapse_blk_sections seeds
  # accordion_panel_set) but is hidden and the tooltip script is dropped
  locked <- withr::with_options(
    list(blockr.locked = TRUE),
    block_card_toggles(blk, NS("x"))
  )
  expect_s3_class(locked, "shiny.tag")
  locked_html <- as.character(locked)
  expect_match(locked_html, "display: none", fixed = TRUE)
  expect_match(locked_html, 'value="outputs".*checked', fixed = FALSE)
  expect_false(grepl("<script", locked_html, fixed = TRUE))
})
