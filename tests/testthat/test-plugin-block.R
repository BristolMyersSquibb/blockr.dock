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
      expect_s3_class(upd$blocks$mod, "blocks")

      expect_identical(
        block_name(upd$blocks$mod[[1L]]),
        "Test block name"
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
