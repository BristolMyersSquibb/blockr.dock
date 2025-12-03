test_that("add block action", {

  ab_action <- add_block_action(reactive(TRUE))

  testServer(
    function(id) {
      moduleServer(
        id,
        ab_action(
          board = reactiveValues(board = new_board()),
          update = reactiveVal(list()),
          domain = MockShinySession$new()
        )
      )
    },
    {
      session$flushReact()
      expect_null(blk())

      session$setInputs(add_block_selection = "dataset_block")
      expect_s3_class(blk(), "block")

      expect_length(update(), 0L)
      session$setInputs(
        add_block_confirm = 1L,
        add_block_id = "test",
        add_block_name = "Test block"
      )

      upd <- update()

      expect_length(upd, 1L)
      expect_named(upd, "blocks")

      expect_length(upd$blocks, 1L)
      expect_named(upd$blocks, "add")

      expect_length(upd$blocks$add, 1L)
      expect_named(upd$blocks$add, "test")
      expect_s3_class(upd$blocks$add, "blocks")
    }
  )
})
