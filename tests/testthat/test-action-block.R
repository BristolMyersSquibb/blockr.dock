test_that("add block action", {

  ab_action <- add_block_action(reactive(TRUE))

  testServer(
    function(id, ...) {
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
        add_block_id = ""
      )

      expect_length(update(), 0L)

      tmp <- blk()
      blk(NULL)

      session$setInputs(
        add_block_confirm = 1L,
        add_block_id = "test"
      )

      expect_length(update(), 0L)

      blk(tmp)

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

test_that("append block action", {

  ap_action <- append_block_action(reactive("a"))

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        ap_action(
          board = reactiveValues(
            board = new_board(blocks = c(a = new_dataset_block()))
          ),
          update = reactiveVal(list()),
          domain = MockShinySession$new()
        )
      )
    },
    {
      session$flushReact()
      expect_null(blk())

      session$setInputs(append_block_selection = "dataset_block")
      expect_null(blk())

      session$setInputs(append_block_selection = "head_block")
      expect_s3_class(blk(), "block")

      expect_length(update(), 0L)

      session$setInputs(
        append_block_confirm = 1L,
        append_block_id = ""
      )

      expect_length(update(), 0L)

      session$setInputs(
        append_block_confirm = 1L,
        append_block_id = "test",
        append_link_id = ""
      )

      expect_length(update(), 0L)

      tmp <- blk()
      blk(NULL)

      session$setInputs(
        append_block_confirm = 1L,
        append_block_id = "test",
        append_link_id = "test"
      )

      expect_length(update(), 0L)

      blk(tmp)

      session$setInputs(
        append_block_confirm = 1L,
        append_block_id = "test",
        append_link_id = "test",
        append_block_name = "Test block"
      )

      upd <- update()

      expect_length(upd, 2L)
      expect_named(upd, c("blocks", "links"))

      expect_length(upd$blocks, 1L)
      expect_named(upd$blocks, "add")

      expect_length(upd$blocks$add, 1L)
      expect_named(upd$blocks$add, "test")
      expect_s3_class(upd$blocks$add, "blocks")

      expect_length(upd$links, 1L)
      expect_named(upd$links, "add")

      expect_length(upd$links$add, 1L)
      expect_named(upd$links$add, "test")
      expect_s3_class(upd$links$add, "links")
    }
  )
})

test_that("remove block action", {

  ap_action <- remove_block_action(reactive("a"))

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        ap_action(
          board = reactiveValues(
            board = new_board(blocks = c(a = new_dataset_block()))
          ),
          update = reactiveVal(list()),
          domain = MockShinySession$new()
        )
      )
    },
    {
      expect_length(update(), 0L)

      session$flushReact()

      upd <- update()

      expect_length(upd, 1L)
      expect_named(upd, "blocks")

      expect_length(upd$blocks, 1L)
      expect_named(upd$blocks, "rm")

      expect_length(upd$blocks$rm, 1L)
      expect_identical(upd$blocks$rm, "a")
    }
  )
})
