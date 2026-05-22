test_that("add block action", {

  r_board <- reactiveValues(board = new_board())
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        add_block_action(
          trigger = reactive(TRUE),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()
      expect_null(blk())

      session$setInputs(add_block_selection = "dataset_block")
      expect_s3_class(blk(), "block")

      expect_length(r_update(), 0L)

      session$setInputs(
        add_block_confirm = 1L,
        add_block_id = ""
      )

      expect_length(r_update(), 0L)

      tmp <- blk()
      blk(NULL)

      session$setInputs(
        add_block_confirm = 2L,
        add_block_id = "test"
      )

      expect_length(r_update(), 0L)

      blk(tmp)

      session$setInputs(
        add_block_confirm = 3L,
        add_block_id = "test",
        add_block_name = "Test block"
      )

      upd <- r_update()

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

  r_board <- reactiveValues(
    board = new_board(blocks = c(a = new_dataset_block()))
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        append_block_action(
          trigger = reactive("a"),
          board = r_board,
          update = r_update
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

      expect_length(r_update(), 0L)

      session$setInputs(
        append_block_confirm = 1L,
        append_block_id = ""
      )

      expect_length(r_update(), 0L)

      session$setInputs(
        append_block_confirm = 2L,
        append_block_id = "test",
        append_link_id = ""
      )

      expect_length(r_update(), 0L)

      tmp <- blk()
      blk(NULL)

      session$setInputs(
        append_block_confirm = 3L,
        append_block_id = "test",
        append_link_id = "test"
      )

      expect_length(r_update(), 0L)

      blk(tmp)

      session$setInputs(
        append_block_confirm = 4L,
        append_block_id = "test",
        append_link_id = "test",
        append_block_name = "Test block"
      )

      upd <- r_update()

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

test_that("add block action chains via keep_or_hide_sidebar on confirm", {
  # Successful confirm should fire `keep_or_hide_sidebar()` (the chain
  # branch) — the pinned-vs-unpinned decision lives inside blockr.ui, so
  # we just assert the call shape from the action handler.

  keep_calls <- list()

  local_mocked_bindings(
    show_sidebar = function(...) invisible(NULL),
    keep_or_hide_sidebar = function(id, ...) {
      keep_calls[[length(keep_calls) + 1L]] <<- list(id = id, args = list(...))
      invisible(NULL)
    },
    hide_sidebar = function(...) invisible(NULL),
    .package = "blockr.ui"
  )

  r_board <- reactiveValues(board = new_board(), board_id = "my_board")
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        add_block_action(
          trigger = reactive(TRUE),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()
      session$setInputs(add_block_selection = "dataset_block")

      session$setInputs(
        add_block_confirm = 1L,
        add_block_id = "x",
        add_block_name = "X block"
      )

      expect_length(r_update(), 1L)
      expect_length(keep_calls, 1L)
      # `sidebar_id` is composed from `board$board_id` inside the action.
      expect_identical(keep_calls[[1L]]$id, "my_board-actions_sidebar")
      expect_identical(keep_calls[[1L]]$args$title, "Add new block")
    }
  )
})

test_that("remove block action", {

  r_board <- reactiveValues(
    board = new_board(blocks = c(a = new_dataset_block()))
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        remove_block_action(
          trigger = reactive("a"),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      expect_length(r_update(), 0L)

      session$flushReact()

      upd <- r_update()

      expect_length(upd, 1L)
      expect_named(upd, "blocks")

      expect_length(upd$blocks, 1L)
      expect_named(upd$blocks, "rm")

      expect_length(upd$blocks$rm, 1L)
      expect_identical(upd$blocks$rm, "a")
    }
  )
})
