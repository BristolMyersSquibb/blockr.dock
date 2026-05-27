test_that("add stack action chains via keep_or_hide_sidebar on confirm", {
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

  r_board <- reactiveValues(
    board = new_board(
      c(
        a = new_dataset_block("iris"),
        b = new_head_block()
      )
    ),
    board_id = "my_board"
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        add_stack_action(
          trigger = reactive(TRUE),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()
      session$setInputs(
        stack_confirm = 1L,
        stack_id = "s1",
        stack_name = "Stack 1",
        stack_block_selection = c("a", "b"),
        stack_color = "#ff0000"
      )

      expect_length(r_update(), 1L)
      expect_length(keep_calls, 1L)
      expect_identical(keep_calls[[1L]]$id, "my_board-actions_sidebar")
      expect_identical(keep_calls[[1L]]$args$title, "Create new stack")
    }
  )
})

test_that("locked dock refuses add_stack_action mutation (#127)", {
  withr::local_options(blockr.dock_is_locked = TRUE)

  r_board <- reactiveValues(
    board = new_board(c(a = new_dataset_block("iris")))
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        add_stack_action(
          trigger = reactive(TRUE),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()
      session$setInputs(
        stack_confirm = 1L,
        stack_id = "test",
        stack_block_selection = "",
        stack_color = "#FFFFFF"
      )
      expect_length(r_update(), 0L)
    }
  )
})

test_that("add stack action", {

  r_board <- reactiveValues(
    board = new_board(
      c(
        a = new_dataset_block("iris"),
        b = new_head_block()
      )
    )
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        add_stack_action(
          trigger = reactive(TRUE),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()
      expect_length(r_update(), 0L)

      session$setInputs(
        stack_confirm = 1L,
        stack_id = ""
      )

      expect_length(r_update(), 0L)

      session$setInputs(
        stack_confirm = 2L,
        stack_id = "test"
      )

      expect_length(r_update(), 0L)

      session$setInputs(
        stack_confirm = 3L,
        stack_id = "test",
        stack_block_selection = "test"
      )

      expect_length(r_update(), 0L)

      session$setInputs(
        stack_confirm = 4L,
        stack_id = "test",
        stack_block_selection = "",
        stack_color = "test"
      )

      expect_length(r_update(), 0L)

      session$setInputs(
        stack_confirm = 5L,
        stack_id = "test",
        stack_block_selection = "",
        stack_color = "#FFFFFF"
      )

      upd <- r_update()

      expect_length(upd, 1L)
      expect_named(upd, "stacks")

      expect_length(upd$stacks, 1L)
      expect_named(upd$stacks, "add")

      expect_length(upd$stacks$add, 1L)
      expect_named(upd$stacks$add, "test")
      expect_s3_class(upd$stacks$add, "stacks")
    }
  )
})

test_that("edit stack action", {

  r_board <- reactiveValues(
    board = new_dock_board(
      c(
        a = new_dataset_block("iris"),
        b = new_head_block()
      ),
      stacks = stacks(a = "a")
    )
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        edit_stack_action(
          trigger = reactive("a"),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()
      expect_length(r_update(), 0L)

      session$setInputs(
        edit_stack_confirm = 1L,
        edit_stack_blocks = "test"
      )

      expect_length(r_update(), 0L)

      session$setInputs(
        edit_stack_confirm = 2L,
        edit_stack_blocks = "",
        edit_stack_color = "test"
      )

      expect_length(r_update(), 0L)

      session$setInputs(
        edit_stack_confirm = 3L,
        edit_stack_blocks = "",
        edit_stack_color = "#FFFFFF",
        edit_stack_name = ""
      )

      expect_length(r_update(), 0L)

      session$setInputs(
        edit_stack_confirm = 4L,
        edit_stack_blocks = "",
        edit_stack_color = "#FFFFFF",
        edit_stack_name = "Test stack"
      )

      upd <- r_update()

      expect_length(upd, 1L)
      expect_named(upd, "stacks")

      expect_length(upd$stacks, 1L)
      expect_named(upd$stacks, "mod")

      expect_length(upd$stacks$mod, 1L)
      expect_named(upd$stacks$mod, "a")
      expect_s3_class(upd$stacks$mod, "stacks")
    }
  )
})

test_that("remove stack action", {

  r_board <- reactiveValues(
    board = new_dock_board(
      c(
        a = new_dataset_block("iris"),
        b = new_head_block()
      ),
      stacks = stacks(a = "a")
    )
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        remove_stack_action(
          trigger = reactive("a"),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()

      upd <- r_update()

      expect_length(upd, 1L)
      expect_named(upd, "stacks")

      expect_length(upd$stacks, 1L)
      expect_named(upd$stacks, "rm")

      expect_length(upd$stacks$rm, 1L)
      expect_identical(upd$stacks$rm, "a")
    }
  )
})
