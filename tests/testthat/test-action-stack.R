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
