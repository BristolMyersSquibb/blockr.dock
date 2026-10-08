test_that("remove stack action", {
  r_board <- reactiveValues(
    board = new_dock_board(
      c(a = new_dataset_block("iris"), b = new_head_block()),
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
      expect_named(upd$stacks, "rm")
      expect_identical(upd$stacks$rm, "a")
    }
  )
})
