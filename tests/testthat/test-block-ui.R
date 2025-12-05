test_that("insert/remove panel test", {

  board <- new_dock_board()

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()
      expect_null(board_update())

      board_update(
        list(
          blocks = list(
            add = blocks(a = new_dataset_block())
          )
        )
      )

      with_mocked_bindings(
        session$flushReact(),
        determine_panel_pos = function(dock) {
          expect_s3_class(dock$proxy, "dock_view_proxy")
          TRUE
        }
      )

      expect_null(board_update())

      board_update(
        list(blocks = list(rm = "a"))
      )

      session$flushReact()

      expect_null(board_update())
    },
    args = list(
      x = board,
      dock = list(
        proxy = dockViewR::dock_view_proxy(
          dock_id(),
          session = MockShinySession$new()
        )
      )
    )
  )

})

test_that("dummy block ui test", {

  ui <- block_ui(
    "test",
    new_dock_board(blocks = c(a = new_dataset_block())),
    edit_block_ui
  )

  expect_type(ui, "list")
  expect_length(ui, 1L)
  expect_s3_class(ui[[1L]], "shiny.tag")
})
