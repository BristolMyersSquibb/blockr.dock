test_that("edit inputs, retired, says where to go instead", {
  notes <- list()
  local_mocked_bindings(
    notify = function(...) notes[[length(notes) + 1L]] <<- list(...)
  )
  r_update <- reactiveVal(list())
  testServer(
    function(id, ...) {
      moduleServer(
        id,
        edit_inputs_action(
          trigger = reactive("b"),
          board = reactiveValues(board = new_board(), board_id = "brd"),
          update = r_update
        )
      )
    },
    {
      session$flushReact()
      expect_length(notes, 1L)
      expect_match(notes[[1L]][[1L]], "Connect to", fixed = TRUE)
      expect_length(r_update(), 0L)
    }
  )
})
