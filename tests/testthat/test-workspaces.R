test_that("dock board with workspaces", {

  blocks <- c(
    a = new_dataset_block("iris"),
    b = new_head_block(),
    c = new_dataset_block("mtcars")
  )

  links <- list(new_link("a", "b"))

  board <- new_dock_board(
    blocks = blocks,
    links = links,
    workspaces = list(
      Data = list(block_ids = c("a", "b")),
      Plots = list(block_ids = "c")
    )
  )

  expect_s3_class(board, "dock_board")
  expect_true(has_workspaces(board))

  ws <- dock_workspaces(board)
  expect_s3_class(ws, "dock_workspaces")
  expect_named(ws, c("Data", "Plots"))

  # Default permission flags
  expect_true(attr(ws, "ws_create"))
  expect_true(attr(ws, "ws_rename"))
  expect_true(attr(ws, "ws_delete"))
})

test_that("dock board without workspaces", {

  board <- new_dock_board()

  expect_false(has_workspaces(board))
  expect_null(dock_workspaces(board))
})

test_that("workspace permission flags", {

  board <- new_dock_board(
    workspaces = list(W1 = list(block_ids = character())),
    ws_create = FALSE,
    ws_rename = FALSE,
    ws_delete = TRUE
  )

  ws <- dock_workspaces(board)
  expect_false(attr(ws, "ws_create"))
  expect_false(attr(ws, "ws_rename"))
  expect_true(attr(ws, "ws_delete"))
})

test_that("workspace permission flags preserved from classed input", {

  ws <- as_dock_workspaces(
    list(W1 = list(block_ids = character())),
    ws_create = FALSE,
    ws_rename = TRUE,
    ws_delete = FALSE
  )

  # Passing already-classed workspaces preserves its flags
  board <- new_dock_board(workspaces = ws)
  ws2 <- dock_workspaces(board)
  expect_false(attr(ws2, "ws_create"))
  expect_true(attr(ws2, "ws_rename"))
  expect_false(attr(ws2, "ws_delete"))
})

test_that("parent/child workspaces", {

  blocks <- c(
    a = new_dataset_block("iris"),
    b = new_head_block(),
    c = new_dataset_block("mtcars")
  )

  board <- new_dock_board(
    blocks = blocks,
    links = list(new_link("a", "b")),
    workspaces = list(
      Analysis = list(
        children = list(
          Data = list(block_ids = c("a", "b")),
          Plots = list(block_ids = "c")
        )
      ),
      Summary = list(block_ids = "a")
    )
  )

  ws <- dock_workspaces(board)
  expect_true(is_ws_parent(ws[["Analysis"]]))
  expect_false(is_ws_parent(ws[["Summary"]]))

  leaves <- ws_leaves(ws)
  expect_named(leaves, c("Data", "Plots", "Summary"))

  leaf_names <- ws_leaf_names(ws)
  expect_equal(leaf_names, c("Data", "Plots", "Summary"))
})

test_that("disabled workspaces", {

  blocks <- c(a = new_dataset_block("iris"))

  board <- new_dock_board(
    blocks = blocks,
    workspaces = list(
      Active = list(block_ids = "a"),
      Hidden = list(block_ids = character(), disabled = TRUE)
    )
  )

  ws <- dock_workspaces(board)
  leaves <- ws_leaves(ws)

  expect_false(leaves[["Active"]][["disabled"]])
  expect_true(leaves[["Hidden"]][["disabled"]])
})

test_that("workspace validation rejects invalid block_ids", {

  blocks <- c(a = new_dataset_block("iris"))

  expect_error(
    new_dock_board(
      blocks = blocks,
      workspaces = list(
        W1 = list(block_ids = c("a", "nonexistent"))
      )
    )
  )
})

test_that("workspace validation rejects duplicate leaf names", {

  blocks <- c(a = new_dataset_block("iris"))

  expect_error(
    new_dock_board(
      blocks = blocks,
      workspaces = list(
        G1 = list(
          children = list(Dup = list(block_ids = "a"))
        ),
        Dup = list(block_ids = character())
      )
    ),
    class = "duplicate_workspace_names"
  )
})

test_that("ser/des roundtrip with flat workspaces", {

  blocks <- c(
    a = new_dataset_block("iris"),
    b = new_head_block()
  )

  board <- new_dock_board(
    blocks = blocks,
    links = list(new_link("a", "b")),
    workspaces = list(
      Data = list(block_ids = c("a", "b")),
      Empty = list(block_ids = character())
    )
  )

  restored <- blockr_deser(blockr_ser(board))

  expect_s3_class(restored, "dock_board")
  expect_true(has_workspaces(restored))

  ws <- dock_workspaces(restored)
  expect_s3_class(ws, "dock_workspaces")
  expect_named(ws, c("Data", "Empty"))

  # block_ids are character after roundtrip (not list)
  expect_identical(ws_leaves(ws)[["Data"]][["block_ids"]], c("a", "b"))
  expect_identical(ws_leaves(ws)[["Empty"]][["block_ids"]], character())
})

test_that("ser/des roundtrip with parent/child workspaces", {

  blocks <- c(
    a = new_dataset_block("iris"),
    b = new_dataset_block("mtcars")
  )

  board <- new_dock_board(
    blocks = blocks,
    workspaces = list(
      Group = list(
        children = list(
          Child1 = list(block_ids = "a"),
          Child2 = list(block_ids = "b")
        )
      )
    )
  )

  restored <- blockr_deser(blockr_ser(board))
  ws <- dock_workspaces(restored)

  expect_true(is_ws_parent(ws[["Group"]]))
  expect_named(ws[["Group"]][["children"]], c("Child1", "Child2"))

  leaves <- ws_leaves(ws)
  expect_identical(leaves[["Child1"]][["block_ids"]], "a")
  expect_identical(leaves[["Child2"]][["block_ids"]], "b")
})

test_that("ser/des roundtrip preserves permission flags", {

  board <- new_dock_board(
    workspaces = list(W1 = list(block_ids = character())),
    ws_create = FALSE,
    ws_rename = TRUE,
    ws_delete = FALSE
  )

  restored <- blockr_deser(blockr_ser(board))
  ws <- dock_workspaces(restored)

  expect_false(attr(ws, "ws_create"))
  expect_true(attr(ws, "ws_rename"))
  expect_false(attr(ws, "ws_delete"))
})

test_that("ser/des roundtrip preserves active_ws", {

  board <- new_dock_board(
    workspaces = list(
      W1 = list(block_ids = character()),
      W2 = list(block_ids = character())
    )
  )

  ws <- dock_workspaces(board)
  attr(ws, "active_ws") <- "W2"
  board[["workspaces"]] <- ws

  ser <- blockr_ser(board)
  restored <- blockr_deser(ser)
  ws2 <- dock_workspaces(restored)

  expect_identical(attr(ws2, "active_ws"), "W2")
})

test_that("ser/des roundtrip preserves disabled flag", {

  blocks <- c(a = new_dataset_block("iris"))

  board <- new_dock_board(
    blocks = blocks,
    workspaces = list(
      Active = list(block_ids = "a"),
      Hidden = list(block_ids = character(), disabled = TRUE)
    )
  )

  restored <- blockr_deser(blockr_ser(board))
  leaves <- ws_leaves(dock_workspaces(restored))

  expect_false(leaves[["Active"]][["disabled"]])
  expect_true(leaves[["Hidden"]][["disabled"]])
})

test_that("ser/des roundtrip with extensions", {

  board <- new_dock_board(
    extensions = new_edit_board_extension(),
    workspaces = list(
      W1 = list(block_ids = character())
    )
  )

  restored <- blockr_deser(blockr_ser(board))

  expect_true(has_workspaces(restored))
  expect_s3_class(dock_workspaces(restored), "dock_workspaces")

  # ext_ids are populated with all extensions by default
  leaves <- ws_leaves(dock_workspaces(restored))
  expect_true(length(leaves[["W1"]][["ext_ids"]]) > 0L)
})
