test_that("the script binds each block's inputs, in link order", {

  board <- new_dock_board(
    blocks = c(a = new_dataset_block("iris"), b = new_head_block(n = 3L)),
    links = list(from = "a", to = "b")
  )

  exprs <- list(
    b = quote(utils::head(.(data), n = 3L)),
    a = quote(datasets::iris)
  )

  parts <- export_code(exprs, board)

  lines <- Map(
    function(id, expr, args, type) {
      paste(deparse(script_line(id, expr, args, type)), collapse = "\n")
    },
    names(parts$exprs),
    parts$exprs,
    parts$args,
    parts$types
  )

  expect_identical(names(lines), c("a", "b"))
  expect_match(lines$a, "^a <- local\\(")
  expect_match(lines$b, "^b <- ")
  expect_match(lines$b, "utils::head(a, n = 3L)", fixed = TRUE)
})

test_that("the code page waits for the build and for every block's state", {

  blk <- function(ready, expr = quote(1)) {
    list(
      server = list(
        state_ready = reactiveVal(ready),
        expr = reactive(expr)
      )
    )
  }

  board <- function(blocks, built = names(blocks), errors = character()) {
    x <- new_dock_board(
      blocks = c(a = new_dataset_block(), b = new_head_block())
    )
    list(
      board = x,
      blocks = blocks[built],
      conditions = data.frame(
        severity = errors,
        stringsAsFactors = FALSE
      )
    )
  }

  isolate({
    expect_identical(
      code_state(board(list(a = blk(TRUE), b = blk(TRUE)), built = "a")),
      "pending"
    )
    expect_identical(
      code_state(board(list(a = blk(TRUE), b = blk(FALSE)))),
      "blocked"
    )
    expect_identical(
      code_state(board(list(a = blk(TRUE), b = blk(TRUE)), errors = "error")),
      "blocked"
    )
    expect_identical(
      code_state(board(list(a = blk(TRUE), b = blk(TRUE)))),
      "ready"
    )
  })
})
