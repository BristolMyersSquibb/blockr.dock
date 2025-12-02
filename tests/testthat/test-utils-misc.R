test_that("suggest_new_colors", {

  n <- 10

  col <- character()

  for (i in seq_len(n)) {
    col <- c(col, suggest_new_colors(col))
  }

  expect_identical(
    col,
    suggest_new_colors(n = n)
  )

  cur <- suggest_new_colors(n = 5)

  expect_identical(
    suggest_new_colors(cur),
    suggest_new_colors(rev(cur))
  )
})
