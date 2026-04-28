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

test_that("suggest_new_colors falls back when colorspace is unavailable", {

  with_mocked_bindings(
    {
      cols <- suggest_new_colors(n = length(fallback_palette) + 2L)

      expect_identical(cols[seq_along(fallback_palette)], fallback_palette)
      expect_identical(
        cols[length(fallback_palette) + seq_len(2L)],
        fallback_palette[seq_len(2L)]
      )
    },
    pkg_avail = function(...) FALSE
  )
})
