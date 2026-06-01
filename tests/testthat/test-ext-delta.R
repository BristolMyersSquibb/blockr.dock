test_that("validate_extensions_delta accepts a controllable mod", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    extensions = new_ctrl_extension()
  )

  expect_silent(
    validate_extensions_delta(
      list(mod = list(doc_extension = list(content = "# hi"))),
      brd
    )
  )

  expect_silent(validate_extensions_delta(list(), brd))
  expect_silent(validate_extensions_delta(list(mod = NULL), brd))
})

test_that("validate_extensions_delta rejects malformed or ungated deltas", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    extensions = new_ctrl_extension()
  )

  expect_error(
    validate_extensions_delta("nope", brd),
    class = "dock_extensions_delta_invalid"
  )

  expect_error(
    validate_extensions_delta(list(add = list(x = 1)), brd),
    class = "dock_extensions_delta_invalid"
  )

  expect_error(
    validate_extensions_delta(list(mod = list(list(content = "x"))), brd),
    class = "dock_extensions_delta_unnamed"
  )

  expect_error(
    validate_extensions_delta(
      list(mod = list(nope = list(content = "x"))),
      brd
    ),
    class = "dock_extensions_delta_unknown_id"
  )

  expect_error(
    validate_extensions_delta(
      list(mod = list(doc_extension = list(bogus = 1))),
      brd
    ),
    class = "dock_extensions_delta_not_ctrl"
  )

  expect_error(
    validate_extensions_delta(
      list(mod = list(doc_extension = 1L)),
      brd
    ),
    class = "dock_extensions_delta_entry_invalid"
  )

  expect_error(
    validate_extensions_delta(
      list(mod = list(doc_extension = list())),
      brd
    ),
    class = "dock_extensions_delta_entry_empty"
  )
})

test_that("apply_extensions_mod writes changed controllable state", {

  isolate({

    content <- reactiveVal("# old")
    ext_res <- list(doc_extension = list(state = list(content = content)))

    apply_extensions_mod(
      list(doc_extension = list(content = "# new")),
      ext_res
    )

    expect_identical(content(), "# new")
  })
})

test_that("apply_extensions_mod skips writes when the value is unchanged", {

  writes <- 0L
  val <- "same"

  rv <- function(x) {
    if (missing(x)) {
      return(val)
    }
    writes <<- writes + 1L
    val <<- x
  }

  ext_res <- list(doc_extension = list(state = list(content = rv)))

  apply_extensions_mod(list(doc_extension = list(content = "same")), ext_res)
  expect_identical(writes, 0L)

  apply_extensions_mod(list(doc_extension = list(content = "changed")), ext_res)
  expect_identical(writes, 1L)
  expect_identical(val, "changed")
})
