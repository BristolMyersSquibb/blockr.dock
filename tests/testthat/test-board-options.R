test_that("each dock board option holds the value it is given, under its id", {

  opts <- list(
    compact = new_compact_option(TRUE),
    view_tabs = new_view_tabs_option(TRUE),
    sync_rails = new_sync_rails_option(FALSE)
  )

  for (id in names(opts)) {
    expect_true(is_board_option(opts[[id]]))
    expect_identical(board_option_id(opts[[id]]), id)
  }

  expect_true(board_option_value(opts[["compact"]]))
  expect_true(board_option_value(opts[["view_tabs"]]))
  expect_false(board_option_value(opts[["sync_rails"]]))
})

test_that("a dock board option made without a value holds none", {

  withr::local_options(
    blockr.compact = TRUE,
    blockr.view_tabs = TRUE,
    blockr.sync_rails = FALSE
  )

  opts <- list(
    new_compact_option(),
    new_view_tabs_option(),
    new_sync_rails_option()
  )

  for (opt in opts) {
    expect_length(opt, 0L)
    expect_null(board_option_value(opt))
  }
})

test_that("the package lists each dock board option with its module", {

  listed <- ._BLOCKR_MODULES_[["options"]]

  expect_named(listed, c("compact", "view_tabs", "sync_rails"))

  for (id in names(listed)) {
    module <- listed[[id]][["module"]]
    expect_identical(module, paste0("js/", gsub("_", "-", id), "-option.js"))
    expect_true(nzchar(system.file(module, package = "blockr.dock")))
  }
})

test_that("the schema of each dock board option ships beside its module", {

  for (id in c("compact", "view_tabs", "sync_rails")) {

    schema <- read_json_schema(
      paste0("js/", gsub("_", "-", id), "-option.schema.json"),
      package = "blockr.dock"
    )

    expect_named(schema, c("type", "description"))
    expect_identical(schema[["type"]], "boolean")
    expect_type(schema[["description"]], "character")
  }
})

test_that("R writes the value of a dock board option that a script gives", {

  board <- new_board(
    options = c(
      new_compact_option(TRUE),
      new_view_tabs_option(FALSE),
      new_sync_rails_option(FALSE)
    )
  )

  expect_identical(
    blockr_ser(board)[["board"]][["options"]],
    list(
      compact = list(value = TRUE, version = 1L),
      view_tabs = list(value = FALSE, version = 1L),
      sync_rails = list(value = FALSE, version = 1L)
    )
  )
})
