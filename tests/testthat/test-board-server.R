test_that("board server", {

  # Multi-view path is exercised by the default `dock_layouts(Page = ...)`
  # layout — `board_server_callback` returns an extra `view_data` reactive.
  board_rv_1 <- board_args(
    blocks = c(a = new_dataset_block())
  )

  with_mock_session(
    {
      res <- board_server_callback(board_rv_1, update = reactiveVal())

      expect_type(res, "list")
      expect_named(res, c("dock", "actions", "view_data"))

      dock <- res[["dock"]]

      expect_type(dock, "list")
      expect_named(
        dock,
        c("layout", "proxy", "prev_active_group", "n_panels",
          "active_group_trail")
      )

      expect_s3_class(dock[["layout"]], "reactive")
      expect_s3_class(dock[["proxy"]], "dock_view_proxy")
      expect_s3_class(dock[["prev_active_group"]], "reactive")
      expect_s3_class(res[["view_data"]], "reactive")
    }
  )

  board_rv_2 <- board_args(
    blocks = c(a = new_dataset_block()),
    extensions = new_edit_board_extension()
  )

  with_mock_session(
    {
      res <- board_server_callback(board_rv_2, update = reactiveVal())

      expect_type(res, "list")
      expect_named(
        res,
        c("dock", "actions", "view_data", "edit_board_extension")
      )

      dock <- res[["dock"]]

      expect_type(dock, "list")
      expect_named(
        dock,
        c("layout", "proxy", "prev_active_group", "n_panels",
          "active_group_trail")
      )

      expect_s3_class(dock[["layout"]], "reactive")
      expect_s3_class(dock[["proxy"]], "dock_view_proxy")
      expect_s3_class(dock[["prev_active_group"]], "reactive")

      ext <- res[["edit_board_extension"]]

      expect_type(ext, "list")
      expect_length(ext, 1L)
      expect_named(ext, "state")

      expect_identical(ext[["state"]], list())
    }
  )

  # manage_dock is now a moduleServer — use with_mock_context directly
  # since with_mock_session's env cloning breaks nested moduleServer observers
  mod_input <- function(name) paste0("dock_main-", name)

  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  res <- with_mock_context(ms, {
    manage_dock("dock_main", board_rv_2, update = reactiveVal(),
                actions = list())
  })

  ms$flushReact()

  n_panels <- res$n_panels

  do.call(
    ms$setInputs,
    set_names(list(TRUE), mod_input(dock_input("initialized")))
  )

  expect_identical(isolate(n_panels()), 2L)

  do.call(
    ms$setInputs,
    set_names(
      list(as_block_panel_id("a")),
      mod_input(dock_input("panel-to-remove"))
    )
  )

  expect_identical(isolate(n_panels()), 1L)

  do.call(
    ms$setInputs,
    set_names(
      list(as_ext_panel_id("edit_board_extension")),
      mod_input(dock_input("panel-to-remove"))
    )
  )

  expect_identical(isolate(n_panels()), 0L)

  do.call(
    ms$setInputs,
    set_names(
      list(1L, c("blk-a", "ext-edit_board_extension")),
      c(mod_input("confirm_add"), mod_input("add_dock_panel"))
    )
  )

  expect_identical(isolate(n_panels()), 2L)

  ms2 <- new_mock_session()
  withr::defer(if (!ms2$isClosed()) ms2$close())

  res2 <- with_mock_context(ms2, {
    manage_dock("dock_main", board_rv_2, update = reactiveVal(),
                actions = list())
  })

  ms2$flushReact()

  prevs <- res2$prev_active_group
  trail <- res2$active_group_trail

  expect_null(isolate(prevs()))
  expect_null(isolate(trail()))

  do.call(
    ms2$setInputs,
    set_names(list("1"), mod_input(dock_input("active-group")))
  )

  expect_null(isolate(prevs()))
  expect_identical(isolate(trail()), "1")

  do.call(
    ms2$setInputs,
    set_names(list("2"), mod_input(dock_input("active-group")))
  )

  expect_identical(isolate(prevs()), "1")
  expect_identical(isolate(trail()), "2")

  do.call(
    ms2$setInputs,
    set_names(list("2"), mod_input(dock_input("active-group")))
  )

  expect_identical(isolate(prevs()), "1")
  expect_identical(isolate(trail()), "2")

  do.call(
    ms2$setInputs,
    set_names(list("1"), mod_input(dock_input("active-group")))
  )

  expect_identical(isolate(prevs()), "2")
  expect_identical(isolate(trail()), "1")

  ms3 <- new_mock_session()
  withr::defer(if (!ms3$isClosed()) ms3$close())

  upd <- reactiveVal()

  with_mock_context(ms3, {
    manage_dock("dock_main", board_rv_2, update = upd, actions = list())
  })

  ms3$flushReact()

  with_mock_context(ms3, {
    upd(
      list(
        blocks = list(
          mod = blocks(a = new_dataset_block(block_name = "Test block"))
        )
      )
    )
  })

  with_mocked_bindings(
    ms3$flushReact(),
    get_dock_panel = function(...) list(title = "Old title")
  )
})
