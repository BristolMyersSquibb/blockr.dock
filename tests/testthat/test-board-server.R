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

  panel_lookups <- 0L

  with_mocked_bindings(
    {
      with_mock_context(ms3, {
        upd(
          list(
            blocks = list(
              mod = list(a = list(block_name = "Test block"))
            )
          )
        )
      })
      ms3$flushReact()

      with_mock_context(ms3, {
        upd(
          list(
            blocks = list(
              mod = list(a = list(dataset = "iris"))
            )
          )
        )
      })
      ms3$flushReact()
    },
    get_dock_panel = function(...) {
      panel_lookups <<- panel_lookups + 1L
      list(title = "Old title")
    }
  )

  expect_identical(panel_lookups, 1L)
})

test_that("live_view_data is NULL while any view layout is uninitialized", {
  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  layouts <- with_mock_context(ms, {
    list(A = reactiveVal(NULL), B = reactiveVal(NULL))
  })

  res <- with_mock_context(ms, {
    vs <- reactiveValues(
      state = dock_layouts(A = new_dock_layout(), B = new_dock_layout())
    )
    dock_mgr <- new_dock_manager()
    dock_mgr$docks$A <- list(layout = layouts$A)
    dock_mgr$docks$B <- list(layout = layouts$B)
    list(vd = live_view_data(vs, dock_mgr), vs = vs)
  })

  expect_null(isolate(res$vd()))

  with_mock_context(ms, layouts$A(list(grid = list(), panels = list())))
  expect_null(isolate(res$vd()))

  with_mock_context(ms, layouts$B(list(grid = list(), panels = list())))

  lys <- isolate(res$vd())
  expect_s3_class(lys, "dock_layouts")
  expect_named(lys, c("A", "B"))
  expect_identical(active_view(lys), "A")
})

test_that("layouts_to_board_observer fires update views on divergence", {
  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = list("a"), B = list("b"))
  )

  rv <- with_mock_context(ms, reactiveValues(board = brd))
  vd <- with_mock_context(ms, reactiveVal(NULL))

  captured <- list()
  upd <- function(payload) {
    captured[[length(captured) + 1L]] <<- payload
  }

  with_mock_context(ms, layouts_to_board_observer(vd, upd, rv))
  ms$flushReact()

  expect_length(captured, 0L)

  new_views <- isolate(board_layouts(rv$board))
  active_view(new_views) <- "B"

  with_mock_context(ms, vd(new_views))
  ms$flushReact()

  expect_length(captured, 1L)
  expect_named(captured[[1L]], "views")
  expect_identical(active_view(captured[[1L]]$views), "B")
})

test_that("layouts_to_board_observer is idempotent when nothing changed", {
  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(Page = list("a"))
  )

  rv <- with_mock_context(ms, reactiveValues(board = brd))
  vd <- with_mock_context(ms, reactiveVal(NULL))

  fire_count <- 0L
  upd <- function(payload) fire_count <<- fire_count + 1L

  with_mock_context(ms, layouts_to_board_observer(vd, upd, rv))
  ms$flushReact()

  with_mock_context(ms, vd(isolate(board_layouts(rv$board))))
  ms$flushReact()

  expect_identical(fire_count, 0L)
})

test_that("apply_board_update.dock_board writes views slot to rv$board", {
  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = list("a"), B = list("b"))
  )

  rv <- with_mock_context(ms, reactiveValues(board = brd))

  new_views <- board_layouts(brd)
  active_view(new_views) <- "B"

  with_mock_context(ms, {
    apply_board_update(
      brd,
      list(views = new_views),
      rv,
      session = ms,
      edit_block = NULL,
      ctrl_block = NULL,
      edit_stack = NULL,
      edit_plugin_args = list(),
      dot_args = list()
    )
  })

  expect_identical(active_view(board_layouts(isolate(rv$board))), "B")
})

test_that("validate_board_update.dock_board rejects malformed views slot", {
  brd <- new_dock_board(blocks = c(a = new_dataset_block()))

  expect_error(
    validate_board_update(list(views = "not a dock_layouts"), brd),
    class = "dock_layouts_structure_invalid"
  )
})

test_that("views slot flows through full board_update lifecycle", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = list("a"), B = list("b"))
  )

  new_views <- board_layouts(brd)
  active_view(new_views) <- "B"

  testServer(
    blockr.core:::board_server.board,
    {
      session$flushReact()

      board_update(list(views = new_views))
      session$flushReact()

      expect_identical(active_view(board_layouts(rv$board)), "B")
      expect_null(board_update())
    },
    args = list(
      x = brd,
      plugins = list(blockr.core::manage_blocks())
    )
  )
})
