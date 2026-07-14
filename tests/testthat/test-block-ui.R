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
        ),
        visibility = fake_visibility("a")
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

test_that("built_cards reads the built set off the required channel", {

  vis <- fake_visibility(c("a", "b", "c"))
  expect_identical(built_cards(vis), character())

  vis$required[["a"]](FALSE)
  vis$required[["b"]](TRUE)
  expect_setequal(built_cards(vis), c("a", "b"))
})

test_that("built_cards on an empty bundle is empty (no crash)", {

  # An empty board has no slots; the built set comes back empty, not a choke --
  # else the board server aborts at seed and the app never stabilises.
  expect_identical(built_cards(fake_visibility()), character())
})

test_that("mark_cards_built marks new cards built (required FALSE)", {

  vis <- fake_visibility(c("a", "b", "c"))

  mark_cards_built(vis, c("a", "b"))

  expect_identical(isolate(vis$required[["a"]]()), FALSE)
  expect_identical(isolate(vis$required[["b"]]()), FALSE)
  expect_setequal(built_cards(vis), c("a", "b"))
  # An untouched slot stays NA (never built).
  expect_true(is.na(isolate(vis$required[["c"]]())))
})

test_that("show_cards sets required TRUE on screen, FALSE off, over built", {

  vis <- fake_visibility(c("a", "b", "c"))

  # a, b, c built; only b is on screen.
  show_cards(vis, built = c("a", "b", "c"), on_screen = "b")

  expect_identical(isolate(vis$required[["a"]]()), FALSE)
  expect_identical(isolate(vis$required[["b"]]()), TRUE)
  expect_identical(isolate(vis$required[["c"]]()), FALSE)
})

test_that("show_cards clears the visible slot of a block leaving the screen", {

  vis <- fake_visibility(c("a", "b"))
  # a was painted into a view; it now leaves the screen.
  vis$required[["a"]](TRUE)
  vis$visible[["a"]]("main")

  show_cards(vis, built = c("a", "b"), on_screen = "b")

  expect_identical(isolate(vis$required[["a"]]()), FALSE)
  expect_true(is.na(isolate(vis$visible[["a"]]())))
})

test_that("show_cards leaves the visible axis alone for on-screen blocks", {

  vis <- fake_visibility("a")
  vis$required[["a"]](TRUE)
  vis$visible[["a"]]("main")

  # a stays on screen: its paint (visible) is the arrange observer's to own.
  show_cards(vis, built = "a", on_screen = "a")

  expect_identical(isolate(vis$visible[["a"]]()), "main")
})

test_that("mark_cards_rendered writes the view id into on-screen slots", {

  vis <- fake_visibility(c("a", "b", "c"))

  # The client arranged view "v1"; c is off screen (another view).
  mark_cards_rendered(vis, on_screen = c("a", "b"), view = "v1")

  expect_identical(isolate(vis$visible[["a"]]()), "v1")
  expect_identical(isolate(vis$visible[["b"]]()), "v1")
  expect_true(is.na(isolate(vis$visible[["c"]]())))
})

test_that("build_block_ui inserts the unbuilt cards and marks them built", {

  inserted <- character()

  local_mocked_bindings(
    insertUI = function(selector, ...) {
      inserted <<- c(inserted, as.character(selector))
      invisible()
    }
  )

  board <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block())
  )
  vis <- fake_visibility(c("a", "b"))

  new <- build_block_ui(
    "test", board, board_blocks(board), vis,
    edit_ui = edit_block_ui, session = MockShinySession$new()
  )

  expect_setequal(new, c("a", "b"))
  expect_setequal(built_cards(vis), c("a", "b"))
  expect_identical(isolate(vis$required[["a"]]()), FALSE)
  expect_length(inserted, 2L)

  # A card already built is not re-inserted: the required channel is the guard.
  inserted <- character()
  again <- build_block_ui(
    "test", board, board_blocks(board), vis,
    edit_ui = edit_block_ui, session = MockShinySession$new()
  )

  expect_identical(again, character())
  expect_length(inserted, 0L)
})

test_that("build_block_ui inserts the incremental card only", {

  inserted <- character()

  local_mocked_bindings(
    insertUI = function(selector, ...) {
      inserted <<- c(inserted, as.character(selector))
      invisible()
    }
  )

  board <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block())
  )
  vis <- fake_visibility(c("a", "b"))
  # a is already built and painted.
  vis$required[["a"]](TRUE)
  vis$visible[["a"]]("main")

  new <- build_block_ui(
    "test", board, board_blocks(board), vis,
    edit_ui = edit_block_ui, session = MockShinySession$new()
  )

  expect_identical(new, "b")
  expect_setequal(built_cards(vis), c("a", "b"))
  # The pre-existing card keeps its state; only the new one is added.
  expect_identical(isolate(vis$required[["a"]]()), TRUE)
  expect_identical(isolate(vis$visible[["a"]]()), "main")
  expect_identical(isolate(vis$required[["b"]]()), FALSE)
  expect_length(inserted, 1L)
})

test_that("ensure_block_ui short-circuits when every card is built", {

  local_mocked_bindings(
    build_block_ui = function(...) stop("built an already-present card")
  )

  board <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block())
  )
  vis <- fake_visibility(c("a", "b"))
  mark_cards_built(vis, c("a", "b"))

  expect_identical(
    ensure_block_ui("test", board, board_blocks(board), vis),
    character()
  )
})

test_that("ensure_block_ui derives the edit plugin from the board", {

  seen <- NULL

  local_mocked_bindings(
    build_block_ui = function(id, x, blocks, visibility, ..., edit_ui,
                              ctrl_ui = NULL, session = NULL) {
      seen <<- list(id = id, blocks = names(blocks), edit_ui = edit_ui)
      mark_cards_built(visibility, names(blocks))
      invisible(names(blocks))
    }
  )

  board <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block())
  )
  vis <- fake_visibility(c("a", "b"))

  ensure_block_ui("test", board, board_blocks(board), vis)

  expect_identical(seen$id, "test")
  expect_setequal(seen$blocks, c("a", "b"))
  expect_identical(seen$edit_ui, board_plugins(board)[["edit_block"]])
})

test_that("remove_block_ui removes the card, leaving the slot to core", {

  removed <- character()

  local_mocked_bindings(
    block_panel_ids = function(...) character(),
    removeUI = function(selector, ...) {
      removed <<- c(removed, as.character(selector))
      invisible()
    }
  )

  board <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block())
  )
  vis <- fake_visibility(c("a", "b"))
  mark_cards_built(vis, c("a", "b"))

  dock <- list(proxy = "PROXY", visibility = vis)

  remove_block_ui(
    "test", board, "a", dock, session = MockShinySession$new()
  )

  # The dock removes the DOM card only. Core's rm_vis_slots prunes the slot
  # (dropping it from the ledger); the dock must not touch the channel.
  expect_length(removed, 1L)
  expect_setequal(built_cards(vis), c("a", "b"))
})
