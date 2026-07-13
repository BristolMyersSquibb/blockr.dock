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
        )
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

test_that("card_visibility seeds the built set: fronts required, rest parked", {

  # `b` is the active view's front panel; `a` a background tab, `c` off screen.
  # Seed has nothing rendered yet -- the arrange observer promotes later.
  expect_identical(
    card_visibility(c("a", "b", "c"), on_screen = "b"),
    c(a = "parked", b = "required", c = "parked")
  )
})

test_that("card_visibility on an empty built set is empty (no order(NULL))", {

  # An empty board has no built cards; the seed map must come back empty, not
  # choke -- else the board server aborts at seed and the app never stabilises.
  expect_length(card_visibility(character(), character()), 0L)
})

test_that("show_cards reconciles membership but never demotes rendered", {

  visible <- reactiveVal(c(a = "rendered", b = "parked", c = "required"))

  # b comes on screen, c leaves. a stays on screen AND stays rendered -- the
  # arrange observer owns that promotion; membership must not undo it.
  show_cards(visible, on_screen = c("a", "b"))

  expect_identical(
    isolate(visible()),
    c(a = "rendered", b = "required", c = "parked")
  )
})

test_that("mark_cards_rendered promotes the on-screen blocks only", {

  visible <- reactiveVal(c(a = "required", b = "required", c = "parked"))

  # The client arranged a's and b's view; c is off screen (another view).
  mark_cards_rendered(visible, on_screen = c("a", "b"))

  expect_identical(
    isolate(visible()),
    c(a = "rendered", b = "rendered", c = "parked")
  )
})

test_that("built_cards reads the built set off the channel", {

  visible <- reactiveVal()
  expect_identical(built_cards(visible), character())

  visible(c(a = "parked", b = "rendered"))
  expect_setequal(built_cards(visible), c("a", "b"))
})

test_that("mark_cards_built / drop_cards edit the channel in place", {

  visible <- reactiveVal()

  mark_cards_built(visible, c("a", "b"))
  expect_identical(isolate(visible()), c(a = "parked", b = "parked"))

  mark_cards_built(visible, "c")
  expect_setequal(built_cards(visible), c("a", "b", "c"))

  drop_cards(visible, "a")
  expect_setequal(built_cards(visible), c("b", "c"))
})

test_that("build_block_ui inserts the unbuilt cards, records them on channel", {

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
  visible <- reactiveVal()

  new <- build_block_ui(
    "test", board, board_blocks(board), visible,
    edit_ui = edit_block_ui, session = MockShinySession$new()
  )

  expect_setequal(new, c("a", "b"))
  expect_identical(isolate(visible()), c(a = "parked", b = "parked"))
  expect_length(inserted, 2L)

  # A card already built is not re-inserted: the channel is the guard.
  inserted <- character()
  again <- build_block_ui(
    "test", board, board_blocks(board), visible,
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
  visible <- reactiveVal(c(a = "rendered"))

  new <- build_block_ui(
    "test", board, board_blocks(board), visible,
    edit_ui = edit_block_ui, session = MockShinySession$new()
  )

  expect_identical(new, "b")
  expect_setequal(built_cards(visible), c("a", "b"))
  # The pre-existing card keeps its state; only the new one is added parked.
  expect_identical(isolate(visible())[["a"]], "rendered")
  expect_identical(isolate(visible())[["b"]], "parked")
  expect_length(inserted, 1L)
})

test_that("ensure_block_ui short-circuits when every card is built", {

  local_mocked_bindings(
    build_block_ui = function(...) stop("built an already-present card")
  )

  board <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block())
  )
  visible <- reactiveVal(c(a = "parked", b = "parked"))

  expect_identical(
    ensure_block_ui("test", board, board_blocks(board), visible),
    character()
  )
})

test_that("ensure_block_ui derives the edit plugin from the board", {

  seen <- NULL

  local_mocked_bindings(
    build_block_ui = function(id, x, blocks, visible, ..., edit_ui,
                              ctrl_ui = NULL, session = NULL) {
      seen <<- list(id = id, blocks = names(blocks), edit_ui = edit_ui)
      mark_cards_built(visible, names(blocks))
      invisible(names(blocks))
    }
  )

  board <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block())
  )
  visible <- reactiveVal()

  ensure_block_ui("test", board, board_blocks(board), visible)

  expect_identical(seen$id, "test")
  expect_setequal(seen$blocks, c("a", "b"))
  expect_identical(seen$edit_ui, board_plugins(board)[["edit_block"]])
})

test_that("remove_block_ui drops the removed cards from the channel", {

  local_mocked_bindings(
    block_panel_ids = function(...) character(),
    removeUI = function(...) invisible()
  )

  board <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block())
  )
  visible <- reactiveVal(c(a = "rendered", b = "rendered"))

  dock <- list(proxy = "PROXY", visible = visible)

  remove_block_ui(
    "test", board, "a", dock, session = MockShinySession$new()
  )

  # The card is gone, so its id must leave the channel -- otherwise a later
  # block reusing the id would never be rebuilt.
  expect_setequal(built_cards(visible), "b")
})
