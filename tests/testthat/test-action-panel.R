# The panel ops share their dispatch with the add-panel modal and the
# tab-close / restore paths (board-server.R), so these tests drive the
# dispatch helpers directly (mocking the show / hide boundary) and then the
# registry actions (asserting the op fires the dispatch and writes nothing to
# the board -- the membership fold and grid mirror capture the result, tested
# with the live dock elsewhere).

test_that("add_dock_panel places a block panel with the caller's hint", {
  captured <- NULL

  local_mocked_bindings(
    show_block_panel = function(block, add_panel, dock, ...) {
      captured <<- list(block = block, add_panel = add_panel, dock = dock)
      invisible(NULL)
    },
    show_ext_panel = function(...) stop("ext path taken")
  )

  brd <- new_dock_board(blocks = c(a = new_dataset_block()))
  pos <- list(referenceGroup = "grp1", direction = "within")

  add_dock_panel("block_panel-a", brd, dock = "DOCK", position = pos)

  expect_identical(captured$add_panel, pos)
  expect_identical(captured$dock, "DOCK")
  expect_s3_class(captured$block, "blocks")
  expect_named(captured$block, "a")
})

test_that("add_dock_panel places an extension panel", {
  captured <- NULL

  local_mocked_bindings(
    show_block_panel = function(...) stop("block path taken"),
    show_ext_panel = function(ext, add_panel, dock, ...) {
      captured <<- list(ext = ext, add_panel = add_panel)
      invisible(NULL)
    }
  )

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    extensions = new_edit_board_extension()
  )

  add_dock_panel("ext_panel-edit_board_extension", brd, dock = "DOCK",
                 position = TRUE)

  expect_true(captured$add_panel)
  expect_true(is_dock_extension(captured$ext))
})

test_that("add_dock_panel defaults the position via determine_panel_pos", {
  captured <- NULL

  local_mocked_bindings(
    determine_panel_pos = function(dock) list(direction = "right"),
    show_block_panel = function(block, add_panel, dock, ...) {
      captured <<- add_panel
      invisible(NULL)
    },
    show_ext_panel = function(...) stop("ext path taken")
  )

  brd <- new_dock_board(blocks = c(a = new_dataset_block()))

  add_dock_panel("block_panel-a", brd, dock = "DOCK")

  expect_identical(captured, list(direction = "right"))
})

test_that("remove_dock_panel hides a block or extension panel", {
  seen <- NULL

  local_mocked_bindings(
    hide_block_panel = function(id, rm_panel, dock, ...) {
      seen <<- list(kind = "block", id = id, rm_panel = rm_panel, dock = dock)
      invisible(NULL)
    },
    hide_ext_panel = function(id, rm_panel, dock, ...) {
      seen <<- list(kind = "ext", id = id, rm_panel = rm_panel)
      invisible(NULL)
    }
  )

  remove_dock_panel("block_panel-a", dock = "DOCK")

  expect_identical(seen$kind, "block")
  expect_true(seen$rm_panel)
  expect_true(is_block_panel_id(seen$id))
  expect_identical(seen$dock, "DOCK")

  remove_dock_panel("ext_panel-foo", dock = "DOCK")

  expect_identical(seen$kind, "ext")
  expect_true(is_ext_panel_id(seen$id))
})

test_that("select_dock_panel activates a panel without adding or removing", {
  seen <- NULL

  local_mocked_bindings(
    show_block_panel = function(block, add_panel, dock, ...) {
      seen <<- list(kind = "block", add_panel = add_panel)
      invisible(NULL)
    },
    show_ext_panel = function(ext, add_panel, dock, ...) {
      seen <<- list(kind = "ext", add_panel = add_panel)
      invisible(NULL)
    }
  )

  select_dock_panel("block_panel-a", dock = "DOCK")

  expect_identical(seen$kind, "block")
  expect_false(seen$add_panel)

  select_dock_panel("ext_panel-foo", dock = "DOCK")

  expect_identical(seen$kind, "ext")
  expect_false(seen$add_panel)
})

test_that("the dispatch rejects an unrecognised panel id", {
  brd <- new_dock_board(blocks = c(a = new_dataset_block()))

  expect_error(
    add_dock_panel("gibberish-1", brd, dock = "DOCK", position = TRUE),
    class = "invalid_dock_panel_id_coercion"
  )
  expect_error(
    remove_dock_panel("gibberish-1", dock = "DOCK"),
    class = "invalid_dock_panel_id_coercion"
  )
  expect_error(
    select_dock_panel("gibberish-1", dock = "DOCK"),
    class = "invalid_dock_panel_id_coercion"
  )
})

test_that("resolve_op_dock resolves a named view or falls back to active", {
  active <- list(tag = "active")
  v1 <- list(tag = "v1")
  docks <- list(view_1 = v1, view_2 = list(tag = "v2"))

  expect_identical(resolve_op_dock(active, docks, "view_1"), v1)
  expect_identical(resolve_op_dock(active, docks, NULL), active)
  expect_identical(resolve_op_dock(active, docks, "gone"), active)
  expect_identical(resolve_op_dock(active, NULL, "view_1"), active)
})

test_that("the panel ops are registered on the board", {
  ids <- chr_ply(board_actions(new_dock_board()), action_id)

  expect_true(
    all(c("add_panel_action", "remove_panel_action", "select_panel_action")
        %in% ids)
  )
})

test_that("add_panel_action places the panel and writes nothing to the board", {
  captured <- NULL

  local_mocked_bindings(
    show_block_panel = function(block, add_panel, dock, ...) {
      captured <<- list(add_panel = add_panel, dock = dock)
      invisible(NULL)
    }
  )

  r_board <- reactiveValues(
    board = new_dock_board(blocks = c(a = new_dataset_block())),
    board_id = "b"
  )
  r_update <- reactiveVal(list())
  op <- list(
    panel = "block_panel-a",
    position = list(referenceGroup = "g", direction = "within")
  )

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        add_panel_action(
          trigger = reactive(op), board = r_board, update = r_update,
          dock = "DOCK", docks = NULL
        )
      )
    },
    {
      session$flushReact()

      expect_identical(captured$add_panel, op$position)
      expect_identical(captured$dock, "DOCK")
      expect_length(r_update(), 0L)
    }
  )
})

test_that("remove_panel_action hides the panel and writes nothing", {
  captured <- NULL

  local_mocked_bindings(
    hide_block_panel = function(id, rm_panel, dock, ...) {
      captured <<- list(rm_panel = rm_panel, dock = dock)
      invisible(NULL)
    }
  )

  r_board <- reactiveValues(
    board = new_dock_board(blocks = c(a = new_dataset_block())),
    board_id = "b"
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        remove_panel_action(
          trigger = reactive(list(panel = "block_panel-a")),
          board = r_board, update = r_update, dock = "DOCK", docks = NULL
        )
      )
    },
    {
      session$flushReact()

      expect_true(captured$rm_panel)
      expect_identical(captured$dock, "DOCK")
      expect_length(r_update(), 0L)
    }
  )
})

test_that("select_panel_action activates the panel and writes nothing", {
  captured <- NULL

  local_mocked_bindings(
    show_block_panel = function(block, add_panel, dock, ...) {
      captured <<- list(add_panel = add_panel)
      invisible(NULL)
    }
  )

  r_board <- reactiveValues(
    board = new_dock_board(blocks = c(a = new_dataset_block())),
    board_id = "b"
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        select_panel_action(
          trigger = reactive(list(panel = "block_panel-a")),
          board = r_board, update = r_update, dock = "DOCK", docks = NULL
        )
      )
    },
    {
      session$flushReact()

      expect_false(captured$add_panel)
      expect_length(r_update(), 0L)
    }
  )
})

test_that("a panel op with a view id targets that view's dock", {
  captured <- NULL

  local_mocked_bindings(
    show_block_panel = function(block, add_panel, dock, ...) {
      captured <<- dock
      invisible(NULL)
    }
  )

  r_board <- reactiveValues(
    board = new_dock_board(blocks = c(a = new_dataset_block())),
    board_id = "b"
  )
  r_update <- reactiveVal(list())
  op <- list(panel = "block_panel-a", position = TRUE, view = "view_2")

  testServer(
    function(id, ...) {
      docks <- reactiveValues(view_1 = "DOCK1", view_2 = "DOCK2")
      moduleServer(
        id,
        add_panel_action(
          trigger = reactive(op), board = r_board, update = r_update,
          dock = "ACTIVE", docks = docks
        )
      )
    },
    {
      session$flushReact()

      expect_identical(captured, "DOCK2")
    }
  )
})
