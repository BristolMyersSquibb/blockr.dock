# The browser publishes its committed-block spec into the parent
# session's namespace as `<sub>-commit`, where <sub> is the module id
# the handler mounts (`block_browser_server("browser")`). Drive that
# input directly to simulate the user adding a block.
commit_spec <- function(...) list(...)

test_that("add block action: valid commit creates one block", {
  r_board <- reactiveValues(board = new_board(), board_id = "b")
  r_update <- reactiveVal(list())

  local_mocked_bindings(
    show_sidebar       = function(...) invisible(list(...)),
    keep_or_hide_sidebar = function(...) invisible(list(...)),
    hide_sidebar       = function(...) invisible(list(...)),
    .package = "blockr.ui"
  )

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        add_block_action(
          trigger = reactive(TRUE),
          board   = r_board,
          update  = r_update
        )
      )
    },
    {
      session$flushReact()

      # Invalid: empty id -> no update.
      session$setInputs(`browser-commit` = commit_spec(
        type = "dataset_block", id = "", title = NULL,
        link_id = NULL, block_input = NULL, target_input = NULL,
        nonce = 1
      ))
      expect_length(r_update(), 0L)

      # Valid commit -> one block under spec$id.
      session$setInputs(`browser-commit` = commit_spec(
        type = "dataset_block", id = "ds1", title = "My data",
        link_id = NULL, block_input = NULL, target_input = NULL,
        nonce = 2
      ))

      upd <- r_update()
      expect_length(upd, 1L)
      expect_named(upd, "blocks")
      expect_named(upd$blocks, "add")
      expect_named(upd$blocks$add, "ds1")
      expect_s3_class(upd$blocks$add, "blocks")
    }
  )
})

test_that("append block action: NULL block_input falls back to the only slot", {
  # head_block has a single input ("data"); the browser hides the
  # input-port picker so spec$block_input arrives as NULL. The handler
  # must fall back to that sole slot.
  r_board <- reactiveValues(
    board = new_board(blocks = c(a = new_dataset_block())),
    board_id = "b"
  )
  r_update <- reactiveVal(list())

  local_mocked_bindings(
    show_sidebar       = function(...) invisible(list(...)),
    keep_or_hide_sidebar = function(...) invisible(list(...)),
    hide_sidebar       = function(...) invisible(list(...)),
    .package = "blockr.ui"
  )

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        append_block_action(
          trigger = reactive("a"),
          board   = r_board,
          update  = r_update
        )
      )
    },
    {
      session$flushReact()
      session$setInputs(`browser-commit` = list(
        type = "head_block", id = "h1", title = NULL,
        link_id = "lnk1", block_input = NULL, target_input = NULL,
        nonce = 1
      ))

      upd <- r_update()
      expect_length(upd$links$add, 1L)
      # The single link's `input` is the head block's only slot.
      expect_identical(as.data.frame(upd$links$add)$input, "data")
    }
  )
})

test_that("append block action: valid commit creates one block + one link", {
  r_board <- reactiveValues(
    board = new_board(blocks = c(a = new_dataset_block())),
    board_id = "b"
  )
  r_update <- reactiveVal(list())

  local_mocked_bindings(
    show_sidebar       = function(...) invisible(list(...)),
    keep_or_hide_sidebar = function(...) invisible(list(...)),
    hide_sidebar       = function(...) invisible(list(...)),
    .package = "blockr.ui"
  )

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        append_block_action(
          trigger = reactive("a"),
          board   = r_board,
          update  = r_update
        )
      )
    },
    {
      session$flushReact()

      # Missing link_id -> no update.
      session$setInputs(`browser-commit` = commit_spec(
        type = "head_block", id = "h1", title = NULL,
        link_id = "", block_input = "data", target_input = NULL,
        nonce = 1
      ))
      expect_length(r_update(), 0L)

      # Valid commit -> blocks add + links add.
      session$setInputs(`browser-commit` = commit_spec(
        type = "head_block", id = "h1", title = "First rows",
        link_id = "lnk1", block_input = "data", target_input = NULL,
        nonce = 2
      ))

      upd <- r_update()
      expect_named(upd, c("blocks", "links"))
      expect_named(upd$blocks$add, "h1")
      expect_named(upd$links$add, "lnk1")
      expect_s3_class(upd$blocks$add, "blocks")
      expect_s3_class(upd$links$add, "links")
    }
  )
})

test_that("prepend block action: target_input picks the link slot", {
  r_board <- reactiveValues(
    board = new_board(blocks = c(m = new_merge_block())),  # arity 2
    board_id = "b"
  )
  r_update <- reactiveVal(list())

  local_mocked_bindings(
    show_sidebar       = function(...) invisible(list(...)),
    keep_or_hide_sidebar = function(...) invisible(list(...)),
    hide_sidebar       = function(...) invisible(list(...)),
    .package = "blockr.ui"
  )

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        prepend_block_action(
          trigger = reactive("m"),
          board   = r_board,
          update  = r_update
        )
      )
    },
    {
      session$flushReact()

      session$setInputs(`browser-commit` = commit_spec(
        type = "dataset_block", id = "ds1", title = NULL,
        link_id = "lnk1", block_input = NULL, target_input = "y",
        nonce = 1
      ))

      upd <- r_update()
      expect_named(upd, c("blocks", "links"))
      expect_named(upd$blocks$add, "ds1")
      expect_named(upd$links$add, "lnk1")
    }
  )
})

test_that("add block action chains via keep_or_hide_sidebar on confirm", {
  keep_calls <- list()

  local_mocked_bindings(
    show_sidebar = function(...) invisible(NULL),
    keep_or_hide_sidebar = function(id, ...) {
      keep_calls[[length(keep_calls) + 1L]] <<-
        list(id = id, args = list(...))
      invisible(NULL)
    },
    hide_sidebar = function(...) invisible(NULL),
    .package = "blockr.ui"
  )

  r_board <- reactiveValues(board = new_board(), board_id = "my_board")
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        add_block_action(
          trigger = reactive(TRUE),
          board   = r_board,
          update  = r_update
        )
      )
    },
    {
      session$flushReact()
      session$setInputs(`browser-commit` = commit_spec(
        type = "dataset_block", id = "x", title = "X block",
        link_id = NULL, block_input = NULL, target_input = NULL,
        nonce = 1
      ))

      expect_length(r_update(), 1L)
      expect_length(keep_calls, 1L)
      expect_identical(keep_calls[[1L]]$id, "my_board-actions_sidebar")
      expect_identical(keep_calls[[1L]]$args$title, "Add new block")
    }
  )
})

test_that("append block action: invalid block_id does not update", {
  r_board <- reactiveValues(
    board = new_board(blocks = c(a = new_dataset_block())),
    board_id = "b"
  )
  r_update <- reactiveVal(list())

  local_mocked_bindings(
    show_sidebar       = function(...) invisible(list(...)),
    keep_or_hide_sidebar = function(...) invisible(list(...)),
    hide_sidebar       = function(...) invisible(list(...)),
    .package = "blockr.ui"
  )

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        append_block_action(
          trigger = reactive("a"),
          board   = r_board,
          update  = r_update
        )
      )
    },
    {
      session$flushReact()
      session$setInputs(`browser-commit` = list(
        type = "head_block", id = "", title = NULL,
        link_id = "lnk1", block_input = "data", target_input = NULL,
        nonce = 1
      ))
      expect_length(r_update(), 0L)
    }
  )
})

test_that("prepend block action: invalid block_id / link_id do not update", {
  r_board <- reactiveValues(
    board = new_board(blocks = c(m = new_merge_block())),
    board_id = "b"
  )
  r_update <- reactiveVal(list())

  local_mocked_bindings(
    show_sidebar       = function(...) invisible(list(...)),
    keep_or_hide_sidebar = function(...) invisible(list(...)),
    hide_sidebar       = function(...) invisible(list(...)),
    .package = "blockr.ui"
  )

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        prepend_block_action(
          trigger = reactive("m"),
          board   = r_board,
          update  = r_update
        )
      )
    },
    {
      session$flushReact()

      # Empty block_id -> bail.
      session$setInputs(`browser-commit` = list(
        type = "dataset_block", id = "", title = NULL,
        link_id = "lnk1", block_input = NULL, target_input = "x",
        nonce = 1
      ))
      expect_length(r_update(), 0L)

      # Empty link_id -> bail.
      session$setInputs(`browser-commit` = list(
        type = "dataset_block", id = "ds1", title = NULL,
        link_id = "", block_input = NULL, target_input = "x",
        nonce = 2
      ))
      expect_length(r_update(), 0L)
    }
  )
})

test_that("prepend: NULL target_input falls back to only slot", {
  # head_block has arity 1 (input "data"); the browser hides the
  # target_input picker, so spec$target_input arrives as NULL. The
  # handler must fall back to `block_input_select(..., mode = "inputs")[1L]`.
  r_board <- reactiveValues(
    board = new_board(blocks = c(h = new_head_block())),
    board_id = "b"
  )
  r_update <- reactiveVal(list())

  local_mocked_bindings(
    show_sidebar       = function(...) invisible(list(...)),
    keep_or_hide_sidebar = function(...) invisible(list(...)),
    hide_sidebar       = function(...) invisible(list(...)),
    .package = "blockr.ui"
  )

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        prepend_block_action(
          trigger = reactive("h"),
          board   = r_board,
          update  = r_update
        )
      )
    },
    {
      session$flushReact()
      session$setInputs(`browser-commit` = list(
        type = "dataset_block", id = "ds1", title = NULL,
        link_id = "lnk1", block_input = NULL, target_input = NULL,
        nonce = 1
      ))

      upd <- r_update()
      expect_length(upd$links$add, 1L)
      expect_identical(as.data.frame(upd$links$add)$input, "data")
    }
  )
})

test_that("append / prepend: unknown type bails after NULL block", {
  # Both handlers must short-circuit on a build failure (the
  # `if (is.null(new_blk)) return()` guard after build_block_from_spec).
  r_board <- reactiveValues(
    board = new_board(blocks = c(a = new_dataset_block(),
                                 m = new_merge_block())),
    board_id = "b"
  )
  r_update_app <- reactiveVal(list())
  r_update_pre <- reactiveVal(list())

  local_mocked_bindings(
    show_sidebar       = function(...) invisible(list(...)),
    keep_or_hide_sidebar = function(...) invisible(list(...)),
    hide_sidebar       = function(...) invisible(list(...)),
    .package = "blockr.ui"
  )

  bogus <- function(update_rv) {
    list(
      type = "no_such_block_xyz", id = "x", title = NULL,
      link_id = "l", block_input = "x", target_input = "x",
      nonce = 1
    )
  }

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        append_block_action(
          trigger = reactive("a"),
          board   = r_board,
          update  = r_update_app
        )
      )
    },
    {
      session$flushReact()
      session$setInputs(`browser-commit` = bogus(r_update_app))
      expect_length(r_update_app(), 0L)
    }
  )

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        prepend_block_action(
          trigger = reactive("m"),
          board   = r_board,
          update  = r_update_pre
        )
      )
    },
    {
      session$flushReact()
      session$setInputs(`browser-commit` = bogus(r_update_pre))
      expect_length(r_update_pre(), 0L)
    }
  )
})

test_that("build_block_from_spec: unknown type bails with notify", {
  # An unknown registry uid in spec$type makes create_block_with_name
  # throw; `build_block_from_spec` catches it, notifies, returns NULL,
  # and the handlers bail without calling update().
  r_board <- reactiveValues(board = new_board(), board_id = "b")
  r_update <- reactiveVal(list())

  local_mocked_bindings(
    show_sidebar       = function(...) invisible(list(...)),
    keep_or_hide_sidebar = function(...) invisible(list(...)),
    hide_sidebar       = function(...) invisible(list(...)),
    .package = "blockr.ui"
  )

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        add_block_action(
          trigger = reactive(TRUE),
          board   = r_board,
          update  = r_update
        )
      )
    },
    {
      session$flushReact()
      session$setInputs(`browser-commit` = list(
        type = "no_such_block_xyz", id = "x", title = NULL,
        link_id = NULL, block_input = NULL, target_input = NULL,
        nonce = 1
      ))
      expect_length(r_update(), 0L)
    }
  )
})

test_that("remove block action", {
  r_board <- reactiveValues(
    board = new_board(blocks = c(a = new_dataset_block()))
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        remove_block_action(
          trigger = reactive("a"),
          board   = r_board,
          update  = r_update
        )
      )
    },
    {
      expect_length(r_update(), 0L)
      session$flushReact()

      upd <- r_update()
      expect_named(upd, "blocks")
      expect_named(upd$blocks, "rm")
      expect_identical(upd$blocks$rm, "a")
    }
  )
})
