# `block_browser_server("browser")` publishes its committed value into the
# parent session's namespace as `browser-commit`. The module now builds
# the block (and, for append / prepend, the link with its port resolved)
# and returns a ready-to-apply value: a `blocks` object for add, or
# `list(blocks, links)` for append / prepend. The handlers just apply it,
# so these tests drive `browser-commit` and assert the `update()` payload.
#
# Id semantics: an empty id field means "assign me one" - the module
# resolves a unique, board-avoiding id at commit. Only a *non-empty*
# duplicate id is rejected (no commit fires).
commit_spec <- function(...) list(...)

test_that("add block action: commit creates one ready block", {
  r_board <- reactiveValues(board = new_board(), board_id = "b")
  r_update <- reactiveVal(list())
  local_mocked_bindings(
    show_sidebar         = function(...) invisible(NULL),
    keep_or_hide_sidebar = function(...) invisible(NULL),
    hide_sidebar         = function(...) invisible(NULL)
  )

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        add_block_action(
          trigger = reactive(TRUE), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()

      session$setInputs(`browser-commit` = commit_spec(
        type = "dataset_block", id = "ds1", title = "My data", nonce = 1
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

test_that("add block action: an empty id is auto-assigned", {
  r_board <- reactiveValues(board = new_board(), board_id = "b")
  r_update <- reactiveVal(list())
  local_mocked_bindings(
    show_sidebar         = function(...) invisible(NULL),
    keep_or_hide_sidebar = function(...) invisible(NULL),
    hide_sidebar         = function(...) invisible(NULL)
  )

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        add_block_action(
          trigger = reactive(TRUE), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()

      session$setInputs(`browser-commit` = commit_spec(
        type = "dataset_block", id = "", title = NULL, nonce = 1
      ))

      upd <- r_update()
      expect_named(upd, "blocks")
      expect_s3_class(upd$blocks$add, "blocks")
      # Auto-generated, non-empty id.
      expect_true(nzchar(names(upd$blocks$add)))
    }
  )
})

test_that("add block action: a non-empty duplicate id is rejected", {
  r_board <- reactiveValues(
    board = new_board(blocks = c(ds1 = new_dataset_block())),
    board_id = "b"
  )
  r_update <- reactiveVal(list())
  local_mocked_bindings(
    show_sidebar         = function(...) invisible(NULL),
    keep_or_hide_sidebar = function(...) invisible(NULL),
    hide_sidebar         = function(...) invisible(NULL)
  )

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        add_block_action(
          trigger = reactive(TRUE), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()

      session$setInputs(`browser-commit` = commit_spec(
        type = "dataset_block", id = "ds1", title = NULL, nonce = 1
      ))
      expect_length(r_update(), 0L)
    }
  )
})

test_that("append block action: NULL block_input falls back to the only slot", {
  r_board <- reactiveValues(
    board = new_board(blocks = c(a = new_dataset_block())),
    board_id = "b"
  )
  r_update <- reactiveVal(list())
  local_mocked_bindings(
    show_sidebar         = function(...) invisible(NULL),
    keep_or_hide_sidebar = function(...) invisible(NULL),
    hide_sidebar         = function(...) invisible(NULL)
  )

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        append_block_action(
          trigger = reactive("a"), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()
      session$setInputs(`browser-commit` = commit_spec(
        type = "head_block", id = "h1", title = NULL,
        link_id = "lnk1", block_input = NULL, nonce = 1
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
    show_sidebar         = function(...) invisible(NULL),
    keep_or_hide_sidebar = function(...) invisible(NULL),
    hide_sidebar         = function(...) invisible(NULL)
  )

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        append_block_action(
          trigger = reactive("a"), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()

      session$setInputs(`browser-commit` = commit_spec(
        type = "head_block", id = "h1", title = "First rows",
        link_id = "lnk1", block_input = "data", nonce = 1
      ))

      upd <- r_update()
      expect_named(upd, c("blocks", "links"))
      expect_named(upd$blocks$add, "h1")
      expect_named(upd$links$add, "lnk1")
      expect_s3_class(upd$blocks$add, "blocks")
      expect_s3_class(upd$links$add, "links")
      # Link wires source -> new block.
      df <- as.data.frame(upd$links$add)
      expect_identical(df$from, "a")
      expect_identical(df$to, "h1")
    }
  )
})

test_that("append block action: an empty link_id is auto-assigned", {
  r_board <- reactiveValues(
    board = new_board(blocks = c(a = new_dataset_block())),
    board_id = "b"
  )
  r_update <- reactiveVal(list())
  local_mocked_bindings(
    show_sidebar         = function(...) invisible(NULL),
    keep_or_hide_sidebar = function(...) invisible(NULL),
    hide_sidebar         = function(...) invisible(NULL)
  )

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        append_block_action(
          trigger = reactive("a"), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()
      session$setInputs(`browser-commit` = commit_spec(
        type = "head_block", id = "h1", title = NULL,
        link_id = "", block_input = "data", nonce = 1
      ))

      upd <- r_update()
      expect_named(upd, c("blocks", "links"))
      expect_true(nzchar(names(upd$links$add)))
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
    show_sidebar         = function(...) invisible(NULL),
    keep_or_hide_sidebar = function(...) invisible(NULL),
    hide_sidebar         = function(...) invisible(NULL)
  )

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        prepend_block_action(
          trigger = reactive("m"), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()

      session$setInputs(`browser-commit` = commit_spec(
        type = "dataset_block", id = "ds1", title = NULL,
        link_id = "lnk1", block_input = NULL, target_input = "y", nonce = 1
      ))

      upd <- r_update()
      expect_named(upd, c("blocks", "links"))
      expect_named(upd$blocks$add, "ds1")
      expect_named(upd$links$add, "lnk1")
      df <- as.data.frame(upd$links$add)
      # Prepend wires new block -> target on the chosen slot.
      expect_identical(df$from, "ds1")
      expect_identical(df$to, "m")
      expect_identical(df$input, "y")
    }
  )
})

test_that("prepend: NULL target_input falls back to only slot", {
  # head_block has arity 1 (input "data"); the browser hides the
  # target_input picker, so spec$target_input arrives as NULL. The
  # module resolves the target's only free slot.
  r_board <- reactiveValues(
    board = new_board(blocks = c(h = new_head_block())),
    board_id = "b"
  )
  r_update <- reactiveVal(list())
  local_mocked_bindings(
    show_sidebar         = function(...) invisible(NULL),
    keep_or_hide_sidebar = function(...) invisible(NULL),
    hide_sidebar         = function(...) invisible(NULL)
  )

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        prepend_block_action(
          trigger = reactive("h"), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()
      session$setInputs(`browser-commit` = commit_spec(
        type = "dataset_block", id = "ds1", title = NULL,
        link_id = "lnk1", block_input = NULL, target_input = NULL, nonce = 1
      ))

      upd <- r_update()
      expect_length(upd$links$add, 1L)
      expect_identical(as.data.frame(upd$links$add)$input, "data")
    }
  )
})

test_that("add block action toggles its pre-rendered sidebar on confirm", {
  keep_calls <- list()

  local_mocked_bindings(
    show_sidebar = function(...) invisible(NULL),
    keep_or_hide_sidebar = function(id, ...) {
      keep_calls[[length(keep_calls) + 1L]] <<-
        list(id = id, args = list(...))
      invisible(NULL)
    },
    hide_sidebar = function(...) invisible(NULL)
  )

  r_board <- reactiveValues(board = new_board(), board_id = "my_board")
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        add_block_action(
          trigger = reactive(TRUE), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()
      session$setInputs(`browser-commit` = commit_spec(
        type = "dataset_block", id = "x", title = "X block", nonce = 1
      ))

      expect_length(r_update(), 1L)
      expect_length(keep_calls, 1L)
      # Add now targets its dedicated, pre-rendered sidebar and re-opens
      # without `ui` (no re-render).
      expect_identical(keep_calls[[1L]]$id, "my_board-add_block_sidebar")
      expect_identical(keep_calls[[1L]]$args$title, "Add new block")
      expect_null(keep_calls[[1L]]$args$ui)
    }
  )
})

test_that("append block action toggles its pre-rendered sidebar on confirm", {
  keep_calls <- list()

  local_mocked_bindings(
    show_sidebar = function(...) invisible(NULL),
    keep_or_hide_sidebar = function(id, ...) {
      keep_calls[[length(keep_calls) + 1L]] <<-
        list(id = id, args = list(...))
      invisible(NULL)
    },
    hide_sidebar = function(...) invisible(NULL)
  )

  r_board <- reactiveValues(
    board = new_board(blocks = c(a = new_dataset_block())),
    board_id = "my_board"
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        append_block_action(
          trigger = reactive("a"), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()
      session$setInputs(`browser-commit` = commit_spec(
        type = "head_block", id = "h1", title = NULL,
        link_id = "lnk1", block_input = "data", nonce = 1
      ))

      expect_named(r_update(), c("blocks", "links"))
      expect_length(keep_calls, 1L)
      # Append also targets its dedicated, pre-rendered sidebar and
      # re-opens without `ui` (no re-render); the source goes in the title.
      expect_identical(keep_calls[[1L]]$id, "my_board-append_block_sidebar")
      expect_identical(keep_calls[[1L]]$args$title, "Append from a")
      expect_null(keep_calls[[1L]]$args$ui)
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
          trigger = reactive("a"), board = r_board, update = r_update
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
