test_that("add link action", {

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        add_link_action(
          trigger = reactive("b"),
          board = reactiveValues(
            board = new_board(
              c(
                a = new_dataset_block("iris"),
                b = new_head_block()
              )
            )
          ),
          update = reactiveVal(list())
        )
      )
    },
    {
      session$flushReact()
      expect_length(update(), 0L)
    }
  )

  r_board <- reactiveValues(
    board = new_board(
      c(
        a = new_dataset_block("iris"),
        b = new_head_block()
      )
    )
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        add_link_action(
          trigger = reactive("a"),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()
      expect_length(r_update(), 0L)

      session$setInputs(create_link = "a")
      expect_length(r_update(), 0L)

      session$setInputs(create_link = "b")
      expect_length(r_update(), 0L)

      session$setInputs(
        add_link_confirm = 1L,
        add_link_id = ""
      )

      expect_length(r_update(), 0L)

      session$setInputs(
        add_link_confirm = 2L,
        add_link_id = "test",
        add_link_input = "test"
      )

      expect_length(r_update(), 0L)

      session$setInputs(
        add_link_confirm = 3L,
        add_link_id = "test",
        add_link_input = "data"
      )

      upd <- r_update()

      expect_length(upd, 1L)
      expect_named(upd, "links")

      expect_length(upd$links, 1L)
      expect_named(upd$links, "add")

      expect_length(upd$links$add, 1L)
      expect_named(upd$links$add, "test")
      expect_s3_class(upd$links$add, "links")
    }
  )
})

test_that("add link action warns and skips sidebar when no inputs available", {
  # Board with a single block: any target with free inputs would have to
  # be a *different* block, so `link_sidebar_body()` finds none and
  # returns NULL. The action handler should `notify()` and NOT open the
  # sidebar.

  notify_calls <- list()
  show_calls <- list()

  local_mocked_bindings(
    notify = function(message, ...) {
      notify_calls[[length(notify_calls) + 1L]] <<- message
      invisible(NULL)
    },
    .package = "blockr.dock"
  )
  local_mocked_bindings(
    show_sidebar = function(id, ...) {
      show_calls[[length(show_calls) + 1L]] <<- list(id = id, args = list(...))
      invisible(NULL)
    },
    .package = "blockr.ui"
  )

  r_board <- reactiveValues(
    board = new_board(c(a = new_dataset_block("iris"))),
    board_id = "my_board"
  )

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        add_link_action(
          trigger = reactive("a"),
          board = r_board,
          update = reactiveVal(list())
        )
      )
    },
    {
      session$flushReact()
      expect_length(show_calls, 0L)
      expect_length(notify_calls, 1L)
      expect_match(notify_calls[[1L]], "No inputs are currently available")
    }
  )
})

test_that("add link action chains via keep_or_hide when inputs remain", {
  # Two unconnected blocks + a third with a free input: confirming a link
  # from `a` to `b` leaves `c` available, so the post-confirm rebuild
  # should call `keep_or_hide_sidebar()` (chain branch) and NOT
  # `hide_sidebar()`.

  keep_calls <- list()
  hide_calls <- list()

  local_mocked_bindings(
    keep_or_hide_sidebar = function(id, ...) {
      keep_calls[[length(keep_calls) + 1L]] <<- list(id = id, args = list(...))
      invisible(NULL)
    },
    hide_sidebar = function(id, ...) {
      hide_calls[[length(hide_calls) + 1L]] <<- list(id = id)
      invisible(NULL)
    },
    show_sidebar = function(...) invisible(NULL),
    .package = "blockr.ui"
  )

  r_board <- reactiveValues(
    board = new_board(
      c(
        a = new_dataset_block("iris"),
        b = new_head_block(),
        c = new_head_block()
      )
    ),
    board_id = "my_board"
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        add_link_action(
          trigger = reactive("a"),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()

      session$setInputs(create_link = "b")
      session$setInputs(
        add_link_confirm = 1L,
        add_link_id = "ab",
        add_link_input = "data"
      )
      # Drive the reactive flush so the deferred `session$onFlushed()`
      # callback installed by the confirm handler fires.
      session$flushReact()

      expect_length(r_update(), 1L)
      expect_length(keep_calls, 1L)
      expect_identical(keep_calls[[1L]]$id, "my_board-actions_sidebar")
      expect_length(hide_calls, 0L)
    }
  )
})

test_that("add link action hides sidebar when no inputs remain post-confirm", {
  # Only one target block with a single available input: after the link
  # consumes it, the post-flush check finds nothing left, so the action
  # should `hide_sidebar()` rather than `keep_or_hide_sidebar()`.

  keep_calls <- list()
  hide_calls <- list()

  local_mocked_bindings(
    keep_or_hide_sidebar = function(id, ...) {
      keep_calls[[length(keep_calls) + 1L]] <<- list(id = id)
      invisible(NULL)
    },
    hide_sidebar = function(id, ...) {
      hide_calls[[length(hide_calls) + 1L]] <<- list(id = id)
      invisible(NULL)
    },
    show_sidebar = function(...) invisible(NULL),
    .package = "blockr.ui"
  )

  r_board <- reactiveValues(
    board = new_board(
      c(a = new_dataset_block("iris"), b = new_head_block())
    ),
    board_id = "my_board"
  )
  r_update <- reactiveVal(list())

  # Wire `update()` to reflect into r_board so the post-flush
  # `link_sidebar_body()` actually sees the just-added link.
  testServer(
    function(id, ...) {
      observeEvent(
        r_update(),
        {
          upd <- r_update()
          if (length(upd$links$add)) {
            board_links(r_board$board) <- c(
              board_links(r_board$board),
              upd$links$add
            )
          }
        }
      )
      moduleServer(
        id,
        add_link_action(
          trigger = reactive("a"),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()

      session$setInputs(create_link = "b")
      session$setInputs(
        add_link_confirm = 1L,
        add_link_id = "ab",
        add_link_input = "data"
      )
      session$flushReact()

      expect_length(hide_calls, 1L)
      expect_identical(hide_calls[[1L]]$id, "my_board-actions_sidebar")
      expect_length(keep_calls, 0L)
    }
  )
})

test_that("locked dock refuses add/remove link action mutations (#127)", {
  withr::local_options(blockr.dock_is_locked = TRUE)

  r_board <- reactiveValues(
    board = new_board(
      c(a = new_dataset_block("iris"), b = new_head_block()),
      links = links(id = "ab", from = "a", to = "b")
    )
  )
  r_update <- reactiveVal(list())

  # add_link_action: confirm-with-valid-inputs must not mutate.
  testServer(
    function(id, ...) {
      moduleServer(
        id,
        add_link_action(
          trigger = reactive("a"),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()
      session$setInputs(create_link = "b")
      session$setInputs(
        add_link_confirm = 1L,
        add_link_id = "new_link",
        add_link_input = "data"
      )
      expect_length(r_update(), 0L)
    }
  )

  # remove_link_action: trigger fires on flush in unlocked, but the gated
  # eventExpr blocks it here.
  testServer(
    function(id, ...) {
      moduleServer(
        id,
        remove_link_action(
          trigger = reactive("ab"),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()
      expect_length(r_update(), 0L)
    }
  )
})

test_that("remove link action", {

  r_board <- reactiveValues(
    board = new_board(
      c(
        a = new_dataset_block("iris"),
        b = new_head_block()
      ),
      links = links(id = "ab", from = "a", to = "b")
    )
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        remove_link_action(
          trigger = reactive("ab"),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()

      upd <- r_update()

      expect_length(upd, 1L)
      expect_named(upd, "links")

      expect_length(upd$links, 1L)
      expect_named(upd$links, "rm")

      expect_length(upd$links$rm, 1L)
      expect_identical(upd$links$rm, "ab")
    }
  )
})
