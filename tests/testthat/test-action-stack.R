# The stack menu publishes its committed selection into the parent
# session's namespace as `menu-commit` (list(blocks, nonce)); the
# panel-level form fields are real Shiny inputs at `menu-stack_name`,
# `menu-stack_color`, and `menu-stack_id`. Drive them directly to
# simulate the user creating / editing a stack.
local_mocked_sidebar <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    show_sidebar         = function(...) invisible(list(...)),
    keep_or_hide_sidebar = function(...) invisible(list(...)),
    hide_sidebar         = function(...) invisible(list(...)),
    .package = "blockr.dock",
    .env = env
  )
}

set_menu <- function(session, blocks, name, color, id, nonce) {
  session$setInputs(
    `menu-stack_name` = name,
    `menu-stack_color` = color,
    `menu-stack_id` = id,
    `menu-commit` = list(blocks = blocks, nonce = nonce)
  )
}

test_that("add stack action: valid commit creates one stack", {
  local_mocked_sidebar()
  r_board <- reactiveValues(
    board = new_board(
      c(a = new_dataset_block("iris"), b = new_head_block())
    ),
    board_id = "my_board"
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        add_stack_action(
          trigger = reactive(TRUE),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()
      set_menu(
        session,
        blocks = c("a", "b"),
        name = "My stack",
        color = "#ff0000",
        id = "s1",
        nonce = 1L
      )

      upd <- r_update()
      expect_length(upd, 1L)
      expect_named(upd, "stacks")
      expect_named(upd$stacks, "add")
      expect_named(upd$stacks$add, "s1")
      expect_s3_class(upd$stacks$add, "stacks")
      expect_identical(stack_blocks(upd$stacks$add[["s1"]]), c("a", "b"))
      expect_identical(stack_name(upd$stacks$add[["s1"]]), "My stack")
      expect_identical(stack_color(upd$stacks$add[["s1"]]), "#ff0000")
    }
  )
})

test_that("add stack action: invalid inputs short-circuit before update", {
  local_mocked_sidebar()
  r_board <- reactiveValues(
    board = new_board(c(a = new_dataset_block("iris"))),
    board_id = "b"
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        add_stack_action(
          trigger = reactive(TRUE),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()

      # Empty id -> rejected.
      set_menu(
        session,
        blocks = "a", name = "N", color = "#ffffff", id = "",
        nonce = 1L
      )
      expect_length(r_update(), 0L)

      # Empty name -> rejected.
      set_menu(
        session,
        blocks = "a", name = "", color = "#ffffff", id = "s1",
        nonce = 2L
      )
      expect_length(r_update(), 0L)

      # Invalid colour -> rejected.
      set_menu(
        session,
        blocks = "a", name = "N", color = "not-a-hex", id = "s1",
        nonce = 3L
      )
      expect_length(r_update(), 0L)
    }
  )
})

test_that("edit stack action: valid commit modifies the existing stack", {
  local_mocked_sidebar()
  r_board <- reactiveValues(
    board = new_dock_board(
      c(a = new_dataset_block("iris"), b = new_head_block()),
      stacks = stacks(s1 = "a")
    ),
    board_id = "b"
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        edit_stack_action(
          trigger = reactive("s1"),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()
      set_menu(
        session,
        blocks = c("a", "b"),
        name = "Updated",
        color = "#00ff00",
        id = NULL,
        nonce = 1L
      )

      upd <- r_update()
      expect_length(upd, 1L)
      expect_named(upd, "stacks")
      expect_named(upd$stacks, "mod")
      expect_named(upd$stacks$mod, "s1")
      # `mod` entries are partial-arg deltas (named list of constructor
      # argument values), not full `stacks` objects.
      expect_type(upd$stacks$mod, "list")
      expect_false(inherits(upd$stacks$mod, "stacks"))
      expect_identical(upd$stacks$mod[["s1"]]$blocks, c("a", "b"))
      expect_identical(upd$stacks$mod[["s1"]]$name, "Updated")
      expect_identical(upd$stacks$mod[["s1"]]$color, "#00ff00")
    }
  )
})

test_that("edit stack action: board change with no active edit is inert", {
  # Reproduces the create-stack crash: with no edit in progress
  # (`trigger()` is NULL), a board change from another action must not
  # error in the auto-close observer, and must not close the sidebar.
  hide_calls <- list()
  local_mocked_bindings(
    show_sidebar = function(...) invisible(NULL),
    keep_or_hide_sidebar = function(...) invisible(NULL),
    hide_sidebar = function(id, ...) {
      hide_calls[[length(hide_calls) + 1L]] <<- id
      invisible(NULL)
    },
    sidebar_state = function(id, ...) list(open = TRUE, pinned = TRUE),
    .package = "blockr.dock"
  )

  r_board <- reactiveValues(
    board = new_dock_board(c(a = new_dataset_block("iris"))),
    board_id = "b"
  )

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        edit_stack_action(
          trigger = reactive(NULL),
          board = r_board,
          update = reactiveVal(list())
        )
      )
    },
    {
      session$flushReact()
      # Another action mutates the board (e.g. creates a stack).
      r_board$board <- new_dock_board(
        c(a = new_dataset_block("iris")),
        stacks = stacks(s1 = "a")
      )
      expect_no_error(session$flushReact())
      expect_length(hide_calls, 0L)
    }
  )
})

test_that("edit stack action: removing the edited stack closes the sidebar", {
  # Open the edit menu for `s1`, then remove `s1` from the board: the
  # board observer must close the sidebar immediately (not wait for an
  # "Update" click), and never error in `lookup_stack()`.
  hide_calls <- list()
  local_mocked_bindings(
    show_sidebar = function(...) invisible(NULL),
    keep_or_hide_sidebar = function(...) invisible(NULL),
    hide_sidebar = function(id, ...) {
      hide_calls[[length(hide_calls) + 1L]] <<- id
      invisible(NULL)
    },
    # The auto-close is gated on the sidebar being open.
    sidebar_state = function(id, ...) list(open = TRUE, pinned = TRUE),
    .package = "blockr.dock"
  )

  r_board <- reactiveValues(
    board = new_dock_board(
      c(a = new_dataset_block("iris"), b = new_head_block()),
      stacks = stacks(s1 = "a")
    ),
    board_id = "b"
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        edit_stack_action(
          trigger = reactive("s1"),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()
      expect_length(hide_calls, 0L)

      # Remove the edited stack -> sidebar closes live.
      r_board$board <- new_dock_board(board_blocks(r_board$board))
      session$flushReact()

      expect_gte(length(hide_calls), 1L)
      expect_identical(hide_calls[[1L]], "b-actions_sidebar")
    }
  )
})

test_that("edit stack action: invalid colour / block id short-circuit", {
  local_mocked_sidebar()
  r_board <- reactiveValues(
    board = new_dock_board(
      c(a = new_dataset_block("iris"), b = new_head_block()),
      stacks = stacks(s1 = "a")
    ),
    board_id = "b"
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        edit_stack_action(
          trigger = reactive("s1"),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()

      # Empty name -> rejected.
      set_menu(
        session,
        blocks = c("a", "b"), name = "", color = "#abcdef", id = NULL,
        nonce = 1L
      )
      expect_length(r_update(), 0L)

      # Invalid colour -> rejected.
      set_menu(
        session,
        blocks = c("a", "b"), name = "N", color = "not-hex", id = NULL,
        nonce = 2L
      )
      expect_length(r_update(), 0L)
    }
  )
})

test_that("remove stack action", {
  r_board <- reactiveValues(
    board = new_dock_board(
      c(a = new_dataset_block("iris"), b = new_head_block()),
      stacks = stacks(a = "a")
    )
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        remove_stack_action(
          trigger = reactive("a"),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()
      upd <- r_update()
      expect_length(upd, 1L)
      expect_named(upd, "stacks")
      expect_named(upd$stacks, "rm")
      expect_identical(upd$stacks$rm, "a")
    }
  )
})
