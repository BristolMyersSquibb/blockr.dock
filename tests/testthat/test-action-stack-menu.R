# The stack actions open menus (action-menu.R) instead of sidebar forms
# (#544). See test-action-menu.R for the helpers' pattern.

local_stack_menus <- function(env = parent.frame()) {
  menus <- new.env()
  menus$sent <- list()
  local_mocked_bindings(
    open_action_menu = function(...) {
      msg <- list(...)
      menus$sent <- c(menus$sent, list(msg))
      invisible(msg)
    },
    .env = env
  )
  menus
}

stack_board <- function(stks = stacks()) {
  reactiveValues(
    board = new_dock_board(
      c(
        a = new_dataset_block("iris"),
        b = new_dataset_block("mtcars"),
        h = new_head_block()
      ),
      stacks = stks
    ),
    board_id = "brd"
  )
}

stack_app <- function(action, trigger, board, update) {
  function(id, ...) {
    moduleServer(id, action(trigger = trigger, board = board, update = update))
  }
}

menu_of <- function(menus) menus$sent[[length(menus$sent)]]

values_of <- function(msg) {
  vals <- lapply(msg$items, `[[`, "value")
  unlist(vals[!vapply(vals, is.null, logical(1L))])
}

choose <- function(session, value = NULL, values = NULL, field = NULL,
                   nonce = 1L) {
  session$setInputs(
    pick = Filter(
      Negate(is.null),
      list(value = value, values = values, field = field, nonce = nonce)
    )
  )
}

test_that("stack menu: rows and head", {
  menus <- local_stack_menus()
  upd <- reactiveVal(list())
  brd <- stack_board(stacks(s1 = new_dock_stack(c("a", "h"), name = "Input")))
  testServer(stack_app(edit_stack_action, reactive("s1"), brd, upd), {
    session$flushReact()
    msg <- menu_of(menus)
    expect_identical(msg$head, list(title = "Input", text = "2 blocks"))
    expect_identical(values_of(msg), c("rename", "colour", "blocks", "dissolve"))
  })
})

test_that("stack menu: Rename opens a field, then renames", {
  menus <- local_stack_menus()
  upd <- reactiveVal(list())
  brd <- stack_board(stacks(s1 = new_dock_stack("a", name = "Input")))
  testServer(stack_app(edit_stack_action, reactive("s1"), brd, upd), {
    session$flushReact()
    choose(session, "rename")
    msg <- menu_of(menus)
    expect_identical(msg$field$value, "Input")
    expect_false(msg$field$empty_ok)
    expect_identical(msg$back$head$title, "Input")
    choose(session, field = "Input data", nonce = 2L)
    expect_identical(upd()$stacks$mod$s1, list(name = "Input data"))
  })
})

test_that("stack menu: Colour lists the current, suggestions and Custom", {
  menus <- local_stack_menus()
  upd <- reactiveVal(list())
  brd <- stack_board(stacks(s1 = new_dock_stack("a", name = "Input", color = "#DF8396")))
  testServer(stack_app(edit_stack_action, reactive("s1"), brd, upd), {
    session$flushReact()
    choose(session, "colour")
    msg <- menu_of(menus)
    vals <- values_of(msg)
    expect_identical(vals[1L], "colour:#DF8396")
    expect_identical(vals[length(vals)], "colour")
    custom <- msg$items[[length(msg$items)]]
    expect_true(custom$colour_picker)
    expect_identical(custom$colour, "#DF8396")
    choose(session, "colour:#2EC1D9", nonce = 2L)
    expect_identical(upd()$stacks$mod$s1, list(color = "#2EC1D9"))
  })
})

test_that("stack menu: Blocks ticks the members and sets them on close", {
  menus <- local_stack_menus()
  upd <- reactiveVal(list())
  brd <- stack_board(stacks(s1 = new_dock_stack("a", name = "Input")))
  testServer(stack_app(edit_stack_action, reactive("s1"), brd, upd), {
    session$flushReact()
    choose(session, "blocks")
    msg <- menu_of(menus)
    expect_true(msg$multi)
    ticked <- vapply(msg$items, function(i) isTRUE(i$checked), logical(1L))
    expect_identical(values_of(msg)[ticked], "a")
    choose(session, values = list("a", "h"), nonce = 2L)
    expect_identical(upd()$stacks$mod$s1, list(blocks = c("a", "h")))
  })
})

test_that("stack menu: Dissolve removes the stack", {
  local_stack_menus()
  upd <- reactiveVal(list())
  brd <- stack_board(stacks(s1 = new_dock_stack("a", name = "Input")))
  testServer(stack_app(edit_stack_action, reactive("s1"), brd, upd), {
    session$flushReact()
    choose(session, "dissolve")
    expect_identical(upd()$stacks$rm, "s1")
  })
})

test_that("add to stack: lists the stacks and New stack", {
  menus <- local_stack_menus()
  upd <- reactiveVal(list())
  brd <- stack_board(
    stacks(s1 = new_dock_stack("a", name = "Input"), s2 = new_dock_stack("h", name = "Out"))
  )
  testServer(stack_app(add_stack_action, reactive("h"), brd, upd), {
    session$flushReact()
    msg <- menu_of(menus)
    expect_identical(msg$caption, "Add Head to")
    expect_identical(values_of(msg), c("stack:s1", "new"), info = "not the stack it is in")
  })
})

test_that("add to stack: joining a stack leaves the old one", {
  local_stack_menus()
  upd <- reactiveVal(list())
  brd <- stack_board(
    stacks(s1 = new_dock_stack("a", name = "Input"), s2 = new_dock_stack(c("h", "b"), name = "Out"))
  )
  testServer(stack_app(add_stack_action, reactive("h"), brd, upd), {
    session$flushReact()
    choose(session, "stack:s1")
    mod <- upd()$stacks$mod
    expect_identical(mod$s1, list(blocks = c("a", "h")))
    expect_identical(mod$s2, list(blocks = "b"))
  })
})

test_that("add to stack: New stack is called Stack N and takes the blocks", {
  local_stack_menus()
  upd <- reactiveVal(list())
  brd <- stack_board(stacks(s1 = new_dock_stack("a", name = "Stack 1")))
  testServer(stack_app(add_stack_action, reactive(c("a", "h")), brd, upd), {
    session$flushReact()
    choose(session, "new")
    added <- upd()$stacks$add
    expect_length(added, 1L)
    expect_identical(stack_name(added[[1L]]), "Stack 2")
    expect_identical(stack_blocks(added[[1L]]), c("a", "h"))
    expect_identical(upd()$stacks$mod$s1, list(blocks = character()))
  })
})

test_that("add stack fired without blocks asks which blocks, then makes it", {
  menus <- local_stack_menus()
  upd <- reactiveVal(list())
  brd <- stack_board()
  testServer(stack_app(add_stack_action, reactive(TRUE), brd, upd), {
    session$flushReact()
    msg <- menu_of(menus)
    expect_true(msg$multi)
    expect_setequal(values_of(msg), c("a", "b", "h"))
    choose(session, values = list("a", "b"))
    added <- upd()$stacks$add
    expect_identical(stack_blocks(added[[1L]]), c("a", "b"))
  })
})

test_that("add to stack takes ids sent from the browser as a list", {
  menus <- local_stack_menus()
  upd <- reactiveVal(list())
  testServer(stack_app(add_stack_action, reactive(list("a", "h")), stack_board(), upd), {
    session$flushReact()
    expect_identical(menu_of(menus)$caption, "Add 2 blocks to")
  })
})
