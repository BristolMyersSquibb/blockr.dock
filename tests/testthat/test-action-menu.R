# The link actions open menus (action-menu.R) instead of sidebar forms
# (#544). open_action_menu() is mocked to record what would be sent; a pick
# comes back on the action module's `pick` input.

local_menus <- function(env = parent.frame()) {
  menus <- new.env()
  menus$sent <- list()
  menus$added <- list()
  local_mocked_bindings(
    open_action_menu = function(...) {
      msg <- list(...)
      menus$sent <- c(menus$sent, list(msg))
      invisible(msg)
    },
    open_add_block_menu = function(mode, ...) {
      menus$added <- c(menus$added, list(mode))
      invisible(NULL)
    },
    .env = env
  )
  menus
}

last_menu <- function(menus) menus$sent[[length(menus$sent)]]

row_values <- function(msg) {
  vals <- lapply(msg$items, `[[`, "value")
  unlist(vals[!vapply(vals, is.null, logical(1L))])
}

pick <- function(session, value = NULL, field = NULL, nonce = 1L) {
  session$setInputs(
    pick = Filter(Negate(is.null), list(value = value, field = field, nonce = nonce))
  )
}

link_board <- function(lnks = links()) {
  reactiveValues(
    board = new_board(
      c(
        a = new_dataset_block("iris"),
        b = new_dataset_block("mtcars"),
        h = new_head_block(),
        m = new_merge_block(),
        r = new_rbind_block()
      ),
      links = lnks
    ),
    board_id = "brd"
  )
}

action_app <- function(action, trigger, board, update) {
  function(id, ...) {
    moduleServer(id, action(trigger = trigger, board = board, update = update))
  }
}

test_that("connect: the menu lists where the block can send and take from", {
  menus <- local_menus()
  upd <- reactiveVal(list())
  testServer(action_app(add_link_action, reactive("h"), link_board(), upd), {
    session$flushReact()
    msg <- last_menu(menus)
    expect_identical(msg$caption, "Connect Head")
    titles <- unlist(lapply(msg$items, `[[`, "title"))
    expect_identical(titles, c("Send to", "Take from"))
    vals <- row_values(msg)
    expect_true(all(c("to:m", "to:r") %in% vals))
    expect_true(all(c("from:a", "from:b") %in% vals))
    expect_false("to:a" %in% vals, "a dataset takes no input")
  })
})

test_that("connect: a target with one free input links at once", {
  menus <- local_menus()
  upd <- reactiveVal(list())
  testServer(action_app(add_link_action, reactive("a"), link_board(), upd), {
    session$flushReact()
    pick(session, "to:h")
    df <- as.data.frame(upd()$links$add)
    expect_identical(df$from, "a")
    expect_identical(df$to, "h")
    expect_identical(df$input, "data")
    expect_length(menus$sent, 1L)
  })
})

test_that("connect: a target with two free inputs asks which, then links", {
  menus <- local_menus()
  upd <- reactiveVal(list())
  testServer(action_app(add_link_action, reactive("a"), link_board(), upd), {
    session$flushReact()
    pick(session, "to:m")
    expect_length(upd(), 0L)
    second <- last_menu(menus)
    expect_identical(row_values(second), c("to:m:x", "to:m:y"))
    expect_identical(second$back$caption, "Connect Dataset")
    pick(session, "to:m:y", nonce = 2L)
    df <- as.data.frame(upd()$links$add)
    expect_identical(df$to, "m")
    expect_identical(df$input, "y")
  })
})

test_that("connect: a variadic target takes a new positional input", {
  local_menus()
  upd <- reactiveVal(list())
  testServer(action_app(add_link_action, reactive("a"), link_board(), upd), {
    session$flushReact()
    pick(session, "to:r")
    df <- as.data.frame(upd()$links$add)
    expect_identical(df$to, "r")
    expect_identical(df$input, "")
  })
})

test_that("connect: taking from another block links into this one", {
  local_menus()
  upd <- reactiveVal(list())
  testServer(action_app(add_link_action, reactive("h"), link_board(), upd), {
    session$flushReact()
    pick(session, "from:b")
    df <- as.data.frame(upd()$links$add)
    expect_identical(df$from, "b")
    expect_identical(df$to, "h")
  })
})

test_that("link menu: rows follow the target", {
  menus <- local_menus()
  upd <- reactiveVal(list())
  brd <- link_board(
    links(
      ar = new_link("a", "r", ""),
      am = new_link("a", "m", "x")
    )
  )
  testServer(action_app(edit_link_action, reactive("ar"), brd, upd), {
    session$flushReact()
    msg <- last_menu(menus)
    expect_identical(msg$head$title, "Dataset → Rbind")
    expect_identical(msg$head$text, "into an unnamed input")
    expect_identical(row_values(msg), c("insert", "rename", "source", "remove"))
  })
  testServer(action_app(edit_link_action, reactive("am"), brd, upd), {
    session$flushReact()
    msg <- last_menu(menus)
    expect_identical(msg$head$text, "into x")
    expect_identical(row_values(msg), c("insert", "move:y", "source", "remove"))
  })
})

test_that("link menu: Rename input opens a field and renames", {
  menus <- local_menus()
  upd <- reactiveVal(list())
  brd <- link_board(
    links(ar = new_link("a", "r", "first"), br = new_link("b", "r", "second"))
  )
  testServer(action_app(edit_link_action, reactive("ar"), brd, upd), {
    session$flushReact()
    pick(session, "rename")
    fld <- last_menu(menus)$field
    expect_identical(fld$value, "first")
    expect_identical(unlist(fld$taken), "second")
    expect_true(fld$empty_ok)
    pick(session, field = "one", nonce = 2L)
    expect_identical(upd()$links$mod$ar, list(input = "one"))
  })
})

test_that("link menu: a taken input name is refused", {
  local_menus()
  upd <- reactiveVal(list())
  brd <- link_board(
    links(ar = new_link("a", "r", "first"), br = new_link("b", "r", "second"))
  )
  testServer(action_app(edit_link_action, reactive("ar"), brd, upd), {
    session$flushReact()
    pick(session, field = "second")
    expect_length(upd(), 0L)
  })
})

test_that("link menu: Move to input moves it", {
  local_menus()
  upd <- reactiveVal(list())
  brd <- link_board(links(am = new_link("a", "m", "x")))
  testServer(action_app(edit_link_action, reactive("am"), brd, upd), {
    session$flushReact()
    pick(session, "move:y")
    expect_identical(upd()$links$mod$am, list(input = "y"))
  })
})

test_that("link menu: Change source offers blocks that keep the graph acyclic", {
  menus <- local_menus()
  upd <- reactiveVal(list())
  brd <- link_board(
    links(ah = new_link("a", "h", "data"), hr = new_link("h", "r", ""))
  )
  testServer(action_app(edit_link_action, reactive("ah"), brd, upd), {
    session$flushReact()
    pick(session, "source")
    vals <- row_values(last_menu(menus))
    expect_true("source:b" %in% vals)
    expect_false("source:r" %in% vals, "r is downstream of h")
    expect_false("source:a" %in% vals, "a is the source already")
    pick(session, "source:b", nonce = 2L)
    expect_identical(upd()$links$mod$ah, list(from = "b"))
  })
})

test_that("link menu: Remove and Insert", {
  menus <- local_menus()
  upd <- reactiveVal(list())
  brd <- link_board(links(ah = new_link("a", "h", "data")))
  testServer(action_app(edit_link_action, reactive("ah"), brd, upd), {
    session$flushReact()
    pick(session, "insert")
    expect_identical(menus$added, list("insert"))
    pick(session, "remove", nonce = 2L)
    expect_identical(upd()$links$rm, "ah")
  })
})

test_that("an action menu row drops what it does not set", {
  row <- menu_row("Remove", "remove", danger = TRUE)
  expect_identical(row, list(label = "Remove", value = "remove", danger = TRUE))
  expect_identical(pick_step("move:y"), "move")
  expect_identical(pick_arg("move:y"), "y")
  expect_null(pick_arg("move"))
})
