# The link menu publishes its committed spec into the parent session's
# namespace as `menu-commit` (list(source, target, link_id, block_input,
# nonce)); the JS binding already composed `source` / `target` from the
# clicked card's `data-direction` and the panel anchor, so the action
# server never reshapes them. Drive that input directly to simulate a
# user committing a link.
commit_menu <- function(session, source, target, link_id,
                        block_input = NULL, nonce = 1L) {
  session$setInputs(
    `menu-commit` = list(
      source = source,
      target = target,
      link_id = link_id,
      block_input = block_input,
      nonce = nonce
    )
  )
}

local_mocked_sidebar <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    show_sidebar         = function(...) invisible(list(...)),
    keep_or_hide_sidebar = function(...) invisible(list(...)),
    hide_sidebar         = function(...) invisible(list(...)),
    .env = env
  )
}

expect_added_link <- function(upd, id, from, to, input) {
  testthat::expect_length(upd, 1L)
  testthat::expect_named(upd, "links")
  testthat::expect_named(upd$links, "add")
  testthat::expect_s3_class(upd$links$add, "links")
  df <- as.data.frame(upd$links$add)
  testthat::expect_identical(df$id, id)
  testthat::expect_identical(df$from, from)
  testthat::expect_identical(df$to, to)
  testthat::expect_identical(df$input, input)
}

test_that("add link action: OUTGOING commit uses the default port", {
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
        add_link_action(
          trigger = reactive("a"),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()
      # Anchor `a` (data block) is the source; card `b` (head) is the
      # target. Arity-1 target renders no port picker, so block_input
      # arrives NULL and the action resolves the single free slot.
      commit_menu(session, source = "a", target = "b", link_id = "ab")

      expect_added_link(
        r_update(), id = "ab", from = "a", to = "b", input = "data"
      )
    }
  )
})

test_that("add link action: OUTGOING commit honours an explicit port", {
  local_mocked_sidebar()
  r_board <- reactiveValues(
    board = new_board(
      c(a = new_dataset_block("iris"), m = new_merge_block())
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
      # Merge target has arity 2 (ports x / y); the user picks `y` in the
      # per-card picker, which the spec carries through verbatim.
      commit_menu(
        session,
        source = "a", target = "m", link_id = "am", block_input = "y"
      )

      expect_added_link(
        r_update(), id = "am", from = "a", to = "m", input = "y"
      )
    }
  )
})

test_that("add link action: variadic target resolves to a positional slot", {
  local_mocked_sidebar()
  r_board <- reactiveValues(
    board = new_board(
      c(a = new_dataset_block("iris"), r = new_rbind_block())
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
      # Variadic target renders no port picker, so block_input arrives
      # NULL; core's name-or-position model treats an integer name as a
      # *named* argument, so the resolved slot must be positional ("").
      commit_menu(session, source = "a", target = "r", link_id = "ar")

      expect_added_link(
        r_update(), id = "ar", from = "a", to = "r", input = ""
      )
    }
  )
})

test_that("resolve_free_input gives a variadic target a positional slot", {
  blocks <- board_blocks(
    new_board(c(a = new_dataset_block("iris"), r = new_rbind_block()))
  )

  expect_identical(resolve_free_input(blocks[["r"]], "r", links()), "")

  # A variadic target already carrying a positional ("") link still
  # resolves to another positional slot.
  positional <- links(id = "ar", from = "a", to = "r", input = "")
  expect_identical(resolve_free_input(blocks[["r"]], "r", positional), "")

  # A legacy integer-named link never makes the resolver generate another
  # integer name; the fresh slot is positional.
  named <- links(id = "ar", from = "a", to = "r", input = "1")
  expect_identical(resolve_free_input(blocks[["r"]], "r", named), "")
})

test_that("add link action: variadic target honours a user-supplied name", {
  local_mocked_sidebar()
  r_board <- reactiveValues(
    board = new_board(
      c(a = new_dataset_block("iris"), r = new_rbind_block())
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
      # The variadic name field carries a typed slot name through as
      # block_input, so the link is created as a named argument.
      commit_menu(
        session, source = "a", target = "r", link_id = "ar",
        block_input = "controls"
      )

      expect_added_link(
        r_update(), id = "ar", from = "a", to = "r", input = "controls"
      )
    }
  )
})

test_that("add link action: a duplicate variadic input name is rejected", {
  local_mocked_sidebar()
  r_board <- reactiveValues(
    board = new_board(
      c(a = new_dataset_block("iris"), b = new_dataset_block("mtcars"),
        r = new_rbind_block()),
      links = links(id = "br", from = "b", to = "r", input = "controls")
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
      # `r` already carries a "controls" input from `b`; core forbids a
      # second identically named input, so the menu rejects the commit.
      commit_menu(
        session, source = "a", target = "r", link_id = "ar",
        block_input = "controls"
      )

      expect_identical(r_update(), list())
    }
  )
})

test_that("link menu renders a name field only for variadic targets", {
  board <- new_board(
    c(a = new_dataset_block("iris"), r = new_rbind_block(),
      m = new_merge_block())
  )

  doc <- xml2::read_html(as.character(link_menu_ui("mid", board, "a")))
  by_class <- function(tok) {
    sprintf(
      "//*[contains(concat(' ', normalize-space(@class), ' '), ' %s ')]", tok
    )
  }
  card <- function(id) {
    xml2::xml_find_first(
      doc,
      paste0(by_class("blockr-link-menu-card"), "[@data-block-type='", id, "']")
    )
  }
  has_field <- function(node, tok) {
    length(xml2::xml_find_all(node, paste0(".", by_class(tok)))) > 0L
  }

  expect_true(
    has_field(card("r"), "blockr-block-browser-field-input-name")
  )
  expect_false(
    has_field(card("r"), "blockr-block-browser-field-block-input")
  )
  expect_false(
    has_field(card("m"), "blockr-block-browser-field-input-name")
  )
  expect_true(
    has_field(card("m"), "blockr-block-browser-field-block-input")
  )

  placeholder <- xml2::xml_attr(
    xml2::xml_find_first(
      card("r"),
      paste0(
        ".", by_class("blockr-block-browser-field-input-name"), "//input"
      )
    ),
    "placeholder"
  )
  expect_identical(placeholder, "leave blank for an unnamed input")
})

test_that("add link action: INCOMING commit targets the anchor", {
  local_mocked_sidebar()
  # Anchor `h` (head) has a free input; the INCOMING section offers `a`
  # as a source. The card click composes source = card, target = anchor.
  r_board <- reactiveValues(
    board = new_board(
      c(a = new_dataset_block("iris"), h = new_head_block())
    ),
    board_id = "my_board"
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        add_link_action(
          trigger = reactive("h"),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()
      commit_menu(session, source = "a", target = "h", link_id = "ah")

      expect_added_link(
        r_update(), id = "ah", from = "a", to = "h", input = "data"
      )
    }
  )
})

test_that("add link action: duplicate link id is rejected by the menu", {
  # Validation now lives in the vendored link menu (which the dock mounts
  # with the board reactive), so a duplicate link id is rejected upstream:
  # the committed reactive never fires and no update is issued. The dock
  # no longer runs its own validator.
  local_mocked_sidebar()

  r_board <- reactiveValues(
    board = new_board(
      c(a = new_dataset_block("iris"), b = new_head_block()),
      links = links(id = "ab", from = "a", to = "b")
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
      commit_menu(session, source = "a", target = "b", link_id = "ab")

      expect_length(r_update(), 0L)
    }
  )
})

test_that("add link action: empty pool still opens the sidebar", {
  # A lone data block: no other blocks to link to and no free inputs of
  # its own, so both pools are empty. The menu owns its empty-state now,
  # so the action opens the sidebar (no pre-flight notify / skip).
  show_calls <- list()
  notify_calls <- list()

  local_mocked_bindings(
    show_sidebar = function(id, ...) {
      show_calls[[length(show_calls) + 1L]] <<- id
      invisible(NULL)
    },
    hide_sidebar = function(...) invisible(NULL),
    keep_or_hide_sidebar = function(...) invisible(NULL)
  )
  local_mocked_bindings(
    notify = function(message, ...) {
      notify_calls[[length(notify_calls) + 1L]] <<- message
      invisible(NULL)
    }
  )

  r_board <- reactiveValues(
    board = new_board(c(a = new_dataset_block("iris"))),
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
      expect_length(show_calls, 1L)
      expect_identical(show_calls[[1L]], "my_board-actions_sidebar")
      expect_length(notify_calls, 0L)
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
