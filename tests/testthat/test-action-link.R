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

# The edit menu's fields are real Shiny inputs at `menu-from`, `menu-to`,
# `menu-input_port` / `menu-input_name`, committed by the `menu-confirm`
# button. Drive them directly to simulate a user editing a link. Only the
# fields the user touches need setting; an untouched field falls back to
# the link's current value.
edit_link_menu <- function(session, from = NULL, to = NULL,
                           input_port = NULL, input_name = NULL, confirm = 1L) {
  vals <- list()
  if (!is.null(from)) vals[["menu-from"]] <- from
  if (!is.null(to)) vals[["menu-to"]] <- to
  if (!is.null(input_port)) vals[["menu-input_port"]] <- input_port
  if (!is.null(input_name)) vals[["menu-input_name"]] <- input_name

  # Set the pickers first and let the selection observers settle (the user
  # picks, then clicks), then commit - mirroring the real two-step flow.
  if (length(vals)) {
    do.call(session$setInputs, vals)
    session$flushReact()
  }
  session$setInputs(`menu-confirm` = confirm)
}

# An edit is committed as a `links$mod` delta: a named list of the changed
# constructor-argument values, keyed by the (unchanged) link id.
expect_link_mod <- function(upd, id, delta) {
  testthat::expect_named(upd, "links")
  testthat::expect_named(upd$links, "mod")
  testthat::expect_named(upd$links$mod, id)
  # A `mod` entry is a partial-arg delta, not a full `links` object.
  testthat::expect_false(inherits(upd$links$mod, "links"))
  testthat::expect_identical(upd$links$mod[[id]], delta)
}

edit_link_env <- function(links, board_id = "b") {
  reactiveValues(
    board = new_board(
      c(
        a = new_dataset_block("iris"),
        b = new_dataset_block("mtcars"),
        h = new_head_block(),
        m = new_merge_block(),
        r = new_rbind_block()
      ),
      links = links
    ),
    board_id = board_id
  )
}

test_that("edit link action: redirecting the target commits a mod delta", {
  local_mocked_sidebar()
  r_board <- edit_link_env(links(l1 = new_link("a", "h", "data")))
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        edit_link_action(
          trigger = reactive("l1"), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()
      # Re-point a -> h onto the merge block's `y` port; from is untouched.
      edit_link_menu(session, to = "m", input_port = "y")

      expect_link_mod(r_update(), "l1", list(to = "m", input = "y"))
    }
  )
})

test_that("edit link action: switching the input slot commits only input", {
  local_mocked_sidebar()
  r_board <- edit_link_env(links(l1 = new_link("a", "m", "x")))
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        edit_link_action(
          trigger = reactive("l1"), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()
      edit_link_menu(session, input_port = "y")

      expect_link_mod(r_update(), "l1", list(input = "y"))
    }
  )
})

test_that("edit link action: naming a variadic positional slot", {
  local_mocked_sidebar()
  r_board <- edit_link_env(links(l1 = new_link("a", "r", "")))
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        edit_link_action(
          trigger = reactive("l1"), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()
      # The variadic target renders a name field; a blank slot becomes named.
      edit_link_menu(session, input_name = "left")

      expect_link_mod(r_update(), "l1", list(input = "left"))
    }
  )
})

test_that("edit link action: redirecting the source commits only from", {
  local_mocked_sidebar()
  r_board <- edit_link_env(links(l1 = new_link("a", "h", "data")))
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        edit_link_action(
          trigger = reactive("l1"), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()
      edit_link_menu(session, from = "b")

      expect_link_mod(r_update(), "l1", list(from = "b"))
    }
  )
})

test_that("edit link action: an unchanged confirm issues no update", {
  local_mocked_sidebar()
  r_board <- edit_link_env(links(l1 = new_link("a", "m", "x")))
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        edit_link_action(
          trigger = reactive("l1"), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()
      # Confirm without touching a field: the delta is empty, so no
      # `links$mod` is emitted (which would needlessly re-evaluate).
      edit_link_menu(session)

      expect_length(r_update(), 0L)
    }
  )
})

test_that("edit link action: a redirect that closes a cycle is rejected", {
  local_mocked_sidebar()
  r_board <- edit_link_env(
    links(l1 = new_link("a", "h", "data"), l2 = new_link("h", "m", "x"))
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        edit_link_action(
          trigger = reactive("l1"), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()
      # l1 is a -> h; `m` already reaches h via l2 (h -> m), so re-pointing
      # l1's source to `m` makes m -> h, which closes a cycle and is
      # rejected (m != h, so it is not caught as a self-link).
      edit_link_menu(session, from = "m")

      expect_length(r_update(), 0L)
    }
  )
})

test_that("edit link action: a self-link is rejected", {
  local_mocked_sidebar()
  r_board <- edit_link_env(links(l1 = new_link("a", "m", "x")))
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        edit_link_action(
          trigger = reactive("l1"), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()
      edit_link_menu(session, from = "m")

      expect_length(r_update(), 0L)
    }
  )
})

test_that("edit link action: removing the edited link closes the sidebar", {
  hide_calls <- list()
  local_mocked_bindings(
    show_sidebar = function(...) invisible(NULL),
    keep_or_hide_sidebar = function(...) invisible(NULL),
    hide_sidebar = function(id, ...) {
      hide_calls[[length(hide_calls) + 1L]] <<- id
      invisible(NULL)
    },
    sidebar_state = function(id, ...) list(open = TRUE, pinned = TRUE)
  )

  r_board <- edit_link_env(links(l1 = new_link("a", "h", "data")))
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        edit_link_action(
          trigger = reactive("l1"), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()
      expect_length(hide_calls, 0L)

      # Remove the edited link elsewhere -> sidebar closes live.
      r_board$board <- new_board(board_blocks(r_board$board))
      session$flushReact()

      expect_gte(length(hide_calls), 1L)
      expect_identical(hide_calls[[1L]], "b-actions_sidebar")
    }
  )
})

test_that("edit link menu ui holds the endpoint / input slots and confirm", {
  board <- new_board(
    c(a = new_dataset_block("iris"), m = new_merge_block()),
    links = links(l1 = new_link("a", "m", "x"))
  )

  doc <- xml2::read_html(as.character(edit_link_menu_ui("mid", board, "l1")))
  by_id <- function(suffix) {
    xml2::xml_find_first(doc, paste0("//*[@id='mid-", suffix, "']"))
  }

  # From / to and the port / name control are server-rendered into these
  # uiOutput slots (so a board change refreshes them live).
  expect_false(is.na(xml2::xml_attr(by_id("endpoints"), "id")))
  expect_false(is.na(xml2::xml_attr(by_id("input_field"), "id")))
  confirm <- xml2::xml_find_first(
    doc,
    paste0(
      "//*[contains(concat(' ', normalize-space(@class), ' '),",
      " ' blockr-link-edit-confirm ')]"
    )
  )
  expect_false(is.na(xml2::xml_attr(confirm, "class")))
})

test_that("edit link menu renders rich source / target pickers", {
  r_board <- reactiveValues(
    board = new_board(
      c(a = new_dataset_block("iris"), m = new_merge_block()),
      links = links(l1 = new_link("a", "m", "x"))
    ),
    board_id = "b"
  )

  testServer(
    function(id, ...) {
      moduleServer(id, function(input, output, session) {
        edit_link_menu_server(
          "menu", board = reactive(r_board$board), link_id = reactive("l1")
        )
      })
    },
    {
      session$flushReact()
      ep <- as.character(output$`menu-endpoints`$html)
      # Both pickers, rendered with the block-browser selectize (the
      # add-panel look), not a bare <select>.
      expect_true(grepl("menu-from", ep, fixed = TRUE))
      expect_true(grepl("menu-to", ep, fixed = TRUE))
      expect_true(grepl("selectize", ep))
    }
  )
})

test_that("edit link input field follows the switched link, not stale input", {
  # Editing one link then another through the persistent menu module must
  # not let the first link's target leak into the second: switching to a
  # variadic-target link has to render the NAME control even though
  # `input$to` still holds the previous (finite) target.
  r_board <- reactiveValues(
    board = new_board(
      c(a = new_dataset_block("iris"), m = new_merge_block(),
        r = new_rbind_block()),
      links = links(l1 = new_link("a", "m", "x"), l2 = new_link("a", "r", ""))
    ),
    board_id = "b"
  )
  lid <- reactiveVal("l1")

  testServer(
    function(id, ...) {
      moduleServer(id, function(input, output, session) {
        edit_link_menu_server(
          "menu", board = reactive(r_board$board), link_id = lid
        )
      })
    },
    {
      session$flushReact()
      session$setInputs(`menu-to` = "m")
      session$flushReact()
      port <- as.character(output$`menu-input_field`$html)
      expect_true(grepl("menu-input_port", port, fixed = TRUE))

      lid("l2")
      session$flushReact()
      name <- as.character(output$`menu-input_field`$html)
      expect_true(grepl("menu-input_name", name, fixed = TRUE))
      expect_false(grepl("menu-input_port", name, fixed = TRUE))
    }
  )
})

test_that("edit link menu ui is empty for an unknown link", {
  board <- new_board(c(a = new_dataset_block("iris")))
  doc <- xml2::read_html(as.character(edit_link_menu_ui("mid", board, "gone")))
  notice <- xml2::xml_find_first(
    doc,
    paste0(
      "//*[contains(concat(' ', normalize-space(@class), ' '),",
      " ' blockr-block-browser-empty ')]"
    )
  )
  expect_match(xml2::xml_text(notice), "no longer on the board")
})

test_that("edit link input field switches on target arity", {
  board <- new_board(
    c(a = new_dataset_block("iris"), m = new_merge_block(),
      r = new_rbind_block()),
    links = links(l1 = new_link("a", "m", "x"))
  )
  row <- edit_link_row(board, "l1")
  ns <- function(x) paste0("mid-", x)

  finite <- as.character(edit_link_input_field(ns, board, "l1", "m", row))
  expect_true(grepl("mid-input_port", finite, fixed = TRUE))
  # The edited link's own slot reads as free, so both merge ports appear.
  expect_true(grepl(">x<", finite) && grepl(">y<", finite))

  variadic <- as.character(edit_link_input_field(ns, board, "l1", "r", row))
  expect_true(grepl("mid-input_name", variadic, fixed = TRUE))
})

test_that("edit link helpers: row lookup, exclusion and delta", {
  board <- new_board(
    c(a = new_dataset_block("iris"), m = new_merge_block()),
    links = links(l1 = new_link("a", "m", "x"), l2 = new_link("a", "m", "y"))
  )

  expect_identical(
    edit_link_row(board, "l1"),
    list(from = "a", to = "m", input = "x")
  )
  expect_null(edit_link_row(board, "nope"))
  expect_null(edit_link_row(board, NULL))

  expect_identical(names(links_without(board, "l1")), "l2")

  expect_length(
    edit_link_delta(list(from = "a", to = "m", input = "x"), board, "l1"),
    0L
  )
  expect_identical(
    edit_link_delta(list(from = "m", to = "m", input = "z"), board, "l1"),
    list(from = "m", input = "z")
  )
})
