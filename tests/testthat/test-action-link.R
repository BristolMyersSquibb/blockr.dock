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
  local_mocked_bindings(
    show_sidebar         = function(...) invisible(list(...)),
    keep_or_hide_sidebar = function(...) invisible(list(...)),
    hide_sidebar         = function(...) invisible(list(...)),
    .env = env
  )
}

expect_added_link <- function(upd, id, from, to, input) {
  expect_length(upd, 1L)
  expect_named(upd, "links")
  expect_named(upd$links, "add")
  expect_s3_class(upd$links$add, "links")
  df <- as.data.frame(upd$links$add)
  expect_identical(df$id, id)
  expect_identical(df$from, from)
  expect_identical(df$to, to)
  expect_identical(df$input, input)
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

test_that("add link action: a commit closes an unpinned panel only", {
  # The connect menu syncs its own cards off the board, so a commit never
  # re-pushes the panel body - it only closes an unpinned panel.
  calls <- function(pinned) {
    seen <- list(show = 0L, hide = 0L)
    local_mocked_bindings(
      show_sidebar = function(...) seen$show <<- seen$show + 1L,
      hide_sidebar = function(...) seen$hide <<- seen$hide + 1L,
      keep_or_hide_sidebar = function(...) {
        stop("the panel must not be rebuilt after a commit")
      },
      sidebar_state = function(id, ...) list(open = TRUE, pinned = pinned)
    )

    r_board <- reactiveValues(
      board = new_board(
        c(a = new_dataset_block("iris"), b = new_head_block())
      ),
      board_id = "b"
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
        commit_menu(session, source = "a", target = "b", link_id = "ab")
      }
    )

    seen
  }

  expect_identical(calls(pinned = FALSE), list(show = 1L, hide = 1L))
  expect_identical(calls(pinned = TRUE), list(show = 1L, hide = 0L))
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
  expect_named(upd, "links")
  expect_named(upd$links, "mod")
  expect_named(upd$links$mod, id)
  # A `mod` entry is a partial-arg delta, not a full `links` object.
  expect_false(inherits(upd$links$mod, "links"))
  expect_identical(upd$links$mod[[id]], delta)
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
        "edit_link_action",
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
        "edit_link_action",
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
        "edit_link_action",
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
        "edit_link_action",
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
        "edit_link_action",
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
        "edit_link_action",
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
        "edit_link_action",
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

test_that("edit link action: a commit closes an unpinned panel only", {
  # The endpoint pickers and the input-slot control are `uiOutput`s on the
  # board, so a pinned panel refreshes itself instead of being rebuilt.
  calls <- function(pinned) {
    seen <- list(show = 0L, hide = 0L)
    local_mocked_bindings(
      show_sidebar = function(...) seen$show <<- seen$show + 1L,
      hide_sidebar = function(...) seen$hide <<- seen$hide + 1L,
      keep_or_hide_sidebar = function(...) {
        stop("the panel must not be rebuilt after a commit")
      },
      sidebar_state = function(id, ...) list(open = TRUE, pinned = pinned)
    )

    r_board <- edit_link_env(links(l1 = new_link("a", "h", "data")))

    testServer(
      function(id, ...) {
        moduleServer(
          id,
          edit_link_action(
            trigger = reactive("l1"),
            board = r_board,
            update = reactiveVal(list())
          )
        )
      },
      {
        session$flushReact()
        edit_link_menu(session, to = "m", input_port = "y")
      }
    )

    seen
  }

  expect_identical(calls(pinned = FALSE), list(show = 1L, hide = 1L))
  expect_identical(calls(pinned = TRUE), list(show = 1L, hide = 0L))
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
    # The auto-close is gated on this action owning the open panel.
    sidebar_state = function(id, ...) {
      list(open = TRUE, pinned = TRUE, owner = "edit_link_action")
    }
  )

  r_board <- edit_link_env(links(l1 = new_link("a", "h", "data")))
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        "edit_link_action",
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

test_that("edit link action: a form written by another action stays open", {
  hide_calls <- list()
  local_mocked_bindings(
    show_sidebar = function(...) invisible(NULL),
    keep_or_hide_sidebar = function(...) invisible(NULL),
    hide_sidebar = function(id, ...) {
      hide_calls[[length(hide_calls) + 1L]] <<- id
      invisible(NULL)
    },
    sidebar_state = function(id, ...) {
      list(open = TRUE, pinned = TRUE, owner = "edit_stack_action")
    }
  )

  r_board <- edit_link_env(links(l1 = new_link("a", "h", "data")))

  testServer(
    function(id, ...) {
      moduleServer(
        "edit_link_action",
        edit_link_action(
          trigger = reactive("l1"),
          board = r_board,
          update = reactiveVal(list())
        )
      )
    },
    {
      session$flushReact()

      r_board$board <- new_board(board_blocks(r_board$board))
      session$flushReact()

      expect_length(hide_calls, 0L)
    }
  )
})

test_that("link actions write the sidebar from their own module", {
  # `show_sidebar()` reads the panel's owner off the session it is called
  # with, so a write has to happen in the action's own reactive domain. A
  # write deferred into a flush callback would run under the root session
  # and stamp that instead, which this records.
  wrote_from <- list()
  local_mocked_bindings(
    show_sidebar = function(...) {
      wrote_from[[length(wrote_from) + 1L]] <<- get_session()$ns(NULL)
      invisible(NULL)
    },
    keep_or_hide_sidebar = function(...) invisible(NULL),
    hide_sidebar = function(...) invisible(NULL)
  )

  r_board <- edit_link_env(links(l1 = new_link("a", "h", "data")))

  fire_action(add_link_action, "a", r_board)
  fire_action(edit_link_action, "l1", r_board)

  expect_identical(wrote_from, list("add_link_action", "edit_link_action"))
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

test_that("edit link target picker offers only blocks with free capacity", {
  board <- new_board(
    c(a = new_dataset_block("iris"), b = new_dataset_block("mtcars"),
      m = new_merge_block(), r = new_rbind_block(), h = new_head_block()),
    links = links(l1 = new_link("a", "m", "x"), l3 = new_link("m", "h", "data"))
  )

  # Editing l3 (m -> h): source-only datasets a / b (arity 0) are dropped;
  # m (free y port), r (variadic) and h (its own slot freed) are offered.
  expect_setequal(edit_link_target_ids(board, "l3"), c("m", "r", "h"))
  expect_false("a" %in% edit_link_target_ids(board, "l3"))
  expect_true("h" %in% edit_link_target_ids(board, "l3"))
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

# --- insert block action --------------------------------------------------
#
# Triggered with a link id. The browser publishes its commit as
# `browser-commit` (as for the add / append flows) and the action turns it
# into one update that drops the split link and adds the two new ones.

insert_board <- function(...) {
  reactiveValues(
    board = new_board(
      c(a = new_dataset_block("iris"), b = new_head_block()),
      links = c(l1 = new_link("a", "b", "data"))
    ),
    board_id = "my_board",
    ...
  )
}

# The update has to be read while the session is alive: a `reactiveVal` is
# destroyed with it, so capture inside the block rather than returning it.
run_insert <- function(r_board, r_update, spec, link = "l1") {

  out <- NULL

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        insert_block_action(
          trigger = reactive(link), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()
      session$setInputs(`browser-commit` = spec)
      out <<- r_update()
    }
  )

  out
}

wiring <- function(links) {
  df <- as.data.frame(links)
  paste0(df$from, ">", df$to, ">", df$input)
}

test_that("insert block action: the split link goes, two links replace it", {
  local_mocked_sidebar()

  upd <- run_insert(
    insert_board(),
    reactiveVal(list()),
    list(type = "head_block", id = "c", nonce = 1L)
  )

  expect_named(upd, c("blocks", "links"))
  expect_named(upd$blocks, "add")
  expect_named(upd$blocks$add, "c")

  # Removal and addition travel together: `modify_board_links()` drops
  # before it adds, so the far end's slot is free when `c > b` claims it.
  expect_named(upd$links, c("add", "rm"))
  expect_identical(upd$links$rm, "l1")
  expect_setequal(wiring(upd$links$add), c("a>c>data", "c>b>data"))
})

test_that("insert block action: the far end keeps the slot it had", {
  local_mocked_sidebar()

  # A variadic target names its incoming entries, so the slot is the
  # entry's identity: inheriting it is what makes this an insertion rather
  # than a rewire. Only the split link's own slot may be reused.
  r_board <- reactiveValues(
    board = new_board(
      c(a = new_dataset_block("iris"), z = new_dataset_block("mtcars"),
        m = new_rbind_block()),
      links = c(
        l1 = new_link("a", "m", "first"), l2 = new_link("z", "m", "second")
      )
    ),
    board_id = "my_board"
  )

  upd <- run_insert(
    r_board, reactiveVal(list()),
    list(type = "head_block", id = "c", nonce = 1L)
  )

  expect_identical(upd$links$rm, "l1")
  expect_setequal(wiring(upd$links$add), c("a>c>data", "c>m>first"))
})

test_that("insert block action: an explicit slot on the new block wins", {
  local_mocked_sidebar()

  upd <- run_insert(
    insert_board(), reactiveVal(list()),
    list(type = "merge_block", id = "c", block_input = "y", nonce = 1L)
  )

  expect_setequal(wiring(upd$links$add), c("a>c>y", "c>b>data"))
})

test_that("insert block action: the two link ids are fresh", {
  local_mocked_sidebar()

  upd <- run_insert(
    insert_board(), reactiveVal(list()),
    list(type = "head_block", id = "c", nonce = 1L)
  )

  ids <- names(upd$links$add)

  expect_length(ids, 2L)
  expect_length(unique(ids), 2L)
  expect_false("l1" %in% ids)
})

test_that("insert block action: a link that has gone commits nothing", {
  local_mocked_sidebar()

  # The panel was opened on a link that has since been removed. Applying
  # the block alone would strand it off the graph.
  upd <- run_insert(
    insert_board(), reactiveVal(list()),
    list(type = "head_block", id = "c", nonce = 1L),
    link = "gone"
  )

  expect_identical(upd, list())
})

test_that("insert panel: cards, an Insert button, and the wire's two ends", {

  board <- new_board(
    c(a = new_dataset_block("iris"), b = new_head_block()),
    links = c(l1 = new_link("a", "b", "data"))
  )

  html <- as.character(block_browser_ui("b", board, insert_into("l1")))
  doc <- xml2::read_html(html)

  cards <- xml2::xml_find_all(
    doc, "//*[contains(@class, 'blockr-block-browser-card')]"
  )
  types <- xml2::xml_attr(cards, "data-block-type")

  # Eligibility matches append: the inserted block has to receive from the
  # link's source, so a source-only block is not on offer.
  expect_true("head_block" %in% types)
  expect_false("dataset_block" %in% types)

  # The context names both ends of the wire being split, so the user can see
  # what they are inserting into.
  context <- xml2::xml_text(
    xml2::xml_find_first(
      doc, "//*[contains(@class, 'blockr-block-browser-context')]"
    )
  )
  # Both ends are named the way the user sees them, which is the block name
  # rather than the id.
  expect_match(context, "Insert into")
  expect_match(context, "Dataset")
  expect_match(context, "Head")

  expect_match(html, "Insert", fixed = TRUE)

  # Both new link ids are generated, so the panel asks for neither.
  expect_true(
    is.na(
      xml2::xml_attr(
        xml2::xml_find_first(
          doc, "//*[contains(@class, 'blockr-block-browser-field-link-id')]"
        ),
        "class"
      )
    )
  )
})

test_that("insert panel: a link that has gone renders without context", {

  board <- new_board(c(a = new_dataset_block("iris")))

  html <- as.character(block_browser_ui("b", board, insert_into("gone")))

  expect_false(grepl("Insert into", html, fixed = TRUE))
})
