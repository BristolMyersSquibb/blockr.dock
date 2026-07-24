# The edit-inputs menu publishes one command per user gesture into the
# parent session's namespace as `menu-commit`: a `reorder` (new slot
# order as link ids), a `rename` (a slot's new name) or a `remove` (a
# slot's link id). Drive that input directly to simulate the JS binding.
inputs_commit <- function(session, action, order = NULL, link_id = NULL,
                          name = NULL, nonce = 1L) {
  session$setInputs(
    `menu-commit` = list(
      action = action,
      order = order,
      link_id = link_id,
      name = name,
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

inputs_board <- function(links, board_id = "b") {
  reactiveValues(
    board = new_board(
      c(
        a = new_dataset_block("iris"),
        b = new_dataset_block("mtcars"),
        c = new_dataset_block("airquality"),
        m = new_merge_block(),
        r = new_rbind_block()
      ),
      links = links
    ),
    board_id = board_id
  )
}

variadic_links <- function() {
  links(
    l1 = new_link("a", "r", ""),
    l2 = new_link("b", "r", ""),
    l3 = new_link("c", "r", "")
  )
}

test_that("block_incoming_links returns rows in board order", {
  board <- new_board(
    c(a = new_dataset_block("iris"), b = new_dataset_block("mtcars"),
      r = new_rbind_block()),
    links = links(
      l1 = new_link("a", "r", ""),
      l2 = new_link("b", "r", "keep")
    )
  )

  rows <- block_incoming_links(board, "r")

  expect_identical(rows$id, c("l1", "l2"))
  expect_identical(rows$from, c("a", "b"))
  expect_identical(rows$input, c("", "keep"))

  expect_identical(nrow(block_incoming_links(board, "a")), 0L)
})

test_that("reorder delta permutes `from` across fixed slots", {
  board <- new_board(
    c(a = new_dataset_block("iris"), b = new_dataset_block("mtcars"),
      c = new_dataset_block("airquality"), r = new_rbind_block()),
    links = variadic_links()
  )

  # Drag c's row (l3) to the top: each whole row moves to its new slot,
  # keeping the links' ids and positions; with every input unnamed here,
  # only the sources change.
  expect_identical(
    edit_inputs_reorder_delta(board, "r", c("l3", "l1", "l2")),
    list(l1 = list(from = "c"), l2 = list(from = "a"), l3 = list(from = "b"))
  )

  # Only the slots whose source actually changed appear in the delta.
  expect_identical(
    edit_inputs_reorder_delta(board, "r", c("l2", "l1", "l3")),
    list(l1 = list(from = "b"), l2 = list(from = "a"))
  )

  # The identity order changes nothing.
  expect_length(
    edit_inputs_reorder_delta(board, "r", c("l1", "l2", "l3")), 0L
  )

  # A non-permutation (stale / missing id) is ignored.
  expect_length(edit_inputs_reorder_delta(board, "r", c("l1", "l2")), 0L)
  expect_length(
    edit_inputs_reorder_delta(board, "r", c("l1", "l2", "x")), 0L
  )
})

test_that("reorder delta carries a named input's name with its row", {
  board <- new_board(
    c(a = new_dataset_block("iris"), b = new_dataset_block("mtcars"),
      c = new_dataset_block("airquality"), r = new_rbind_block()),
    links = links(
      l1 = new_link("a", "r", ""),
      l2 = new_link("b", "r", "keep"),
      l3 = new_link("c", "r", "")
    )
  )

  # Drag l2's row (source b, name "keep") to the top: the whole row moves,
  # so the name travels with its source instead of being left at slot l2.
  expect_identical(
    edit_inputs_reorder_delta(board, "r", c("l2", "l1", "l3")),
    list(
      l1 = list(from = "b", input = "keep"),
      l2 = list(from = "a", input = "")
    )
  )
})

test_that("rename delta names / clears a variadic slot", {
  board <- new_board(
    c(a = new_dataset_block("iris"), b = new_dataset_block("mtcars"),
      r = new_rbind_block()),
    links = links(
      l1 = new_link("a", "r", ""),
      l2 = new_link("b", "r", "keep")
    )
  )

  expect_identical(
    edit_inputs_rename_delta(board, "r", "l1", "left", NULL),
    list(l1 = list(input = "left"))
  )

  # Clearing a name returns the slot to positional.
  expect_identical(
    edit_inputs_rename_delta(board, "r", "l2", "", NULL),
    list(l2 = list(input = ""))
  )

  # An unchanged name is a no-op.
  expect_length(edit_inputs_rename_delta(board, "r", "l2", "keep", NULL), 0L)

  # A link that is not on the block is a no-op.
  expect_length(edit_inputs_rename_delta(board, "r", "nope", "x", NULL), 0L)
})

test_that("edit inputs action: reorder commits a from-permutation mod", {
  local_mocked_sidebar()
  r_board <- inputs_board(variadic_links())
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        edit_inputs_action(
          trigger = reactive("r"), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()
      inputs_commit(session, "reorder", order = c("l3", "l1", "l2"))

      upd <- r_update()
      expect_named(upd, "links")
      expect_named(upd$links, "mod")
      expect_identical(
        upd$links$mod,
        list(
          l1 = list(from = "c"),
          l2 = list(from = "a"),
          l3 = list(from = "b")
        )
      )
    }
  )
})

test_that("edit inputs action: an identity reorder issues no update", {
  local_mocked_sidebar()
  r_board <- inputs_board(variadic_links())
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        edit_inputs_action(
          trigger = reactive("r"), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()
      inputs_commit(session, "reorder", order = c("l1", "l2", "l3"))

      expect_length(r_update(), 0L)
    }
  )
})

test_that("edit inputs action: rename commits an input mod", {
  local_mocked_sidebar()
  r_board <- inputs_board(variadic_links())
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        edit_inputs_action(
          trigger = reactive("r"), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()
      inputs_commit(session, "rename", link_id = "l2", name = "keep")

      upd <- r_update()
      expect_named(upd$links, "mod")
      expect_identical(upd$links$mod, list(l2 = list(input = "keep")))
    }
  )
})

test_that("edit inputs action: a duplicate input name is rejected", {
  local_mocked_sidebar()
  r_board <- inputs_board(
    links(
      l1 = new_link("a", "r", "keep"),
      l2 = new_link("b", "r", "")
    )
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        edit_inputs_action(
          trigger = reactive("r"), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()
      # `l1` already carries "keep"; core forbids a second identically
      # named input, so the menu rejects the rename.
      inputs_commit(session, "rename", link_id = "l2", name = "keep")

      expect_length(r_update(), 0L)
    }
  )
})

test_that("edit inputs action: remove commits a links rm", {
  local_mocked_sidebar()
  r_board <- inputs_board(variadic_links())
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        edit_inputs_action(
          trigger = reactive("r"), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()
      inputs_commit(session, "remove", link_id = "l2")

      upd <- r_update()
      expect_named(upd$links, "rm")
      expect_identical(upd$links$rm, "l2")
    }
  )
})

test_that("edit inputs action: removing the block closes the sidebar", {
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

  r_board <- inputs_board(variadic_links())
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        edit_inputs_action(
          trigger = reactive("r"), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()
      expect_length(hide_calls, 0L)

      # Drop the block whose inputs are being edited -> sidebar closes.
      r_board$board <- new_board(board_blocks(r_board$board)["a"])
      session$flushReact()

      expect_gte(length(hide_calls), 1L)
      expect_identical(hide_calls[[1L]], "b-actions_sidebar")
    }
  )
})

test_that("edit inputs menu server renders the row list", {
  r_board <- reactiveValues(
    board = new_board(
      c(a = new_dataset_block("iris"), b = new_dataset_block("mtcars"),
        r = new_rbind_block()),
      links = links(
        l1 = new_link("a", "r", ""),
        l2 = new_link("b", "r", "keep")
      )
    ),
    board_id = "b"
  )

  testServer(
    function(id, ...) {
      moduleServer(id, function(input, output, session) {
        edit_inputs_menu_server(
          "menu", board = reactive(r_board$board), block_id = reactive("r")
        )
      })
    },
    {
      session$flushReact()
      html <- as.character(output$`menu-rows`$html)
      expect_true(grepl("blockr-inputs-row", html, fixed = TRUE))
      expect_true(grepl("data-link-id=\"l1\"", html, fixed = TRUE))
      expect_true(grepl("data-link-id=\"l2\"", html, fixed = TRUE))
    }
  )
})

test_that("variadic block renders an ordered, editable row list", {
  board <- new_board(
    c(a = new_dataset_block("iris"), b = new_dataset_block("mtcars"),
      r = new_rbind_block()),
    links = links(
      l1 = new_link("a", "r", ""),
      l2 = new_link("b", "r", "keep")
    )
  )

  doc <- xml2::read_html(as.character(edit_inputs_rows(NS("mid"), board, "r")))
  by_class <- function(tok) {
    sprintf(
      "//*[contains(concat(' ', normalize-space(@class), ' '), ' %s ')]", tok
    )
  }
  rows <- xml2::xml_find_all(doc, by_class("blockr-inputs-row"))

  expect_length(rows, 2L)
  expect_identical(
    xml2::xml_attr(rows, "data-link-id"), c("l1", "l2")
  )

  # Each row carries a drag handle, a name field and a remove button.
  handle <- xml2::xml_find_all(rows[[1]], paste0(
    ".", by_class("blockr-inputs-drag-handle")
  ))
  expect_length(handle, 1L)

  names_in <- xml2::xml_find_all(doc, paste0(
    by_class("blockr-inputs-name-input")
  ))
  expect_identical(xml2::xml_attr(names_in, "value"), c("", "keep"))

  expect_length(
    xml2::xml_find_all(doc, by_class("blockr-inputs-remove")), 2L
  )
})

test_that("finite block renders a block-picker selectize per port", {
  board <- new_board(
    c(a = new_dataset_block("iris"), b = new_dataset_block("mtcars"),
      m = new_merge_block()),
    links = links(l1 = new_link("a", "m", "x"))
  )

  html <- as.character(edit_inputs_rows(NS("mid"), board, "m"))
  doc <- xml2::read_html(html)
  by_class <- function(tok) {
    sprintf(
      "//*[contains(concat(' ', normalize-space(@class), ' '), ' %s ')]", tok
    )
  }
  rows <- xml2::xml_find_all(doc, by_class("blockr-inputs-row"))

  # One row per declared port, in port order.
  expect_identical(xml2::xml_attr(rows, "data-port"), c("x", "y"))

  # Each port is a block-browser selectize (not a bare <select>): the
  # namespaced input id and the selectize config are present.
  expect_true(grepl("mid-src_x", html, fixed = TRUE))
  expect_true(grepl("mid-src_y", html, fixed = TRUE))
  expect_true(grepl("selectize", html))

  # Port x preselects its current source `a` via the selectize's items.
  expect_match(html, '"items"')
  expect_match(html, '"a"')
})

test_that("edit inputs menu ui carries the block / arity markers", {
  board <- new_board(
    c(a = new_dataset_block("iris"), r = new_rbind_block()),
    links = links(l1 = new_link("a", "r", ""))
  )

  doc <- xml2::read_html(as.character(edit_inputs_menu_ui("mid", board, "r")))
  root <- xml2::xml_find_first(doc, "//*[@id='mid-commit']")
  expect_identical(xml2::xml_attr(root, "data-block"), "r")
  expect_identical(xml2::xml_attr(root, "data-variadic"), "true")

  # A finite block is flagged non-variadic.
  fin <- xml2::read_html(
    as.character(edit_inputs_menu_ui("mid", board, "a"))
  )
  root_fin <- xml2::xml_find_first(fin, "//*[@id='mid-commit']")
  expect_identical(xml2::xml_attr(root_fin, "data-variadic"), "false")
})

test_that("Add input section is shown for variadic blocks only", {
  by_class <- function(tok) {
    sprintf(
      "//*[contains(concat(' ', normalize-space(@class), ' '), ' %s ')]", tok
    )
  }
  has_add <- function(board, blk) {
    doc <- xml2::read_html(
      as.character(edit_inputs_rows(NS("mid"), board, blk))
    )
    length(xml2::xml_find_all(doc, by_class("blockr-inputs-add-section"))) > 0L
  }

  # Variadic -> an "Add input" section (a block picker; the slot count is
  # open-ended).
  vb <- new_board(
    c(a = new_dataset_block("iris"), r = new_rbind_block()),
    links = links(l1 = new_link("a", "r", ""))
  )
  expect_true(has_add(vb, "r"))

  # Finite -> no add section even with a free port; ports are fixed and are
  # connected through their per-row pickers instead.
  mb <- new_board(
    c(a = new_dataset_block("iris"), m = new_merge_block()),
    links = links(l1 = new_link("a", "m", "x"))
  )
  expect_false(has_add(mb, "m"))
})

test_that("finite redirect delta: connect, redirect, disconnect, no-op", {
  board <- new_board(
    c(a = new_dataset_block("iris"), b = new_dataset_block("mtcars"),
      m = new_merge_block()),
    links = links(l1 = new_link("a", "m", "x"))
  )

  # Unwired port y -> b: add a fresh link.
  add <- edit_inputs_redirect_delta(board, "m", "y", "b", NULL)
  expect_named(add$links, "add")
  df <- as.data.frame(add$links$add)
  expect_identical(c(df$from, df$to, df$input), c("b", "m", "y"))

  # Wired port x -> b: redirect the existing link (mod its `from`, keep id).
  expect_identical(
    edit_inputs_redirect_delta(board, "m", "x", "b", NULL),
    list(links = list(mod = list(l1 = list(from = "b"))))
  )

  # Same source: no-op.
  expect_length(edit_inputs_redirect_delta(board, "m", "x", "a", NULL), 0L)

  # Blank on a wired port: disconnect (remove the link).
  expect_identical(
    edit_inputs_redirect_delta(board, "m", "x", "", NULL),
    list(links = list(rm = "l1"))
  )

  # Blank on an unwired port: no-op.
  expect_length(edit_inputs_redirect_delta(board, "m", "y", "", NULL), 0L)
})

test_that("finite sources exclude the block and its descendants", {
  board <- new_board(
    c(a = new_dataset_block("iris"), m = new_merge_block(),
      h = new_head_block()),
    links = links(l1 = new_link("a", "m", "x"), l2 = new_link("m", "h", "data"))
  )
  # h is downstream of m, so feeding m from h would cycle -> h excluded.
  expect_false("h" %in% edit_inputs_eligible_sources(board, "m"))
  expect_false("m" %in% edit_inputs_eligible_sources(board, "m"))
  expect_true("a" %in% edit_inputs_eligible_sources(board, "m"))
})

test_that("edit inputs action: a redirect that would cycle is rejected", {
  local_mocked_sidebar()
  r_board <- reactiveValues(
    board = new_board(
      c(a = new_dataset_block("iris"), m = new_merge_block(),
        h = new_head_block()),
      links = links(
        l1 = new_link("a", "m", "x"), l2 = new_link("m", "h", "data")
      )
    ),
    board_id = "b"
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        edit_inputs_action(
          trigger = reactive("m"), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()
      # Point m's free port y at h -> h is downstream of m -> cycle -> rejected.
      session$setInputs(
        `menu-commit` = list(
          action = "redirect", port = "y", source = "h", nonce = 1L
        )
      )
      expect_length(r_update(), 0L)
    }
  )
})

test_that("add delta appends a positional variadic input", {
  board <- new_board(
    c(a = new_dataset_block("iris"), m = new_merge_block(),
      h = new_head_block(), r = new_rbind_block()),
    links = links(l1 = new_link("a", "r", ""), l2 = new_link("r", "h", "data"))
  )

  # Adding `m` as a source appends a fresh unnamed (positional) link r <- m.
  add <- edit_inputs_add_delta(board, "r", "m", NULL)
  df <- as.data.frame(add$links$add)
  expect_identical(c(df$from, df$to, df$input), c("m", "r", ""))

  # The empty choice is a no-op.
  expect_length(edit_inputs_add_delta(board, "r", "", NULL), 0L)

  # A source the block already reaches (h is downstream of r) would cycle and
  # is excluded from the picker in the first place.
  expect_false("h" %in% edit_inputs_eligible_sources(board, "r"))
})

test_that("edit inputs action: an add command commits a new link", {
  local_mocked_sidebar()
  r_board <- inputs_board(variadic_links())
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        edit_inputs_action(
          trigger = reactive("r"), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()
      # The "Add input" picker emits an add command carrying the chosen source.
      session$setInputs(
        `menu-commit` = list(action = "add", source = "m", nonce = 1L)
      )
      upd <- r_update()
      expect_named(upd$links, "add")
      df <- as.data.frame(upd$links$add)
      expect_identical(df$from, "m")
      expect_identical(df$to, "r")
      expect_identical(df$input, "")
    }
  )
})

test_that("edit inputs menu ui is empty for an unknown block", {
  board <- new_board(c(a = new_dataset_block("iris")))
  doc <- xml2::read_html(
    as.character(edit_inputs_menu_ui("mid", board, "gone"))
  )
  notice <- xml2::xml_find_first(
    doc,
    paste0(
      "//*[contains(concat(' ', normalize-space(@class), ' '),",
      " ' blockr-block-browser-empty ')]"
    )
  )
  expect_match(xml2::xml_text(notice), "no longer on the board")
})
