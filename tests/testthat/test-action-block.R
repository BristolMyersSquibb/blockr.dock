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

test_that("block actions show the block browser from their own module", {
  # A pick comes back as the module's own `browser-commit`, so the browser
  # has to be drawn under the action's namespace and shown from its session.
  shown <- list()
  local_mocked_bindings(
    show_sidebar = function(id, ui = NULL, title = NULL,
                            session = get_session()) {
      shown[[length(shown) + 1L]] <<- list(
        ns = session$ns(NULL), id = id, title = title,
        html = as.character(ui)
      )
      invisible(NULL)
    }
  )

  r_board <- reactiveValues(
    board = new_board(c(a = new_dataset_block("iris"), m = new_merge_block())),
    board_id = "b"
  )

  fire_action(add_block_action, TRUE, r_board)
  fire_action(append_block_action, "a", r_board)
  fire_action(prepend_block_action, "m", r_board)

  expect_identical(
    lapply(shown, `[[`, "ns"),
    list("add_block_action", "append_block_action", "prepend_block_action")
  )
  expect_identical(unique(chr_xtr(shown, "id")), "b-actions_sidebar")
  expect_identical(shown[[1L]]$title, "Add a block")
  expect_match(shown[[2L]]$title, "^Append to ")
  expect_match(shown[[3L]]$title, "^Prepend to ")
  expect_match(
    shown[[2L]]$html, 'id="append_block_action-browser-commit"', fixed = TRUE
  )
})

test_that("append block action: a name field names the variadic slot", {
  r_board <- reactiveValues(
    board = new_board(blocks = c(a = new_dataset_block())),
    board_id = "b"
  )
  r_update <- reactiveVal(list())
  local_mocked_bindings(
    show_sidebar         = function(...) invisible(NULL),
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
      # Appending a variadic rbind: the name field arrives as block_input
      # and names the new block's fresh slot.
      session$setInputs(`browser-commit` = commit_spec(
        type = "rbind_block", id = "r1", title = NULL,
        link_id = "lnk1", block_input = "controls", nonce = 1
      ))

      df <- as.data.frame(r_update()$links$add)
      expect_identical(df$to, "r1")
      expect_identical(df$input, "controls")
    }
  )
})

test_that("prepend block action: a name field names the target slot", {
  r_board <- reactiveValues(
    board = new_board(blocks = c(r = new_rbind_block())),
    board_id = "b"
  )
  r_update <- reactiveVal(list())
  local_mocked_bindings(
    show_sidebar         = function(...) invisible(NULL),
    hide_sidebar         = function(...) invisible(NULL)
  )

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        prepend_block_action(
          trigger = reactive("r"), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()
      # Prepending a source into a variadic target: target_input carries
      # the typed name for the target's fresh slot.
      session$setInputs(`browser-commit` = commit_spec(
        type = "dataset_block", id = "d1", title = NULL,
        link_id = "lnk1", target_input = "controls", nonce = 1
      ))

      df <- as.data.frame(r_update()$links$add)
      expect_identical(df$to, "r")
      expect_identical(df$input, "controls")
    }
  )
})

test_that("prepend block action: a duplicate target name is rejected", {
  r_board <- reactiveValues(
    board = new_board(
      blocks = c(a = new_dataset_block(), r = new_rbind_block()),
      links = links(id = "ar", from = "a", to = "r", input = "controls")
    ),
    board_id = "b"
  )
  r_update <- reactiveVal(list())
  local_mocked_bindings(
    show_sidebar         = function(...) invisible(NULL),
    hide_sidebar         = function(...) invisible(NULL)
  )

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        prepend_block_action(
          trigger = reactive("r"), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()
      session$setInputs(`browser-commit` = commit_spec(
        type = "dataset_block", id = "d1", title = NULL,
        link_id = "lnk1", target_input = "controls", nonce = 1
      ))

      expect_identical(r_update(), list())
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

test_that("a card added without its form adds the block", {
  local_mocked_bindings(
    show_sidebar = function(...) invisible(NULL)
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
      # A card's form left empty: the type, no ids.
      session$setInputs(`browser-commit` = commit_spec(
        type = "dataset_block", nonce = 1
      ))

      upd <- r_update()
      expect_named(upd, "blocks")
      expect_length(upd$blocks$add, 1L)
      expect_s3_class(upd$blocks$add[[1L]], "dataset_block")
    }
  )
})

test_that("an append card added without its form adds the block and link", {
  local_mocked_bindings(
    show_sidebar = function(...) invisible(NULL)
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
        type = "head_block", nonce = 1
      ))

      upd <- r_update()
      expect_named(upd, c("blocks", "links"))
      lnk <- as.data.frame(upd$links$add)
      expect_identical(lnk$from, "a")
      expect_identical(lnk$to, names(upd$blocks$add))
      expect_identical(lnk$input, "data")
    }
  )
})

test_that("the block browser lists every type, append only receivers", {

  metas <- block_metas("add")
  ds <- Filter(function(x) identical(x$type, "dataset_block"), metas)

  expect_length(ds, 1L)
  expect_identical(ds[[1L]]$package, "blockr.core")
  expect_identical(ds[[1L]]$category, "input")

  # A source-only block cannot receive a link, so append does not offer it.
  app <- chr_xtr(block_metas("append"), "type")
  expect_false("dataset_block" %in% app)
  expect_true("head_block" %in% app)
})

test_that("the add-panel menu marks a block by its category", {

  board <- new_dock_board(
    c(a = new_dataset_block("iris"), b = new_head_block()),
    extensions = new_edit_board_extension()
  )

  items <- add_panel_menu_items(board, c("a", "b"), dock_ext_ids(board))
  marks <- lst_xtr(Filter(function(x) !is.null(x$mark), items), "mark")

  # An extension has no category, so its mark takes the one a block without
  # one falls back to.
  expect_identical(
    chr_xtr(marks, "category"),
    c("input", "transform", "uncategorized")
  )
  expect_null(unlst(lst_xtr(marks, "color")))
})

test_that("the block browser takes in a block registered anew", {

  uid <- "menu_probe_block"
  withr::defer(unregister_blocks(uid))

  labels <- function() {
    metas <- Filter(function(x) identical(x$type, uid), block_metas("add"))
    chr_xtr(metas, "name")
  }

  register_block(new_dataset_block, "Probe", "A probe block", uid = uid)
  expect_identical(labels(), "Probe")

  # Same uid, so the registry keeps its size.
  register_block(
    new_dataset_block, "Renamed probe", "A probe block", uid = uid,
    overwrite = TRUE
  )
  expect_identical(labels(), "Renamed probe")
})

# The block browser in the sidebar. Its form is the only way to set the IDs
# of a block and its links, which cannot be changed after adding.

by_class <- function(tok) {
  sprintf(
    "//*[contains(concat(' ', normalize-space(@class), ' '), ' %s ')]", tok
  )
}

browser_card <- function(html, block_type) {
  xml2::xml_find_first(
    xml2::read_html(html),
    paste0(
      by_class("blockr-block-browser-card"), "[@data-block-type='",
      block_type, "']"
    )
  )
}

# The fields a card's form offers, by the class suffix each reports through.
card_fields <- function(html, block_type) {
  fields <- xml2::xml_find_all(
    browser_card(html, block_type),
    paste0(".", by_class("blockr-block-browser-field"))
  )
  sub(
    ".*blockr-block-browser-field-", "", xml2::xml_attr(fields, "class")
  )
}

test_that("the block browser offers the fields of its flow", {

  board <- new_board(
    c(a = new_dataset_block("iris"), m = new_merge_block()),
    links = c(l1 = new_link("a", "m", "x"))
  )
  html <- function(target) {
    as.character(block_browser_ui("b", board, target))
  }

  add <- html(NULL)
  expect_identical(card_fields(add, "dataset_block"), c("id", "title"))
  expect_match(add, ">Add block</button>", fixed = TRUE)

  # A merge takes two inputs, so appending one asks which the link lands on.
  app <- html(append_to("a"))
  expect_identical(
    card_fields(app, "merge_block"),
    c("id", "title", "link-id", "block-input")
  )
  expect_identical(card_fields(app, "head_block"), c("id", "title", "link-id"))
  expect_match(app, ">Append block</button>", fixed = TRUE)

  # The merge has one free input left, `y`, so there is nothing to ask.
  pre <- html(prepend_to("m"))
  expect_identical(
    card_fields(pre, "dataset_block"), c("id", "title", "link-id")
  )
  expect_match(pre, ">Prepend block</button>", fixed = TRUE)

  # An insert makes two links, so it asks for an ID for each.
  ins <- html(insert_into("l1"))
  expect_identical(
    card_fields(ins, "head_block"),
    c("id", "title", "near-link-id", "far-link-id")
  )
  expect_match(ins, ">Insert block</button>", fixed = TRUE)
})

test_that("a prepend asks which free input of its target the link lands on", {

  board <- new_board(c(a = new_dataset_block("iris"), m = new_merge_block()))

  html <- as.character(block_browser_ui("b", board, prepend_to("m")))
  opts <- xml2::xml_text(
    xml2::xml_find_all(
      browser_card(html, "dataset_block"),
      paste0(
        ".", by_class("blockr-block-browser-field-target-input"), "//option"
      )
    )
  )

  expect_identical(opts, c("x", "y"))
})

test_that("block browser renders a name field for variadic ends", {

  board <- new_board(
    c(a = new_dataset_block("iris"), r = new_rbind_block(),
      m = new_merge_block())
  )
  field_input <- function(html, block_type, cls) {
    xml2::xml_find_first(
      browser_card(html, block_type), paste0(".", by_class(cls), "//input")
    )
  }

  # Append a variadic rbind -> block-input becomes a free-text name field;
  # a finite merge keeps its port <select> (no text input).
  append_html <- as.character(block_browser_ui("b", board, append_to("a")))
  rbind_name <- field_input(
    append_html, "rbind_block", "blockr-block-browser-field-block-input"
  )
  expect_false(is.na(rbind_name))
  expect_identical(
    xml2::xml_attr(rbind_name, "placeholder"),
    "leave blank for an unnamed input"
  )
  expect_true(
    is.na(
      field_input(
        append_html, "merge_block", "blockr-block-browser-field-block-input"
      )
    )
  )

  # Prepend into a variadic target -> target-input becomes a name field.
  prepend_html <- as.character(block_browser_ui("b", board, prepend_to("r")))
  expect_false(
    is.na(
      field_input(
        prepend_html, "dataset_block",
        "blockr-block-browser-field-target-input"
      )
    )
  )
})

test_that("the block browser is the commit input", {

  doc <- xml2::read_html(as.character(block_browser_ui("act-browser")))
  root <- xml2::xml_find_first(doc, by_class("blockr-block-browser"))

  expect_identical(xml2::xml_attr(root, "id"), "act-browser-commit")

  chevron <- xml2::xml_find_first(
    doc, by_class("blockr-block-browser-card-chevron")
  )
  expect_identical(
    xml2::xml_attr(chevron, "data-blockr-tooltip"), "Configure before adding"
  )
})

# Runs `gen` with the sidebar's state stubbed: whether the action holds the
# open sidebar and whether it is pinned. Records what is shown and hidden, in
# order.
sidebar_run <- function(gen, trigger, board, holds = FALSE, pinned = FALSE,
                        expr) {

  rec <- new.env(parent = emptyenv())
  rec$calls <- list()
  log <- function(what, ...) {
    rec$calls[[length(rec$calls) + 1L]] <- list(what, ...)
  }

  local_mocked_bindings(
    owns_open_sidebar = function(...) holds,
    sidebar_state = function(...) {
      list(open = holds, pinned = pinned, owner = NULL)
    },
    show_sidebar = function(id, ui = NULL, title = NULL, ...) {
      log("show", id = id, title = title, html = as.character(ui))
    },
    hide_sidebar = function(id, ...) log("hide", id = id),
    reset_block_browser = function(...) log("reset")
  )

  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        action_id(gen),
        gen(trigger = trigger, board = board, update = r_update)
      )
    },
    {
      session$flushReact()
      eval(expr)
    }
  )

  list(calls = rec$calls, update = isolate(r_update()))
}

test_that("a gesture shows the block browser, held or not", {

  r_board <- reactiveValues(
    board = new_board(c(a = new_dataset_block("iris"))),
    board_id = "my_board"
  )

  # Held, the browser is shown afresh for the gesture's target, as a pinned
  # one is when the DAG fires the action again on a new selection.
  for (holds in c(FALSE, TRUE)) {

    res <- sidebar_run(
      append_block_action, reactive("a"), r_board, holds = holds,
      expr = quote(NULL)
    )

    expect_identical(chr_xtr(res$calls, 1L), "show")

    show <- res$calls[[1L]]
    expect_identical(show$id, "my_board-actions_sidebar")
    expect_match(show$title, "^Append to ")
    expect_match(
      show$html, 'id="append_block_action-browser-commit"', fixed = TRUE
    )
  }
})

test_that("a pick closes the browser, unless it is pinned", {

  r_board <- reactiveValues(
    board = new_board(c(a = new_dataset_block("iris"), m = new_merge_block())),
    board_id = "my_board"
  )
  pick <- quote(
    session$setInputs(
      `browser-commit` = list(type = "head_block", id = "h1", nonce = 1)
    )
  )
  after_pick <- function(gen, target, ...) {
    res <- sidebar_run(gen, reactive(target), r_board, expr = pick, ...)
    expect_named(res$update$blocks$add, "h1")
    # The first call is the gesture's.
    chr_xtr(res$calls[-1L], 1L)
  }

  expect_identical(
    after_pick(append_block_action, "a", holds = TRUE), "hide"
  )
  # A pinned one stays, and the form the pick came from is cleared.
  expect_identical(
    after_pick(append_block_action, "a", holds = TRUE, pinned = TRUE),
    "reset"
  )
  # A prepend takes up one of its target's inputs, so a pinned browser is
  # shown afresh, without it, once the board has taken the link in.
  res <- sidebar_run(
    prepend_block_action, reactive("m"),
    reactiveValues(board = isolate(r_board$board), board_id = "my_board"),
    holds = TRUE, pinned = TRUE,
    expr = quote({
      session$setInputs(
        `browser-commit` = list(type = "head_block", id = "h1", nonce = 1)
      )
      expect_length(rec$calls, 2L)
      board$board <- new_board(
        c(a = new_dataset_block("iris"), m = new_merge_block(),
          h1 = new_head_block()),
        links = c(l1 = new_link("h1", "m", "x"))
      )
      session$flushReact()
    })
  )
  expect_identical(chr_xtr(res$calls, 1L), c("show", "reset", "show"))
  expect_false(grepl("target-input", res$calls[[3L]]$html, fixed = TRUE))
  # A sidebar another action holds is left alone.
  expect_identical(after_pick(append_block_action, "a"), character())
})

test_that("a target that leaves the board closes the browser", {

  r_board <- reactiveValues(
    board = new_board(c(a = new_dataset_block("iris"), b = new_head_block())),
    board_id = "my_board"
  )

  res <- sidebar_run(
    append_block_action, reactive("a"), r_board, holds = TRUE,
    expr = quote({
      board$board <- new_board(c(b = new_head_block()))
      session$flushReact()
    })
  )

  expect_identical(chr_xtr(res$calls, 1L), c("show", "hide"))
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
