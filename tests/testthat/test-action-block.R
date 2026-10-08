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

test_that("block actions open their menu from their own module", {
  # The pick comes back as the module's own `browser-commit`, so the menu
  # has to be opened with the action's session: its namespace names the
  # input the pick is sent to.
  opened <- list()
  local_mocked_bindings(
    open_add_block_menu = function(mode, caption, at = NULL,
                                   session = get_session()) {
      opened[[length(opened) + 1L]] <<- list(
        ns = session$ns(NULL), mode = mode, caption = caption
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
    lapply(opened, `[[`, "ns"),
    list("add_block_action", "append_block_action", "prepend_block_action")
  )
  expect_identical(
    chr_ply(opened, `[[`, "mode"),
    c("add", "append", "prepend")
  )
  expect_identical(opened[[1L]]$caption, "Add a block")
  expect_match(opened[[2L]]$caption, "^Append to ")
  expect_match(opened[[3L]]$caption, "^Prepend to ")
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

test_that("the block actions' menu opens where the gesture happened", {

  opened <- list()
  local_mocked_bindings(
    open_add_block_menu = function(mode, caption, at = NULL,
                                   session = get_session()) {
      opened[[length(opened) + 1L]] <<- list(at = at)
      invisible(NULL)
    }
  )

  r_board <- reactiveValues(
    board = new_board(c(a = new_dataset_block())),
    board_id = "my_board"
  )

  trigger <- new_trigger()

  testServer(
    function(id, ...) {
      moduleServer(
        action_id(append_block_action),
        append_block_action(
          trigger = trigger,
          board = r_board,
          update = reactiveVal(list())
        )
      )
    },
    {
      trigger("a", at = list(id = "my_board-block_a-edit_block-block_menu"))
      session$flushReact()

      # Fired from code, with nowhere named.
      trigger("a")
      session$flushReact()
    }
  )

  expect_identical(
    opened,
    list(
      list(at = list(id = "my_board-block_a-edit_block-block_menu")),
      list(at = NULL)
    )
  )
})

test_that("a pick from the block actions' menu adds the block", {
  local_mocked_bindings(
    open_add_block_menu = function(...) invisible(NULL)
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
      # What add-block-menu.js sends: the type, no ids.
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

test_that("a pick from the append menu adds the block and its link", {
  local_mocked_bindings(
    open_add_block_menu = function(...) invisible(NULL)
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

test_that("the block actions' menu lists types, append only receivers", {

  items <- add_block_menu_items("add")
  rows <- Filter(function(x) !is.null(x$type), items)
  titles <- Filter(function(x) !is.null(x$title), items)

  expect_true(length(titles) > 0L)
  expect_true("dataset_block" %in% chr_ply(rows, `[[`, "type"))
  ds <- Filter(function(x) identical(x$type, "dataset_block"), rows)[[1L]]
  expect_identical(ds$badge, "blockr.core")
  # The row's mark takes its colour from the category, in blockr.ui.
  expect_identical(ds$mark$category, "input")
  expect_null(ds$mark$color)

  # A source-only block cannot receive a link, so append does not offer it.
  app <- Filter(function(x) !is.null(x$type), add_block_menu_items("append"))
  expect_false("dataset_block" %in% chr_ply(app, `[[`, "type"))
  expect_true("head_block" %in% chr_ply(app, `[[`, "type"))
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

test_that("the block actions' menu takes in a block registered anew", {

  uid <- "menu_probe_block"
  withr::defer(unregister_blocks(uid))

  labels <- function() {
    rows <- Filter(
      function(x) identical(x$type, uid),
      add_block_menu_items("add")
    )
    chr_xtr(rows, "label")
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

# The block browser in the sidebar, which the menu's tool opens. Its form is
# the only way to set what the menu leaves to the server: the IDs, above all,
# cannot be changed after adding.

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

test_that("the block browser is the commit input, and opens on the query", {

  html <- as.character(block_browser_ui("act-browser", query = "head"))
  doc <- xml2::read_html(html)
  root <- xml2::xml_find_first(doc, by_class("blockr-block-browser"))

  expect_identical(xml2::xml_attr(root, "id"), "act-browser-commit")
  expect_identical(
    xml2::xml_attr(
      xml2::xml_find_first(doc, by_class("blockr-block-browser-search")),
      "value"
    ),
    "head"
  )

  chevron <- xml2::xml_find_first(
    doc, by_class("blockr-block-browser-card-chevron")
  )
  expect_identical(
    xml2::xml_attr(chevron, "data-blockr-tooltip"), "Configure before adding"
  )
})

test_that("the block actions' menu sends its tool to the action", {

  sent <- NULL
  session <- list(
    ns = NS("my_board-append_block_action"),
    sendCustomMessage = function(type, message) sent <<- message
  )

  open_add_block_menu("append", "Append to Iris", session = session)

  expect_identical(sent$commit, "my_board-append_block_action-browser-commit")
  expect_identical(sent$expand, "my_board-append_block_action-expand")
})

# Runs `gen` with the sidebar's state stubbed: whether the action holds the
# open sidebar and whether it is pinned. Records what is shown, hidden and
# opened, in order.
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
    open_add_block_menu = function(mode, caption, ...) {
      log("menu", mode = mode, caption = caption)
    }
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

test_that("the menu's tool opens the block browser on what was typed", {

  r_board <- reactiveValues(
    board = new_board(c(a = new_dataset_block("iris"))),
    board_id = "my_board"
  )

  res <- sidebar_run(
    append_block_action, reactive("a"), r_board,
    expr = quote(session$setInputs(expand = list(query = "hea", nonce = 1)))
  )

  whats <- chr_xtr(res$calls, 1L)
  expect_identical(whats, c("menu", "show"))

  show <- res$calls[[2L]]
  expect_identical(show$id, "my_board-actions_sidebar")
  expect_identical(show$title, res$calls[[1L]]$caption)
  expect_match(
    show$html, 'id="append_block_action-browser-commit"', fixed = TRUE
  )
  expect_match(show$html, 'value="hea"', fixed = TRUE)
})

test_that("a gesture shows the browser afresh where the action holds it", {

  r_board <- reactiveValues(
    board = new_board(c(a = new_dataset_block("iris"))),
    board_id = "my_board"
  )

  res <- sidebar_run(
    append_block_action, reactive("a"), r_board, holds = TRUE,
    expr = quote(NULL)
  )

  # No menu: the browser follows the gesture, as a pinned one follows the
  # DAG's selection.
  expect_identical(chr_xtr(res$calls, 1L), "show")
  expect_match(res$calls[[1L]]$title, "^Append to ")
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
  expect_identical(
    after_pick(append_block_action, "a", holds = TRUE, pinned = TRUE),
    character()
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
      expect_length(rec$calls, 1L)
      board$board <- new_board(
        c(a = new_dataset_block("iris"), m = new_merge_block(),
          h1 = new_head_block()),
        links = c(l1 = new_link("h1", "m", "x"))
      )
      session$flushReact()
    })
  )
  expect_identical(chr_xtr(res$calls, 1L), c("show", "show"))
  expect_false(grepl("target-input", res$calls[[2L]]$html, fixed = TRUE))
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
