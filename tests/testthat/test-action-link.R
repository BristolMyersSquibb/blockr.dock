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

test_that("a link menu card leads with the block's mark", {
  board <- new_board(c(a = new_dataset_block("iris"), m = new_merge_block()))

  doc <- xml2::read_html(as.character(link_menu_ui("mid", board, "a")))

  mark <- xml2::xml_find_all(
    doc,
    paste0(
      "//*[", has_class("blockr-link-menu-card"), "][@data-block-type='m']",
      "/*[", has_class("blockr-block-browser-card-header"), "]",
      "/span[", has_class("blockr-block-mark"), "]"
    )
  )

  # A list row's mark is blockr.ui's at its plain 24px.
  expect_length(mark, 1L)
  expect_identical(xml2::xml_attr(mark, "class"), "blockr-block-mark")
  expect_identical(
    xml2::xml_attr(mark, "data-category"),
    block_metadata(new_merge_block())$category
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

# A blank slot's identity is its position in the board's link list, so the
# board has to be read in order, not as a set.
incoming <- function(board, to) {
  df <- as.data.frame(board_links(board))
  df <- df[df$to == to, ]
  paste0(df$from, ifelse(nzchar(df$input), paste0("(", df$input, ")"), ""))
}

# What the board looks like once core has applied the delta the action emitted.
applied <- function(board, upd) {
  blocks <- board_blocks(board)
  board_blocks(board) <- c(blocks, upd$blocks$add)
  blockr.core:::modify_board_links(
    board, add = upd$links$add, rm = upd$links$rm, before = upd$links$before
  )
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
  expect_named(upd$links, c("add", "rm", "before"))
  expect_identical(upd$links$rm, "l1")
  expect_setequal(wiring(upd$links$add), c("a>c>data", "c>b>data"))
})

variadic_board <- function(first = "first", second = "second") {
  reactiveValues(
    board = new_board(
      c(a = new_dataset_block("iris"), z = new_dataset_block("mtcars"),
        m = new_rbind_block()),
      links = c(
        l1 = new_link("a", "m", first), l2 = new_link("z", "m", second)
      )
    ),
    board_id = "my_board"
  )
}

test_that("insert block action: a named far end keeps the slot it had", {
  local_mocked_sidebar()

  # Where the entries are named, the name is the identity, so the only thing
  # that has to survive is the slot.
  r_board <- variadic_board()

  upd <- run_insert(
    r_board, reactiveVal(list()),
    list(type = "head_block", id = "c", nonce = 1L)
  )

  expect_identical(upd$links$rm, "l1")
  expect_setequal(wiring(upd$links$add), c("a>c>data", "c>m>first"))

  # And the sibling is untouched, read off the board in order rather than as
  # a set, so a re-order could fail this.
  expect_identical(
    incoming(applied(isolate(r_board$board), upd), "m"),
    c("c(first)", "z(second)")
  )
})

test_that("insert block action: a blank far end keeps its position", {
  local_mocked_sidebar()

  # `resolve_free_input()` hands every variadic target a blank slot, so this
  # is the common shape, not an edge case. A blank slot carries no name: its
  # identity is where it sits in the link list, which `sync_dot_args()` walks
  # in order to hand out positional arguments. Drop the split link and append
  # its replacement and the sibling slides up one, swapping an rbind's rows.
  r_board <- variadic_board("", "")

  before <- incoming(isolate(r_board$board), "m")
  expect_identical(before, c("a", "z"))

  upd <- run_insert(
    r_board, reactiveVal(list()),
    list(type = "head_block", id = "c", nonce = 1L)
  )

  expect_identical(
    incoming(applied(isolate(r_board$board), upd), "m"),
    c("c", "z")
  )
})

test_that("insert block action: splitting a middle link holds every position", {
  local_mocked_sidebar()

  # Two links cannot tell a correct placement from a plain swap, so this
  # splits the middle of three. Position follows the board's link order for
  # named and blank entries alike, since `sync_dot_args()` re-adds every key
  # in that order, so both are checked.
  three <- function(i1, i2, i3) {
    reactiveValues(
      board = new_board(
        c(a = new_dataset_block("iris"), z = new_dataset_block("iris"),
          q = new_dataset_block("iris"), m = new_rbind_block()),
        links = c(
          l1 = new_link("a", "m", i1), l2 = new_link("z", "m", i2),
          l3 = new_link("q", "m", i3)
        )
      ),
      board_id = "my_board"
    )
  }

  named <- three("one", "two", "three")
  upd <- run_insert(
    named, reactiveVal(list()),
    list(type = "head_block", id = "c", nonce = 1L), link = "l2"
  )

  expect_identical(
    incoming(applied(isolate(named$board), upd), "m"),
    c("a(one)", "c(two)", "q(three)")
  )

  blank <- three("", "", "")
  upd <- run_insert(
    blank, reactiveVal(list()),
    list(type = "head_block", id = "c", nonce = 1L), link = "l2"
  )

  expect_identical(
    incoming(applied(isolate(blank$board), upd), "m"),
    c("a", "c", "q")
  )
})

test_that("insert block action: splitting the last link needs no placement", {
  local_mocked_sidebar()

  # Placement is inert at the end, but the payload carries it anyway rather
  # than branching on where the split link happened to sit.
  r_board <- variadic_board("", "")
  upd <- run_insert(
    r_board, reactiveVal(list()),
    list(type = "head_block", id = "c", nonce = 1L), link = "l2"
  )

  expect_identical(
    incoming(applied(isolate(r_board$board), upd), "m"),
    c("a", "c")
  )
})

test_that("insert block action: an explicit slot on the new block wins", {
  local_mocked_sidebar()

  upd <- run_insert(
    insert_board(), reactiveVal(list()),
    list(type = "merge_block", id = "c", block_input = "y", nonce = 1L)
  )

  expect_setequal(wiring(upd$links$add), c("a>c>y", "c>b>data"))
})

test_that("insert block action: both link ids are fresh, the far one placed", {
  local_mocked_sidebar()

  upd <- run_insert(
    insert_board(), reactiveVal(list()),
    list(type = "head_block", id = "c", nonce = 1L)
  )

  ids <- names(upd$links$add)
  far <- as.data.frame(upd$links$add)

  expect_length(unique(ids), 2L)
  expect_false("l1" %in% ids)

  # The far end no longer borrows the split link's id to hold its position:
  # `before` names where it goes, so both links are simply new.
  expect_identical(
    upd$links$before,
    set_names("l1", far$id[far$from == "c"])
  )
})

test_that("insert block action: link ids can be given", {
  local_mocked_sidebar()

  upd <- run_insert(
    insert_board(), reactiveVal(list()),
    list(type = "head_block", id = "c", near_link_id = "in",
         far_link_id = "out", nonce = 1L)
  )

  expect_setequal(names(upd$links$add), c("in", "out"))
  expect_identical(upd$links$before, c(out = "l1"))
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
