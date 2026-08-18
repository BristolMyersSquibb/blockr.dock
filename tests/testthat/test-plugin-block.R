test_that("edit block server", {

  testServer(
    edit_block_server(),
    {
      expect_null(update())

      session$setInputs(
        block_name_in = "Test block name"
      )

      upd <- update()

      expect_length(upd, 1L)
      expect_type(upd, "list")
      expect_named(upd, "blocks")

      expect_length(upd$blocks, 1L)
      expect_named(upd$blocks, "mod")

      expect_length(upd$blocks$mod, 1L)
      expect_named(upd$blocks$mod, "a")
      expect_type(upd$blocks$mod, "list")

      expect_identical(
        upd$blocks$mod$a,
        list(block_name = "Test block name")
      )
    },
    args = list(
      block_id = "a",
      board = board_args(blocks = c(a = new_dataset_block())),
      update = reactiveVal()
    )
  )

  expect_error(
    testServer(
      edit_block_server(
        list(function(...) TRUE)
      ),
      NULL,
      args = list(
        block_id = "a",
        board = board_args(blocks = c(a = new_dataset_block())),
        update = reactiveVal()
      )
    ),
    class = "invalid_edit_block_server_callback_result"
  )
})

test_that("renaming a block does not loop (#181)", {

  pushed <- character()

  local_mocked_bindings(
    updateTextInput = function(session, input_id, label, value) {
      pushed <<- c(pushed, value)
    }
  )

  board_named <- function(nm) {
    blk <- new_dataset_block()
    block_name(blk) <- nm
    new_dock_board(blocks = c(a = blk))
  }

  board <- reactiveValues(board = board_named("Dataset"))

  testServer(
    edit_block_server(),
    {
      session$flushReact()

      expect_identical(pushed, "Dataset")

      n_before <- length(pushed)

      session$setInputs(block_name_in = "Renamed block")
      session$flushReact()

      expect_identical(
        update()$blocks$mod$a,
        list(block_name = "Renamed block")
      )

      # The input sync reads the applied board, not the pending request.
      # Core applies updates last (priority = -Inf), so board$board still
      # holds the old name here -> the sync does not fire and no stale name
      # is pushed back to loop (#181).
      expect_length(pushed, n_before)

      board$board <- board_named("Renamed block")
      session$flushReact()

      # Applied name now matches the input -> still no spurious push.
      expect_length(pushed, n_before)

      board$board <- board_named("External")
      session$flushReact()

      # A rename originating elsewhere still flows into the text input.
      expect_identical(pushed, c("Dataset", "External"))
    },
    args = list(
      block_id = "a",
      board = board,
      update = reactiveVal()
    )
  )
})

test_that("block_cond_buckets keys messages by condition id (#36)", {

  df <- data.frame(
    block = "a",
    phase = c("data", "eval", "eval", "status", "eval"),
    severity = c("warning", "warning", "message", "message", "error"),
    message = c("w one", "w two", "m one", "status note", "boom"),
    id = c("id_w1", "id_w2", "id_m1", "id_status", "id_e1"),
    stringsAsFactors = FALSE
  )

  buckets <- block_cond_buckets(df)

  expect_named(buckets, c("error", "warning", "message"))

  expect_false("status note" %in% buckets$message)

  expect_identical(buckets$warning, c(id_w1 = "w one", id_w2 = "w two"))
  expect_identical(buckets$message, c(id_m1 = "m one"))
  expect_identical(buckets$error, c(id_e1 = "boom"))
})

test_that("block_cond_buckets de-duplicates repeated conditions (#36)", {

  df <- data.frame(
    block = "a",
    phase = c("data", "eval"),
    severity = "warning",
    message = "same warning",
    id = "dup_id",
    stringsAsFactors = FALSE
  )

  expect_identical(
    block_cond_buckets(df)$warning,
    c(dup_id = "same warning")
  )
})

test_that("condition UI updates surgically by condition id (#36)", {

  inserted <- character()
  removed <- character()

  local_mocked_bindings(
    insert_ui = function(selector, ..., ui) {
      id <- ui$attribs$id
      if (is.null(id)) id <- NA_character_
      inserted[[length(inserted) + 1L]] <<- id
      expect_s3_class(ui, "shiny.tag")
    },
    remove_ui = function(selector, ...) {
      removed[[length(removed) + 1L]] <<- selector
    }
  )

  with_mock_session(
    {
      cond <- reactiveVal()

      update_blk_cond_observer(cond)

      session$flushReact()

      expect_length(inserted, 0L)
      expect_length(removed, 0L)

      cond(
        list(
          error = character(),
          warning = c(a1 = "first warning", b2 = "second warning"),
          message = c(c3 = "a message")
        )
      )

      session$flushReact()

      expect_length(inserted, 3L)
      expect_length(removed, 0L)
      expect_true(any(grepl("cond_warning_a1", inserted, fixed = TRUE)))
      expect_true(any(grepl("cond_message_c3", inserted, fixed = TRUE)))

      cond(
        list(
          error = c(d4 = "boom"),
          warning = c(a1 = "first warning"),
          message = c(c3 = "a message")
        )
      )

      session$flushReact()

      expect_length(inserted, 4L)
      expect_length(removed, 1L)
      expect_true(any(grepl("cond_error_d4", inserted, fixed = TRUE)))
      expect_match(removed[[1L]], "cond_warning_b2")
    }
  )
})

test_that("locked dock keeps block_card_toggles hidden (#122)", {

  blk <- new_dataset_block()
  attr(blk, "visible") <- "outputs"

  # Unlocked: widget renders visible, with `selected` matching saved attr
  unlocked <- withr::with_options(
    list(blockr.locked = NULL),
    block_card_toggles(blk, NS("x"))
  )
  expect_s3_class(unlocked, "shiny.tag.list")
  unlocked_html <- as.character(htmltools::tagList(unlocked))
  expect_match(unlocked_html, 'value="outputs".*checked', fixed = FALSE)
  expect_false(grepl("display: none", unlocked_html, fixed = TRUE))

  # Locked: widget still renders (so input$collapse_blk_sections seeds
  # accordion_panel_set) but is hidden and the tooltip script is dropped
  locked <- withr::with_options(
    list(blockr.locked = TRUE),
    block_card_toggles(blk, NS("x"))
  )
  expect_s3_class(locked, "shiny.tag")
  locked_html <- as.character(locked)
  expect_match(locked_html, "display: none", fixed = TRUE)
  expect_match(locked_html, 'value="outputs".*checked', fixed = FALSE)
  expect_false(grepl("<script", locked_html, fixed = TRUE))
})

test_that("the block plugin reports whether its card offers inputs (#69)", {

  reported <- function(blk) {

    res <- NULL

    testServer(
      edit_block_server(),
      res <<- session$returned,
      args = list(
        block_id = "a",
        board = board_args(blocks = c(a = blk)),
        update = reactiveVal()
      )
    )

    edit_block_validator(res)

    res[["has_inputs"]]
  }

  expect_false(reported(new_rbind_block()))
  expect_true(reported(new_dataset_block()))
})

test_that("an input-free block card drops the inputs section (#69)", {

  card_parts <- function(blk) {

    card <- edit_block_ui(
      "blk",
      blk,
      "a",
      expr_ui = expr_ui("blk", blk),
      block_ui = div(id = "blk-out")
    )

    root <- xml2::read_html(as.character(htmltools::tagList(card)))

    list(
      panels = xml2::xml_attr(
        xml2::xml_find_all(
          root,
          paste0(
            "//div[", has_class("blockr-block-accordion"), "]/div[@data-value]"
          )
        ),
        "data-value"
      ),
      toggles = xml2::xml_attr(
        xml2::xml_find_all(
          root,
          paste0(
            "//div[", has_class("blockr-section-toggle"), "]",
            "//input[@type='checkbox']"
          )
        ),
        "value"
      )
    )
  }

  # An rbind block is configured entirely by its links, so its section would
  # come up empty and its toggle would open nothing.
  rbnd <- card_parts(new_rbind_block())

  expect_identical(rbnd$panels, "outputs")
  expect_identical(rbnd$toggles, "outputs")

  dataset <- card_parts(new_dataset_block())

  expect_identical(dataset$panels, c("inputs", "outputs"))
  expect_identical(dataset$toggles, c("inputs", "outputs"))
})

test_that("a board saved before #69 restores an input-free card", {

  blk <- new_rbind_block()
  attr(blk, "visible") <- c("inputs", "outputs")

  toggles <- xml2::xml_find_all(
    xml2::read_html(
      as.character(
        htmltools::tagList(block_card_toggles(blk, NS("x"), has_inputs = FALSE))
      )
    ),
    "//input[@type='checkbox']"
  )

  expect_identical(xml2::xml_attr(toggles, "value"), "outputs")
  expect_identical(xml2::xml_attr(toggles, "checked"), "checked")
})

test_that("block card sections carry the css-styling contract (#214)", {

  card <- block_card_content(
    NS("blk"),
    expr_ui = div(id = "blk-expr"),
    block_ui = div(id = "blk-out")
  )

  root <- xml2::read_html(as.character(htmltools::tagList(card)))

  # The stylesheet keys the per-panel styling off this class and the stable
  # panel data-value, so both must be present for the cards to render styled.
  acc <- xml2::xml_find_all(
    root,
    paste0("//div[", has_class("blockr-block-accordion"), "]")
  )
  expect_length(acc, 1L)

  values <- xml2::xml_attr(
    xml2::xml_find_all(acc, "./div[@data-value]"),
    "data-value"
  )
  expect_setequal(values, c("inputs", "outputs"))

  # The header-hide and body frame now live in the stylesheet: the panel's own
  # header and body must not inline the styles tagQuery used to bake in per card
  # (a revert to that would silently detach them from the css contract above).
  inputs_item <- "./div[@data-value='inputs']"

  header <- xml2::xml_find_first(
    acc,
    paste0(inputs_item, "/div[", has_class("accordion-header"), "]")
  )
  body <- xml2::xml_find_first(
    acc,
    paste0(inputs_item, "//div[", has_class("accordion-body"), "]")
  )
  expect_true(is.na(xml2::xml_attr(header, "style")))
  expect_true(is.na(xml2::xml_attr(body, "style")))
})

test_that("block card carries no html output (#403)", {

  blk <- new_dataset_block()

  card <- edit_block_ui(
    "blk",
    blk,
    "a",
    expr_ui = div(id = "blk-expr"),
    block_ui = div(id = "blk-out")
  )

  root <- xml2::read_html(as.character(htmltools::tagList(card)))

  # Each html output value costs a rebind of its scope plus two whole-page
  # clientdata walks, and the card's status dot, status note and title were
  # three of them per card. All three are written in place instead.
  expect_length(
    xml2::xml_find_all(
      root,
      paste0("//*[", has_class("shiny-html-output"), "]")
    ),
    0L
  )

  dot <- xml2::xml_find_all(
    root,
    paste0(
      "//span[", has_class("blockr-status-dot"),
      " and ", has_class("blockr-attr-output"), "]"
    )
  )
  expect_length(dot, 1L)
  expect_identical(xml2::xml_attr(dot, "id"), "blk-status_indicator")

  slot <- xml2::xml_find_all(
    root,
    paste0(
      "//div[", has_class("blockr-status-note-slot"),
      " and ", has_class("blockr-attr-output"), "]"
    )
  )
  expect_length(slot, 1L)
  expect_identical(xml2::xml_attr(slot, "id"), "blk-status_note")

  # Both notes ship with the card and the stylesheet reveals the one the slot's
  # data-status names, so a status change writes an attribute rather than
  # sending markup.
  notes <- xml2::xml_find_all(
    slot,
    paste0("./div[", has_class("blockr-status-note"), "]")
  )
  expect_setequal(xml2::xml_attr(notes, "data-status"), c("waiting", "unset"))

  # The title is seeded from the block and kept in sync client-side off the
  # rename input, so it needs no output of its own.
  title <- xml2::xml_find_all(
    root,
    paste0("//span[", has_class("blockr-title"), "]")
  )
  expect_length(title, 1L)
  expect_identical(xml2::xml_text(title), block_name(blk))
})

test_that("card status and title land in the browser (e2e, #403)", {

  skip_on_cran()

  # The fixture renders both cards, with `b` waiting on a data input it has no
  # link for and `a` evaluating.
  app <- new_app_driver(
    system.file("examples", "block-status", "app.R", package = "blockr.dock"),
    name = "card-status",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 30 * 1000
  )
  withr::defer(app$stop())

  wait_dock_loaded(app, 2)

  el <- function(blk, part) {
    sprintf("my_board-block_%s-edit_block-%s", blk, part)
  }

  attr_js <- function(id, name) {
    sprintf("document.getElementById('%s').getAttribute('%s')", id, name)
  }

  read <- function(js) coal(app$get_js(js), "<null>", fail_all = FALSE)

  diagnose <- function() {
    sprintf(
      "[card-status] b dot style=%s note=%s title=%s",
      read(attr_js(el("b", "status_indicator"), "style")),
      read(attr_js(el("b", "status_note"), "data-status")),
      read(
        sprintf(
          "document.querySelector('#%s .blockr-title').textContent",
          el("a", "title_display")
        )
      )
    )
  }

  # The binding writes the shared spec straight onto the dot, so the colour and
  # the tooltip arrive without any markup travelling.
  wait_js(
    app,
    paste0(attr_js(el("b", "status_indicator"), "style"), " !== null"),
    diagnose
  )

  expect_match(
    app$get_js(attr_js(el("b", "status_indicator"), "style")),
    "#f59e0b",
    fixed = TRUE
  )
  expect_identical(
    app$get_js(attr_js(el("b", "status_indicator"), "title")),
    "Waiting for a data input"
  )

  # An evaluated block clears the attributes rather than keeping a stale spec.
  # Its note slot naming `ready` is what says the value landed at all, so the
  # bare dot below is a cleared one and not an unrendered one.
  wait_js(
    app,
    paste0(attr_js(el("a", "status_note"), "data-status"), " === 'ready'"),
    diagnose
  )
  expect_null(app$get_js(attr_js(el("a", "status_indicator"), "style")))
  expect_null(app$get_js(attr_js(el("a", "status_indicator"), "title")))

  # Both notes ship with the card and the stylesheet reveals exactly the one
  # the slot's data-status names.
  expect_identical(
    app$get_js(attr_js(el("b", "status_note"), "data-status")),
    "waiting"
  )
  expect_identical(
    app$get_js(
      sprintf(
        paste0(
          "Array.from(document.querySelectorAll('#%s > .blockr-status-note'))",
          ".filter((n) => getComputedStyle(n).display !== 'none')",
          ".map((n) => n.dataset.status).join(',')"
        ),
        el("b", "status_note")
      )
    ),
    "waiting"
  )

  # A card built only on first visit still ends up with its status. These stay
  # Shiny outputs precisely for that: they suspend while off screen and render
  # when the card is inserted, where a custom message sent before the insert
  # would simply be dropped.
  expect_false(
    app$get_js(
      sprintf(
        "document.getElementById('%s') !== null",
        el("c", "status_indicator")
      )
    )
  )

  app$run_js(
    paste0(
      "document.querySelector('#my_board-view_nav ",
      ".blockr-view-item[data-view-id=\"later\"]').click()"
    )
  )

  wait_js(
    app,
    sprintf(
      paste(
        "document.getElementById('%s') !== null &&",
        "%s === 'Waiting for a data input'"
      ),
      el("c", "status_indicator"),
      attr_js(el("c", "status_indicator"), "title")
    ),
    diagnose
  )

  # Renaming drives the displayed title from the rename input itself, so no
  # output sits between the two.
  title_js <- sprintf(
    "document.querySelector('#%s .blockr-title').textContent",
    el("a", "title_display")
  )
  expect_identical(app$get_js(title_js), "Dataset")

  do.call(
    app$set_inputs,
    set_names(list("Renamed"), el("a", "block_name_in"))
  )

  wait_js(app, paste0(title_js, " === 'Renamed'"), diagnose)
})

test_that("block_cond_buckets drops status-phase rows from warnings (#290)", {

  df <- data.frame(
    block = "a",
    phase = c("status", "eval", "data"),
    severity = c("warning", "warning", "error"),
    message = c("waiting note", "real warning", "real error"),
    id = c("s1", "w1", "e1"),
    stringsAsFactors = FALSE
  )

  buckets <- block_cond_buckets(df)

  expect_named(buckets, c("error", "warning", "message"))

  # The status-phase explanation is not painted as a warning ...
  expect_identical(buckets$warning, c(w1 = "real warning"))

  # ... while genuine warnings and errors still surface, keyed by id.
  expect_identical(buckets$error, c(e1 = "real error"))
  expect_length(buckets$message, 0L)
})

test_that("block_status_style is the shared status-dot spec (#290)", {

  waiting <- block_status_style("waiting")
  expect_identical(waiting$color, "#f59e0b")
  expect_identical(waiting$size, 8L)
  expect_identical(waiting$ring, 2L)
  expect_identical(waiting$ring_color, "#ffffff")
  expect_identical(waiting$label, "Waiting for a data input")

  expect_identical(block_status_style("unset")$color, "#eab308")
  expect_identical(block_status_style("failed")$color, "#dc2626")

  # `ready`, `dormant` and non-strings carry no indicator.
  for (st in list("ready", "dormant", NULL, character(), c("a", "b"))) {
    expect_null(block_status_style(st))
  }
})

test_that("block_status_badge is the shared badge derivation (#314)", {

  # Attention states pass through to the shared style spec.
  expect_identical(block_status_badge("waiting"), block_status_style("waiting"))
  expect_identical(block_status_badge("unset"), block_status_style("unset"))
  expect_identical(block_status_badge("failed"), block_status_style("failed"))

  # An error condition promotes the badge to `failed`, catching a render-phase
  # error that leaves the eval status `ready`.
  expect_identical(
    block_status_badge("ready", 2L),
    block_status_style("failed")
  )

  # `ready` and an absent status carry no badge.
  expect_null(block_status_badge("ready"))
  expect_null(block_status_badge(NULL))

  # `dormant` is indeterminate: `NA` tells a persistent renderer to keep the
  # existing badge rather than clear it.
  expect_identical(block_status_badge("dormant"), NA)
})

test_that("a stale block carries a muted badge (#408)", {

  stale <- block_status_style("stale")

  expect_identical(stale$color, "#6b7280")
  expect_identical(stale$label, "Inputs changed since this block last ran")
  expect_identical(stale$size, 8L)
  expect_identical(stale$ring, 2L)
  expect_identical(stale$ring_color, "#ffffff")

  # The muted treatment is its own, not a reuse of an attention colour.
  attention <- lapply(c("waiting", "unset", "failed"), block_status_style)
  expect_false(stale$color %in% chr_xtr(attention, "color"))

  # A stale block flags as out of date rather than falling through to "no
  # badge", which is what made it indistinguishable from a healthy one.
  expect_type(block_status_badge("stale"), "list")
  expect_identical(block_status_badge("stale"), stale)
  expect_match(block_status_dot_attrs("stale")$style, "#6b7280", fixed = TRUE)

  # Recorded errors do not survive the input change that made the block stale:
  # they were raised against inputs it no longer has, and it has not re-run, so
  # a red dot would assert a failure nobody has observed on the current inputs.
  expect_identical(block_status_badge("stale", 2L), stale)

  # A dormant block keeps its error badge -- nothing about its inputs changed,
  # so the last-known failure still describes them.
  expect_identical(
    block_status_badge("dormant", 2L),
    block_status_style("failed")
  )

  # The body keeps its last-known output, so no placeholder note replaces it.
  expect_null(block_status_note("stale"))
})

test_that("block status indicator + note reflect eval status (#290)", {

  waiting_dot <- block_status_dot_attrs("waiting")
  expect_match(waiting_dot$style, "#f59e0b", fixed = TRUE)
  expect_identical(waiting_dot$title, "Waiting for a data input")
  expect_identical(waiting_dot[["aria-label"]], "Waiting for a data input")
  # The white ring is carried in the written style from the shared spec, not
  # the CSS.
  expect_match(waiting_dot$style, "0 0 0 2px #ffffff", fixed = TRUE)

  # An error condition reddens the dot even when the eval status is `ready`,
  # matching the DAG node badge.
  expect_match(
    block_status_dot_attrs("ready", 2L)$style,
    "#dc2626",
    fixed = TRUE
  )

  expect_match(block_status_dot_attrs("unset")$style, "#eab308", fixed = TRUE)
  expect_match(block_status_dot_attrs("failed")$style, "#dc2626", fixed = TRUE)

  expect_match(
    as.character(block_status_note("waiting")),
    "Waiting for a data input"
  )
  expect_match(
    as.character(block_status_note("unset")),
    "Set this block's inputs",
    fixed = TRUE
  )

  # `failed` keeps the error styling, so no placeholder note.
  expect_null(block_status_note("failed"))

  # A `ready` or `dormant` block, and an absent status, carry no affordance:
  # the dot's attributes are all cleared rather than left stale from the last
  # status.
  blank <- list(style = "", title = "", role = "", `aria-label` = "")

  for (st in list("ready", "dormant", NULL, character(), c("a", "b"))) {
    expect_identical(block_status_dot_attrs(st), blank)
    expect_null(block_status_note(st))
  }

  # The slot keys on the raw status and the stylesheet reveals a note only for
  # the two that have one, so a status without a note matches nothing.
  expect_identical(
    block_status_note_attrs("ready"),
    list(`data-status` = "ready")
  )
  expect_identical(block_status_note_attrs(NULL), list(`data-status` = ""))
})

test_that("edit block server surfaces eval status reactively (#290)", {

  status <- reactiveVal("waiting")

  empty_conds <- data.frame(
    block = character(), phase = character(), severity = character(),
    message = character(), id = character()
  )
  block_conds <- reactiveVal(empty_conds)

  board <- reactiveValues(
    board = new_dock_board(blocks = c(a = new_dataset_block())),
    eval = list(a = reactive(status())),
    blocks = list(a = list(server = list(conditions = block_conds)))
  )

  testServer(
    edit_block_server(),
    {
      session$flushReact()

      expect_identical(blk_status(), "waiting")
      expect_match(output$status_indicator$style, "#f59e0b", fixed = TRUE)
      expect_identical(output$status_note, list(`data-status` = "waiting"))

      status("failed")
      session$flushReact()

      # A `failed` block shows the dot but reveals no note -- the raised error
      # uses the error styling instead of a status placeholder.
      expect_identical(blk_status(), "failed")
      expect_match(output$status_indicator$style, "#dc2626", fixed = TRUE)
      expect_identical(output$status_note, list(`data-status` = "failed"))

      status("ready")
      session$flushReact()

      # A ready block with no conditions carries no status affordance at all.
      expect_identical(blk_status(), "ready")
      expect_identical(output$status_indicator$style, "")
      expect_identical(output$status_indicator$title, "")

      # A render-phase error leaves the eval status `ready` but still reddens
      # the dot, matching the DAG node badge (the note stays hidden).
      block_conds(
        data.frame(
          block = "a", phase = "render", severity = "error",
          message = "boom", id = "e1", stringsAsFactors = FALSE
        )
      )
      session$flushReact()

      expect_identical(blk_status(), "ready")
      expect_match(output$status_indicator$style, "#dc2626", fixed = TRUE)
      expect_identical(output$status_note, list(`data-status` = "ready"))
    },
    args = list(
      block_id = "a",
      board = board,
      update = reactiveVal()
    )
  )
})
