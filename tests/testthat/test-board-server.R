test_that("board server", {

  # Multi-view path is exercised by the default `dock_layouts(Page = ...)`
  # layout — `board_server_callback` returns an extra `view_data` reactive.
  board_rv_1 <- board_args(
    blocks = c(a = new_dataset_block())
  )

  with_mock_session(
    {
      res <- board_server_callback(board_rv_1, update = reactiveVal())

      expect_type(res, "list")
      expect_named(res, c("dock", "actions", "view_data"))

      # `dock` is the active-dock reactiveValues handle the extensions
      # receive; its contents are filled by the reconcile pass (init render),
      # exercised by the app test — here we assert only the returned shape.
      expect_s3_class(res[["dock"]], "reactivevalues")
      expect_s3_class(res[["view_data"]], "reactive")
    }
  )

  board_rv_2 <- board_args(
    blocks = c(a = new_dataset_block()),
    extensions = new_edit_board_extension()
  )

  with_mock_session(
    {
      res <- board_server_callback(board_rv_2, update = reactiveVal())

      expect_type(res, "list")
      expect_named(
        res,
        c("dock", "actions", "view_data", "edit_board_extension")
      )

      expect_s3_class(res[["dock"]], "reactivevalues")

      ext <- res[["edit_board_extension"]]

      expect_type(ext, "list")
      expect_length(ext, 1L)
      expect_named(ext, "state")

      expect_identical(ext[["state"]], list())
    }
  )

  # manage_dock is now a moduleServer — use with_mock_context directly
  # since with_mock_session's env cloning breaks nested moduleServer observers
  mod_input <- function(name) paste0("dock_main-", name)

  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  res <- with_mock_context(ms, {
    manage_dock("dock_main", board_rv_2, update = reactiveVal())
  })

  ms$flushReact()

  n_panels <- res$n_panels

  do.call(
    ms$setInputs,
    set_names(list(TRUE), mod_input(dock_input("initialized")))
  )

  expect_identical(isolate(n_panels()), 2L)

  do.call(
    ms$setInputs,
    set_names(
      list(as_block_panel_id("a")),
      mod_input(dock_input("panel-to-remove"))
    )
  )

  expect_identical(isolate(n_panels()), 1L)

  do.call(
    ms$setInputs,
    set_names(
      list(as_ext_panel_id("edit_board_extension")),
      mod_input(dock_input("panel-to-remove"))
    )
  )

  expect_identical(isolate(n_panels()), 0L)

  do.call(
    ms$setInputs,
    set_names(
      list(
        1L,
        c(
          as_block_panel_id("a"),
          as_ext_panel_id("edit_board_extension")
        )
      ),
      c(mod_input("confirm_add"), mod_input("add_dock_panel"))
    )
  )

  expect_identical(isolate(n_panels()), 2L)

  ms2 <- new_mock_session()
  withr::defer(if (!ms2$isClosed()) ms2$close())

  res2 <- with_mock_context(ms2, {
    manage_dock("dock_main", board_rv_2, update = reactiveVal())
  })

  ms2$flushReact()

  prevs <- res2$prev_active_group
  trail <- res2$active_group_trail

  expect_null(isolate(prevs()))
  expect_null(isolate(trail()))

  do.call(
    ms2$setInputs,
    set_names(list("1"), mod_input(dock_input("active-group")))
  )

  expect_null(isolate(prevs()))
  expect_identical(isolate(trail()), "1")

  do.call(
    ms2$setInputs,
    set_names(list("2"), mod_input(dock_input("active-group")))
  )

  expect_identical(isolate(prevs()), "1")
  expect_identical(isolate(trail()), "2")

  do.call(
    ms2$setInputs,
    set_names(list("2"), mod_input(dock_input("active-group")))
  )

  expect_identical(isolate(prevs()), "1")
  expect_identical(isolate(trail()), "2")

  do.call(
    ms2$setInputs,
    set_names(list("1"), mod_input(dock_input("active-group")))
  )

  expect_identical(isolate(prevs()), "2")
  expect_identical(isolate(trail()), "1")

  ms3 <- new_mock_session()
  withr::defer(if (!ms3$isClosed()) ms3$close())

  upd <- reactiveVal()

  with_mock_context(ms3, {
    manage_dock("dock_main", board_rv_2, update = upd)
  })

  ms3$flushReact()

  panel_lookups <- 0L

  with_mocked_bindings(
    {
      with_mock_context(ms3, {
        upd(
          list(
            blocks = list(
              mod = list(a = list(block_name = "Test block"))
            )
          )
        )
      })
      ms3$flushReact()

      with_mock_context(ms3, {
        upd(
          list(
            blocks = list(
              mod = list(a = list(dataset = "iris"))
            )
          )
        )
      })
      ms3$flushReact()
    },
    get_dock_panel = function(...) {
      panel_lookups <<- panel_lookups + 1L
      list(title = "Old title")
    }
  )

  expect_identical(panel_lookups, 1L)
})

test_that("renaming a block refreshes the dock panel title (#193)", {

  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  # The rename observer runs before the board update is applied, so board$board
  # still carries the old name; the new name lives only in the mod delta. A
  # board read would relabel to the old title and no-op -- assert the panel
  # picks up the delta's name instead.
  board_rv <- board_args(blocks = c(a = new_dataset_block()))
  old_name <- with_mock_context(
    ms,
    isolate(block_name(board_blocks(board_rv$board)[["a"]]))
  )
  upd <- reactiveVal()

  with_mock_context(ms, {
    manage_dock("dock_main", board_rv, update = upd)
  })

  ms$flushReact()

  title_set <- NULL

  with_mocked_bindings(
    with_mocked_bindings(
      {
        with_mock_context(ms, {
          upd(
            list(
              blocks = list(
                mod = list(a = list(block_name = "Renamed"))
              )
            )
          )
        })
        ms$flushReact()
      },
      set_panel_title = function(proxy, id, title, ...) {
        title_set <<- title
        invisible(proxy)
      },
      .package = "dockViewR"
    ),
    get_dock_panel = function(...) list(title = old_name)
  )

  expect_identical(title_set, "Renamed")
})

test_that("live_view_data is NULL while any view layout is uninitialized", {
  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  layouts <- with_mock_context(ms, {
    list(A = reactiveVal(NULL), B = reactiveVal(NULL))
  })

  res <- with_mock_context(ms, {
    client_views <- reactiveVal(
      dock_layouts(A = new_dock_layout(), B = new_dock_layout())
    )
    ids <- names(client_views())
    docks <- reactiveValues()
    docks[[ids[[1L]]]] <- list(layout = layouts$A)
    docks[[ids[[2L]]]] <- list(layout = layouts$B)
    client_active <- reactiveVal(ids[[1L]])
    list(vd = live_view_data(client_views, docks, client_active))
  })

  expect_null(isolate(res$vd()))

  with_mock_context(ms, layouts$A(list(grid = list(), panels = list())))
  expect_null(isolate(res$vd()))

  with_mock_context(ms, layouts$B(list(grid = list(), panels = list())))

  lys <- isolate(res$vd())
  expect_s3_class(lys, "dock_layouts")
  expect_identical(unname(view_names(lys)), c("A", "B"))
  expect_identical(active_name(lys), "A")
})

test_that("live_view_data re-evaluates once docks are populated (#243)", {

  # Regression: live_view_data first evaluated with `docks` still empty (its
  # dock module not yet created by reconcile) and hit the empty-dock early
  # return. When `docks` was a plain environment that read took no reactive
  # dependency, so reconcile populating `docks` never re-triggered it --
  # view_data() stayed NULL for the session and serialize fell back to the
  # default layout. As a `reactiveValues`, reading `docks[[v_id]]` (even for an
  # absent key) subscribes, so creating the dock re-evaluates this -- with no
  # separate signal and regardless of flush order.
  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  res <- with_mock_context(ms, {
    client_views <- reactiveVal(dock_layouts(A = new_dock_layout()))
    docks <- reactiveValues()
    client_active <- reactiveVal(NULL)
    list(
      vd = live_view_data(client_views, docks, client_active),
      docks = docks,
      view = names(client_views())[[1L]]
    )
  })

  # First read with empty docks: NULL, but the absent-key read has subscribed.
  expect_null(isolate(res$vd()))

  # Mimic reconcile creating the dock: the reactiveValues write alone
  # re-triggers live_view_data.
  layout <- with_mock_context(
    ms, reactiveVal(list(grid = list(), panels = list()))
  )
  with_mock_context(ms, res$docks[[res$view]] <- list(layout = layout))

  lys <- isolate(res$vd())

  expect_s3_class(lys, "dock_layouts")
  expect_identical(unname(view_names(lys)), "A")
})

test_that("view_data() tracks a reported layout despite flush order (#243)", {

  # The same regression through the real reconcile + live_view_data composition.
  # Reading view_data() before the first flush reproduces the init order the
  # bug needs -- the upsync reads live_view_data while `docks` is still empty,
  # before reconcile creates the dock modules. The browser then reports a
  # rearranged layout via the dock `_state` input, which view_data() must pick
  # up rather than staying frozen at the seed.
  board_rv <- board_args(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(Page = dock_layout("a", "b"))
  )

  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  res <- with_mock_context(
    ms, board_server_callback(board_rv, update = reactiveVal())
  )

  # Force the first read before reconcile runs: docks empty, so NULL.
  expect_null(isolate(res$view_data()))

  ms$flushReact()

  # The browser reports the live grid through the dock `_state` input, carrying
  # the real `block_panel-*` ids reversed from the seeded order. It is built
  # with the public dock_layout() constructor -- which dockview_to_layout()
  # accepts unclassed -- rather than hand-rolled dockview JSON.
  reported <- unclass(dock_layout("block_panel-b", "block_panel-a"))
  do.call(ms$setInputs, set_names(list(reported), "Page-dock_state"))
  ms$flushReact()

  vd <- isolate(res$view_data())

  # view_data() carries the reported order, not the frozen `[a, b]` default.
  expect_s3_class(vd, "dock_layouts")
  expect_identical(
    layout_panel_ids(vd[["Page"]]),
    c("block_panel-b", "block_panel-a")
  )
})

test_that("layouts_to_board_observer fires update views on divergence", {
  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = list("a"), B = list("b"))
  )

  rv <- with_mock_context(ms, reactiveValues(board = brd))
  vd <- with_mock_context(ms, reactiveVal(NULL))

  captured <- list()
  upd <- function(payload) {
    captured[[length(captured) + 1L]] <<- payload
  }

  with_mock_context(ms, layouts_to_board_observer(vd, upd, rv))
  ms$flushReact()

  expect_length(captured, 0L)

  new_views <- isolate(board_layouts(rv$board))
  b_id <- vid(new_views, "B")
  active_view(new_views) <- b_id

  with_mock_context(ms, vd(new_views))
  ms$flushReact()

  expect_length(captured, 1L)
  expect_named(captured[[1L]], "views")
  expect_identical(captured[[1L]]$views$active, b_id)
  expect_null(captured[[1L]]$views$mod)
})

test_that("layouts_to_board_observer is idempotent when nothing changed", {
  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(Page = list("a"))
  )

  rv <- with_mock_context(ms, reactiveValues(board = brd))
  vd <- with_mock_context(ms, reactiveVal(NULL))

  fire_count <- 0L
  upd <- function(payload) fire_count <<- fire_count + 1L

  with_mock_context(ms, layouts_to_board_observer(vd, upd, rv))
  ms$flushReact()

  with_mock_context(ms, vd(isolate(board_layouts(rv$board))))
  ms$flushReact()

  expect_identical(fire_count, 0L)
})

test_that("view nav renders one labelled item per view (#189)", {

  board <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(First = dock_layout("a"), Second = dock_layout("b")),
    active = "First"
  )

  ui <- view_nav_ui("brd", board_layouts(board))

  items <- htmltools::tagQuery(ui)$find(".blockr-view-item")$selectedTags()
  spans <- htmltools::tagQuery(ui)$find(".blockr-view-item-name")$selectedTags()

  labels <- chr_ply(
    spans,
    function(span) {
      txt <- span$children
      if (length(txt)) as.character(txt[[1L]]) else ""
    }
  )

  expect_length(items, 2L)
  expect_setequal(labels, c("First", "Second"))
})

test_that("reconcile_views adds a nav item only for unshown views (#189)", {

  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  board <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(First = dock_layout("a"), Second = dock_layout("b")),
    active = "First"
  )

  sent <- list()
  rec_session <- list(
    sendInputMessage = function(input_id, message) {
      sent[[length(sent) + 1L]] <<- message
    }
  )

  # Stand in for create_view: register the dock without the DOM / module
  # machinery so `names(docks)` tracks created views. The add decision under
  # test keys off client_views, not docks.
  stub_create <- function(v_id, layout, board, update, session, docks, ...) {
    dock <- list(
      layout = function() NULL,
      live_panels = reactiveVal(as.character(layout_panel_ids(layout)))
    )
    docks[[v_id]] <- dock
  }

  docks <- with_mock_context(ms, reactiveValues())
  client_views <- with_mock_context(
    ms,
    reactiveVal(seed_view_state(board_layouts(board)))
  )
  client_active <- with_mock_context(ms, reactiveVal(NULL))
  active_dock <- with_mock_context(ms, reactiveValues())
  update <- with_mock_context(ms, reactiveVal())

  reconcile <- function(brd) {
    board <- with_mock_context(ms, reactiveValues(board = brd))
    with_mocked_bindings(
      with_mock_context(
        ms,
        reconcile_views(
          board, update, docks, active_dock, client_active, client_views,
          rec_session
        )
      ),
      create_view = stub_create,
      switch_active_view = function(...) invisible(),
      .package = "blockr.dock"
    )
  }

  adds <- function() Filter(function(m) "add" %in% names(m), sent)

  # Init: board_ui has statically rendered both views, so reconcile must add
  # none (the bug re-added each as a blank-labelled duplicate).
  reconcile(board)
  expect_length(adds(), 0L)

  # A view added at runtime (named only by its list-key id) reaches the nav
  # exactly once, labelled from the id rather than the empty layout name.
  board <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(
      First = dock_layout("a"),
      Second = dock_layout("b"),
      Third = dock_layout("a")
    ),
    active = "First"
  )

  sent <- list()
  reconcile(board)

  rt <- adds()

  expect_length(rt, 1L)
  expect_identical(rt[[1L]]$add$id, "Third")
  expect_identical(rt[[1L]]$add$name, "Third")
})

test_that("reconcile_views forwards the live board to created views (#194)", {

  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  board <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(Page = dock_layout())
  )

  rv <- with_mock_context(ms, reactiveValues(board = board))

  forwarded <- NULL
  capture_create <- function(v_id, layout, board, update, session, docks, ...) {
    forwarded <<- board
    docks[[v_id]] <- list(layout = function() NULL)
  }

  docks <- with_mock_context(ms, reactiveValues())
  client_views <- with_mock_context(ms, reactiveVal(list()))
  client_active <- with_mock_context(ms, reactiveVal(NULL))
  active_dock <- with_mock_context(ms, reactiveValues())
  update <- with_mock_context(ms, reactiveVal())
  rec_session <- list(
    ns = identity,
    sendInputMessage = function(...) invisible()
  )

  with_mocked_bindings(
    with_mock_context(
      ms,
      reconcile_views(
        rv, update, docks, active_dock, client_active, client_views,
        rec_session
      )
    ),
    create_view = capture_create,
    switch_active_view = function(...) invisible(),
    .package = "blockr.dock"
  )

  # manage_dock's interaction observers (add panel, rename block) read
  # board$board after the fact, so reconcile must forward the reactive handle.
  # The bug forwarded the snapshot object, whose $board is NULL — crashing
  # board_blocks() with `is_board(x) is not TRUE` on the next add-panel click.
  expect_true(is_dock_board(isolate(forwarded$board)))
  expect_silent(isolate(board_block_ids(forwarded$board)))
})

test_that("reconcile_views skips an added panel pending its echo (#196)", {

  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  session <- list(
    ns = identity,
    sendInputMessage = function(input_id, message) invisible(),
    sendCustomMessage = function(type, message) invisible()
  )

  # board_layouts holds both panels synchronously (the block-add fold) and the
  # proxy's live_panels tracker has caught `b` from insert_block_ui, but the
  # browser has not yet echoed `b` back through get_dock. reconcile must not
  # restore here: membership agrees, so the only apparent difference is the
  # not-yet-echoed panel insert_block_ui already placed -- restoring would wipe
  # it (#196).
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(V = dock_layout("a", "b"))
  )
  board <- with_mock_context(ms, reactiveValues(board = brd))

  target_ids <- layout_panel_ids(board_layouts(brd)[["V"]])

  behind <- board_layouts(
    new_dock_board(
      blocks = c(a = new_dataset_block()),
      layouts = list(V = dock_layout("a"))
    )
  )[["V"]]

  docks <- with_mock_context(ms, reactiveValues())
  docks[["V"]] <- list(
    layout = function() behind,
    live_panels = with_mock_context(ms, reactiveVal(target_ids))
  )

  client_views <- with_mock_context(
    ms,
    reactiveVal(seed_view_state(board_layouts(brd)))
  )
  client_active <- with_mock_context(ms, reactiveVal("V"))
  active_dock <- with_mock_context(ms, reactiveValues())
  update <- with_mock_context(ms, reactiveVal())

  diffed <- 0L

  with_mocked_bindings(
    with_mock_context(
      ms,
      reconcile_views(
        board, update, docks, active_dock, client_active, client_views,
        session
      )
    ),
    apply_layout_diff = function(...) diffed <<- diffed + 1L,
    switch_active_view = function(...) invisible(),
    .package = "blockr.dock"
  )

  expect_identical(diffed, 0L)
})

test_that("reconcile_views pushes a programmatic membership change", {

  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  session <- list(
    ns = identity,
    sendInputMessage = function(input_id, message) invisible(),
    sendCustomMessage = function(type, message) invisible()
  )

  # board_layouts now carries `a` + `b`, but the dock's tracked membership is
  # only `a`: a programmatic views$mod added `b`'s panel with no live op, so
  # reconcile must push the new layout to the dock and record the new set.
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(V = dock_layout("a", "b"))
  )
  board <- with_mock_context(ms, reactiveValues(board = brd))

  target <- board_layouts(brd)[["V"]]

  behind_ids <- layout_panel_ids(
    board_layouts(
      new_dock_board(
        blocks = c(a = new_dataset_block()),
        layouts = list(V = dock_layout("a"))
      )
    )[["V"]]
  )

  live_panels <- with_mock_context(ms, reactiveVal(behind_ids))

  docks <- with_mock_context(ms, reactiveValues())
  docks[["V"]] <- list(
    layout = function() NULL,
    live_panels = live_panels
  )

  client_views <- with_mock_context(
    ms,
    reactiveVal(seed_view_state(board_layouts(brd)))
  )
  client_active <- with_mock_context(ms, reactiveVal("V"))
  active_dock <- with_mock_context(ms, reactiveValues())
  update <- with_mock_context(ms, reactiveVal())

  captured <- NULL

  with_mocked_bindings(
    with_mock_context(
      ms,
      reconcile_views(
        board, update, docks, active_dock, client_active, client_views,
        session
      )
    ),
    apply_layout_diff = function(view, target, ...) {
      captured <<- list(view = view, target = target)
    },
    switch_active_view = function(...) invisible(),
    .package = "blockr.dock"
  )

  expect_false(is.null(captured))
  expect_identical(captured$view, "V")
  expect_true(layouts_match(captured$target, target))

  # The push records the new membership, so a later pass is a no-op.
  expect_setequal(isolate(live_panels()), layout_panel_ids(target))
})

test_that("apply_board_update.dock_board switches active view", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = list("a"), B = list("b"))
  )

  out <- apply_board_update(brd, list(views = list(active = vid(brd, "B"))))

  expect_true(is_dock_board(out))
  expect_identical(active_name(out), "B")
})

test_that("apply_board_update folds an added block into the active view", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(A = list("a"), B = list("a"))
  )

  active <- active_view(board_layouts(brd))
  other <- setdiff(names(board_layouts(brd)), active)

  out <- apply_board_update(
    brd,
    list(blocks = list(add = as_blocks(c(b = new_head_block()))))
  )

  # The added block's panel lands in the active view synchronously -- not via
  # the debounced live sync -- and nowhere else (#196, persistence gap).
  expect_true(
    "block_panel-b" %in% layout_panel_ids(board_layouts(out)[[active]])
  )
  expect_false(
    "block_panel-b" %in% layout_panel_ids(board_layouts(out)[[other]])
  )
})

test_that("validate_board_update.dock_board rejects malformed views slot", {
  brd <- new_dock_board(blocks = c(a = new_dataset_block()))

  expect_error(
    validate_board_update(list(views = "not a delta"), brd),
    class = "dock_views_delta_invalid"
  )
})

test_that("views slot flows through full board_update lifecycle", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = list("a"), B = list("b"))
  )

  testServer(
    blockr.core:::board_server.board,
    {
      session$flushReact()

      board_update(list(views = list(active = "B")))
      session$flushReact()

      expect_identical(active_name(rv$board), "B")
      expect_null(board_update())
    },
    args = list(
      x = brd,
      plugins = list(blockr.core::manage_blocks())
    )
  )
})

test_that("validate_board_update.dock_board rejects bad extensions slot", {
  brd <- new_dock_board(blocks = c(a = new_dataset_block()))

  expect_error(
    validate_board_update(list(extensions = "not a delta"), brd),
    class = "dock_extensions_delta_invalid"
  )
})

test_that("extensions mod state is applied via the update lifecycle", {

  board_rv <- board_args(
    blocks = c(a = new_dataset_block()),
    extensions = new_ctrl_extension(content = "# old")
  )

  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  upd <- reactiveVal()

  res <- with_mock_context(ms, board_server_callback(board_rv, update = upd))

  ms$flushReact()

  # ext_res is spread into the callback result, so the extension's
  # controllable state is reachable without a dock handle.
  content <- res$doc_extension$state$content

  expect_s3_class(content, "reactiveVal")
  expect_identical(isolate(content()), "# old")

  # An `extensions$mod` delta is applied by the closure observer, not the
  # (now pure) apply hook.
  upd(
    list(
      extensions = list(mod = list(doc_extension = list(content = "# new")))
    )
  )
  ms$flushReact()

  expect_identical(isolate(content()), "# new")
})

test_that("a live-only panel add folds into board_layouts (#217)", {

  board_rv <- board_args(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(Page = list("a"))
  )

  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  upd <- reactiveVal()
  res <- with_mock_context(ms, board_server_callback(board_rv, update = upd))
  ms$flushReact()

  live_panels <- isolate(res$dock$live_panels)
  expect_false(is.null(live_panels))

  # The add-panel modal / show_panel touch only the live dock; board_layouts
  # must catch up in the same flush (via the fold observer), not the debounced
  # echo, or a later reconcile would restore a layout missing the new panel.
  cur <- isolate(live_panels())
  isolate(live_panels(c(cur, "block_panel-b")))
  ms$flushReact()

  delta <- isolate(upd())

  expect_false(is.null(delta$views$mod))
  expect_setequal(
    layout_panel_ids(delta$views$mod[[1L]]),
    c("block_panel-a", "block_panel-b")
  )
})

test_that("extension servers can read peer extension state", {

  peers <- NULL

  probe <- new_dock_extension(
    server = function(id, board, update, extensions, ...) {
      peers <<- extensions
      moduleServer(id, function(input, output, session) list(state = list()))
    },
    ui = function(id) tagList(),
    name = "Probe",
    class = "probe_extension",
    ctor = function(...) NULL
  )

  board_rv <- board_args(
    blocks = c(a = new_dataset_block()),
    extensions = as_dock_extensions(
      list(new_ctrl_extension(content = "# hi"), probe)
    )
  )

  with_mock_session(
    {
      board_server_callback(board_rv, update = reactiveVal())

      expect_true("doc_extension" %in% ls(peers))
      expect_identical(
        isolate(peers[["doc_extension"]]$state$content()),
        "# hi"
      )
    }
  )
})

test_that("New view modal confirm submits an add-and-activate delta", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = list("a"))
  )

  captured <- NULL

  testServer(
    function(input, output, session) {
      client_views <- reactiveVal(board_layouts(brd))
      board <- reactiveValues(board = brd)
      add_view_observer(
        client_views,
        session,
        board = board,
        update = function(x) captured <<- x
      )
    },
    {
      session$setInputs(
        view_new_name = "Charts",
        view_new_blocks = "a",
        view_new_exts = character(),
        confirm_view_add = 1L
      )
      session$flushReact()

      # The new view has no id yet, so "add and activate" travels as the
      # add key in both slots; the dock resolves it to the minted id in
      # normalize_views_delta().
      expect_named(captured$views, c("add", "active"))
      expect_identical(captured$views$active, "Charts")
      expect_identical(names(captured$views$add), "Charts")
      expect_true(is_dock_layout(captured$views$add[[1L]]))
    }
  )
})

test_that("locked board switches views client-side, no board update (#127)", {

  withr::local_options(blockr.locked = TRUE)

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = dock_layout("a"), B = dock_layout("b")),
    active = "A"
  )

  client_active <- reactiveVal("A")
  captured <- NULL

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        function(input, output, session) {
          switch_view_observer(
            session,
            update = function(x) captured <<- x,
            client_active = client_active,
            board = reactiveValues(board = brd),
            docks = reactiveValues(),
            active_dock = reactiveValues()
          )
        }
      )
    },
    {
      # The nav starts on the active view; that first (init) input is
      # swallowed by ignoreInit. The second is the switch under test.
      session$setInputs(view_nav = "A")
      session$setInputs(view_nav = "B")
      session$flushReact()

      # The active view advances in session state, but nothing travels the
      # update channel: on a locked board the core gate rejects every
      # board_update, so navigation has to switch the DOM directly.
      expect_identical(client_active(), "B")
      expect_null(captured)
    }
  )
})

test_that("unlocked board switches views through the update channel", {

  withr::local_options(blockr.locked = FALSE)

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = dock_layout("a"), B = dock_layout("b")),
    active = "A"
  )

  client_active <- reactiveVal("A")
  captured <- NULL

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        function(input, output, session) {
          switch_view_observer(
            session,
            update = function(x) captured <<- x,
            client_active = client_active,
            board = reactiveValues(board = brd),
            docks = reactiveValues(),
            active_dock = reactiveValues()
          )
        }
      )
    },
    {
      # The nav starts on the active view; that first (init) input is
      # swallowed by ignoreInit. The second is the switch under test.
      session$setInputs(view_nav = "A")
      session$setInputs(view_nav = "B")
      session$flushReact()

      # The switch rides the update lifecycle; flipping client_active is the
      # reconcile pass's job, so the observer leaves it untouched.
      expect_identical(captured, list(views = list(active = "B")))
      expect_identical(client_active(), "A")
    }
  )
})

test_that("hiding a block's input section freezes it on the channel (#127)", {

  brd <- new_board(blocks = c(a = new_dataset_block(), b = new_dataset_block()))

  frozen <- reactiveVal(NULL)
  vis_a <- reactiveVal(c("inputs", "outputs"))
  vis_b <- reactiveVal(c("inputs", "outputs"))

  board <- reactiveValues(
    board = brd,
    blocks = list(
      a = list(server = list(visible = vis_a)),
      b = list(server = list(visible = vis_b))
    )
  )

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        function(input, output, session) freeze_hidden_inputs(board, frozen)
      )
    },
    {
      session$flushReact()
      # Both sections shown: nothing frozen.
      expect_identical(frozen(), character())

      # Hide b's input section -> b is named on the channel.
      vis_b(c("outputs"))
      session$flushReact()
      expect_identical(frozen(), "b")

      # Show it again -> thaw.
      vis_b(c("inputs", "outputs"))
      session$flushReact()
      expect_identical(frozen(), character())
    }
  )
})
