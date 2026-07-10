test_that("board server", {

  # Multi-view path is exercised by the default single-view board --
  # `board_server_callback` returns an extra `view_data` reactive.
  board_rv_1 <- board_args(
    blocks = c(a = new_dataset_block())
  )

  with_mock_session(
    {
      res <- board_server_callback(
        board_rv_1,
        update = reactiveVal(),
        visible = reactiveVal()
      )

      expect_type(res, "list")
      expect_named(res, c("dock", "actions", "view_data", "extensions"))

      # `dock` is the internal active-dock reactiveValues handle (the block
      # insert / remove plugin places panels through it), still returned to the
      # board server; its contents are filled by the reconcile pass (init
      # render), exercised by the app test — here we assert only the shape.
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
      res <- board_server_callback(
        board_rv_2,
        update = reactiveVal(),
        visible = reactiveVal()
      )

      expect_type(res, "list")
      expect_named(
        res,
        c("dock", "actions", "view_data", "extensions")
      )

      expect_s3_class(res[["dock"]], "reactivevalues")

      ext <- res[["extensions"]][["edit_board"]]

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
      list(as_ext_panel_id("edit_board")),
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
          as_ext_panel_id("edit_board")
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

test_that("an extension key never leaks into core's plugin args (#318)", {

  # `edit` is a prefix of core's `edit_block` block-server formal; returned as
  # a bare entry it would partial-match when core splats the callback result
  # into every block server's args, hijacking the block server. The extension
  # results ride nested under `extensions`, off that arg namespace.
  board_rv <- board_args(
    blocks = c(a = new_dataset_block()),
    extensions = list(edit = new_edit_board_extension())
  )

  with_mock_session(
    {
      res <- board_server_callback(
        board_rv,
        update = reactiveVal(),
        visible = reactiveVal()
      )

      expect_false("edit" %in% names(res))
      expect_named(res[["extensions"]], "edit")
    }
  )
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

test_that("rename skips views without the block's panel (#116)", {

  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  # A board whose single view holds only `a`; `b` is parked with no panel in
  # this view's dock. The per-view rename observer fires for every view, so it
  # must skip the ones that do not hold the renamed block -- pushing a title to
  # an absent panel reaches dockView with an unknown id and throws client-side.
  board_rv <- board_args(
    blocks = c(a = new_dataset_block("iris"), b = new_dataset_block("mtcars")),
    views = list(v1 = dock_view("a", name = "V1"))
  )
  upd <- reactiveVal()

  with_mock_context(ms, {
    manage_dock("dock_main", board_rv, update = upd)
  })

  ms$flushReact()

  renamed <- character()

  rename <- function(blk_id, name) {
    with_mocked_bindings(
      with_mocked_bindings(
        {
          with_mock_context(ms, {
            upd(list(blocks = list(mod = set_names(
              list(list(block_name = name)), blk_id
            ))))
          })
          ms$flushReact()
        },
        set_panel_title = function(proxy, id, title, ...) {
          renamed <<- c(renamed, as.character(id))
          invisible(proxy)
        },
        .package = "dockViewR"
      ),
      get_dock_panel = function(id, ...) {
        if (identical(as.character(id), as.character(as_block_panel_id("a")))) {
          list(title = "iris")
        } else {
          NULL
        }
      }
    )
  }

  rename("b", "Renamed b")
  expect_identical(renamed, character())

  rename("a", "Renamed a")
  expect_identical(renamed, as.character(as_block_panel_id("a")))
})

test_that("live_view_data is NULL while any view layout is uninitialized", {
  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  layouts <- with_mock_context(ms, {
    list(A = reactiveVal(NULL), B = reactiveVal(NULL))
  })

  res <- with_mock_context(ms, {
    client_views <- reactiveVal(
      reconstruct_dock_views(list(A = dock_view(), B = dock_view()))
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
  expect_named(lys, c("views", "grids"))
  expect_s3_class(lys$views, "dock_views")
  expect_identical(unname(view_names(lys$views)), c("A", "B"))
  expect_identical(active_name(lys$views), "A")
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
    client_views <- reactiveVal(reconstruct_dock_views(list(A = dock_view())))
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

  expect_named(lys, c("views", "grids"))
  expect_identical(unname(view_names(lys$views)), "A")
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
    views = list(Page = c("a", "b"))
  )

  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  res <- with_mock_context(
    ms,
    board_server_callback(
      board_rv,
      update = reactiveVal(),
      visible = reactiveVal()
    )
  )

  # Force the first read before reconcile runs: docks empty, so NULL.
  expect_null(isolate(res$view_data()))

  ms$flushReact()

  # The browser reports the live grid through the dock `_state` input, carrying
  # the real `block_panel-*` ids reversed from the seeded order. The dockView
  # `_state` is the native grid tree plus active group, expanded from our grid
  # via the internal cast -- not hand-rolled JSON.
  g <- as_dock_grid(dock_grid("block_panel-b", "block_panel-a"))
  reported <- list(grid = grid_to_tree(g), activeGroup = "1")
  do.call(ms$setInputs, set_names(list(reported), "Page-dock_state"))
  ms$flushReact()

  vd <- isolate(res$view_data())

  # view_data() carries the reported order, not the frozen `[a, b]` default.
  expect_named(vd, c("views", "grids"))
  expect_identical(
    layout_panel_ids(vd$grids[["Page"]]),
    c("block_panel-b", "block_panel-a")
  )
})

test_that("visible_block_ids returns the front-tab block of each group", {

  two_groups <- dock_grid(panels("block_panel-a"), panels("block_panel-b"))
  expect_setequal(visible_block_ids(two_groups), c("a", "b"))

  background_tab <- dock_grid(
    panels("block_panel-a", "block_panel-b", active = "block_panel-a")
  )
  expect_identical(visible_block_ids(background_tab), "a")

  ext_front <- dock_grid(panels("ext_panel-editor"))
  expect_identical(visible_block_ids(ext_front), character())

  nested <- dock_grid(
    panels("block_panel-a"),
    group(panels("block_panel-b"), panels("ext_panel-editor"))
  )
  expect_setequal(visible_block_ids(nested), c("a", "b"))

  expect_identical(visible_block_ids(dock_grid()), character())
})

test_that("report_visible_observer publishes the active view's blocks", {
  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  env <- with_mock_context(ms, {
    layout_a <- reactiveVal(NULL)
    layout_b <- reactiveVal(NULL)

    docks <- new.env(parent = emptyenv())
    docks[["A"]] <- list(layout = layout_a)
    docks[["B"]] <- list(layout = layout_b)

    visible <- reactiveVal()
    client_active <- reactiveVal("A")

    report_visible_observer(visible, client_active, docks)

    list(a = layout_a, b = layout_b, active = client_active, visible = visible)
  })

  ms$flushReact()
  expect_null(isolate(env$visible()))

  with_mock_context(ms, env$a(
    dock_grid(panels("block_panel-a"), panels("block_panel-b"))
  ))
  ms$flushReact()
  expect_setequal(isolate(env$visible()), c("a", "b"))

  with_mock_context(ms, env$b(dock_grid(panels("block_panel-d"))))
  ms$flushReact()
  expect_setequal(isolate(env$visible()), c("a", "b"))

  with_mock_context(ms, env$a(
    dock_grid(panels("ext_panel-editor"), panels("block_panel-b"))
  ))
  ms$flushReact()
  expect_identical(isolate(env$visible()), "b")

  with_mock_context(ms, env$active("B"))
  ms$flushReact()
  expect_identical(isolate(env$visible()), "d")
})

test_that("report_visible_observer coalesces set-equal reports", {
  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  env <- with_mock_context(ms, {
    layout <- reactiveVal(NULL)

    docks <- new.env(parent = emptyenv())
    docks[["A"]] <- list(layout = layout)

    visible <- reactiveVal()
    client_active <- reactiveVal("A")

    report_visible_observer(visible, client_active, docks)

    list(layout = layout, visible = visible)
  })

  with_mock_context(ms, env$layout(
    dock_grid(panels("block_panel-a"), panels("block_panel-b"))
  ))
  ms$flushReact()

  first <- isolate(env$visible())
  expect_setequal(first, c("a", "b"))

  with_mock_context(ms, env$layout(
    dock_grid(panels("block_panel-b"), panels("block_panel-a"))
  ))
  ms$flushReact()

  expect_identical(isolate(env$visible()), first)
})

test_that("board_server_callback seeds visibility before the client reports", {

  board_rv <- board_args(
    blocks = c(a = new_dataset_block(), b = new_head_block())
  )

  with_mock_session({
    visible <- reactiveVal()
    board_server_callback(board_rv, update = reactiveVal(), visible = visible)

    expect_identical(isolate(visible()), "a")
  })
})

test_that("the visibility seed reads the active view's open tabs", {

  # An expressed tab group hides its back tab: only the front panel of each
  # group seeds visibility (the active view's placed grid at birth).
  board_rv <- board_args(
    blocks = c(
      a = new_dataset_block(), b = new_head_block(), d = new_head_block()
    ),
    grids = list(v = dock_grid(panels("a", "b", active = "b"), "d"))
  )

  with_mock_session({
    visible <- reactiveVal()
    board_server_callback(board_rv, update = reactiveVal(), visible = visible)

    expect_setequal(isolate(visible()), c("b", "d"))
  })
})

test_that("the visibility seed spans separate leaves", {

  # Separate single-panel leaves each front their own tab, so every member is
  # on-screen (none is a hidden back tab).
  board_rv <- board_args(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    grids = list(page = dock_grid("a", "b"))
  )

  with_mock_session({
    visible <- reactiveVal()
    board_server_callback(board_rv, update = reactiveVal(), visible = visible)

    expect_setequal(isolate(visible()), c("a", "b"))
  })
})

test_that("view nav renders one labelled item per view (#189)", {

  board <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(First = "a", Second = "b"),
    active = "First"
  )

  ui <- view_nav_ui("brd", board_views(board))

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
    views = list(First = "a", Second = "b"),
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
    reactiveVal(seed_view_state(board_views(board)))
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
    views = list(First = "a", Second = "b", Third = "a"),
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
    views = list(Page = list())
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

test_that("reconcile_views never pushes a layout back to a live dock (#259)", {

  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  session <- list(
    ns = identity,
    sendInputMessage = function(input_id, message) invisible(),
    sendCustomMessage = function(type, message) invisible()
  )

  # The board carries `a` + `b` for view V while the live dock's tracked
  # membership is only `a`: the committed board and the live dock have diverged.
  # A view's arrangement is client-owned and flows dock -> board only, so
  # reconcile must never restore the dock from the board. That board -> dock
  # push -- restore_dock faithfully replaying a board that lagged the live
  # dock -- is exactly the feedback loop that tore panels down on a slow
  # client (#252). Pre-#259 this divergence drove apply_layout_diff ->
  # restore_layout; now nothing pushes.
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(V = c("a", "b"))
  )
  board <- with_mock_context(ms, reactiveValues(board = brd))

  behind_ids <- view_members(
    board_views(
      new_dock_board(
        blocks = c(a = new_dataset_block()),
        views = list(V = "a")
      )
    )[["V"]]
  )

  docks <- with_mock_context(ms, reactiveValues())
  docks[["V"]] <- list(
    layout = function() NULL,
    live_panels = with_mock_context(ms, reactiveVal(behind_ids))
  )

  client_views <- with_mock_context(
    ms,
    reactiveVal(seed_view_state(board_views(brd)))
  )
  client_active <- with_mock_context(ms, reactiveVal("V"))
  active_dock <- with_mock_context(ms, reactiveValues())
  update <- with_mock_context(ms, reactiveVal())

  restored <- 0L

  with_mocked_bindings(
    with_mock_context(
      ms,
      reconcile_views(
        board, update, docks, active_dock, client_active, client_views,
        session
      )
    ),
    restore_layout = function(...) restored <<- restored + 1L,
    switch_active_view = function(...) invisible(),
    .package = "blockr.dock"
  )

  expect_identical(restored, 0L)
})

test_that("apply_board_update.dock_board switches active view", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(A = "a", B = "b")
  )

  out <- apply_board_update(brd, list(views = list(active = vid(brd, "B"))))

  expect_true(is_dock_board(out))
  expect_identical(active_name(out), "B")
})

test_that("apply_board_update folds an added block into the active view", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    views = list(A = "a", B = "a"),
    grids = list(A = dock_grid("a"), B = dock_grid("a"))
  )

  active <- active_view(board_views(brd))
  other <- setdiff(names(board_views(brd)), active)

  out <- apply_board_update(
    brd,
    list(blocks = list(add = as_blocks(c(b = new_head_block()))))
  )

  # The added block joins the active view's membership synchronously, and no
  # other view. Membership is authoritative, so it is placed in the active
  # view too -- a default spot until the client echo supplies its arrangement.
  expect_true("block_panel-b" %in% view_members(board_views(out)[[active]]))
  expect_false("block_panel-b" %in% view_members(board_views(out)[[other]]))
  expect_true(
    "block_panel-b" %in%
      layout_panel_ids(
        view_grid(board_views(out)[[active]], board_grids(out)[[active]])
      )
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
    views = list(A = "a", B = "b")
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

  res <- with_mock_context(
    ms,
    board_server_callback(board_rv, update = upd, visible = reactiveVal())
  )

  ms$flushReact()

  # ext_res rides under `extensions` in the callback result, so the
  # extension's controllable state is reachable without a dock handle.
  content <- res$extensions$doc$state$content

  expect_s3_class(content, "reactiveVal")
  expect_identical(isolate(content()), "# old")

  # An `extensions$mod` delta is applied by the closure observer, not the
  # (now pure) apply hook.
  upd(
    list(
      extensions = list(mod = list(doc = list(content = "# new")))
    )
  )
  ms$flushReact()

  expect_identical(isolate(content()), "# new")
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
      board_server_callback(
        board_rv,
        update = reactiveVal(),
        visible = reactiveVal()
      )

      expect_true("doc" %in% ls(peers))
      expect_identical(
        isolate(peers[["doc"]]$state$content()),
        "# hi"
      )
    }
  )
})

test_that("extension servers receive view_data, not the active dock (#264)", {

  captured <- NULL

  probe <- new_dock_extension(
    server = function(id, ...) {
      captured <<- list(...)
      moduleServer(id, function(input, output, session) list(state = list()))
    },
    ui = function(id) tagList(),
    name = "Probe",
    class = "probe_extension",
    ctor = function(...) NULL
  )

  board_rv <- board_args(
    blocks = c(a = new_dataset_block()),
    extensions = as_dock_extensions(list(probe))
  )

  with_mock_session(
    {
      board_server_callback(
        board_rv,
        update = reactiveVal(),
        visible = reactiveVal()
      )

      expect_true(is.reactive(captured[["view_data"]]))

      # `dock` (active_dock) is retired from the extension surface -- it is
      # internal now (#264).
      expect_false("dock" %in% names(captured))
    }
  )
})

test_that("New view modal confirm submits an add-and-activate delta", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(A = "a")
  )

  captured <- NULL

  testServer(
    function(input, output, session) {
      client_views <- reactiveVal(seed_view_state(board_views(brd)))
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
      expect_true(is_dock_view(captured$views$add[[1L]]))
    }
  )
})
