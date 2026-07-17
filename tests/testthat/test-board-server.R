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
        visibility = fake_visibility("a")
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
        visibility = fake_visibility("a")
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
    manage_dock("dock_main", board_rv_2, visibility = fake_visibility("a"),
                update = reactiveVal())
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
    manage_dock("dock_main", board_rv_2, visibility = fake_visibility("a"),
                update = reactiveVal())
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
    manage_dock("dock_main", board_rv_2, visibility = fake_visibility("a"),
                update = upd)
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
        visibility = fake_visibility(board_rv)
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
    manage_dock("dock_main", board_rv, visibility = fake_visibility(board_rv),
                update = upd)
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
    manage_dock("dock_main", board_rv, visibility = fake_visibility(board_rv),
                update = upd)
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

test_that("live_view_data uses a view's stored grid until it reports (#304)", {
  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(A = "a", B = "b")
  )

  res <- with_mock_context(ms, {
    board <- reactiveValues(board = brd)
    client_views <- reactiveVal(seed_view_state(board_views(brd)))
    ids <- names(client_views())
    layouts <- list(A = reactiveVal(NULL), B = reactiveVal(NULL))
    docks <- reactiveValues()
    docks[[ids[[1L]]]] <- list(layout = layouts$A)
    docks[[ids[[2L]]]] <- list(layout = layouts$B)
    client_active <- reactiveVal(ids[[1L]])
    list(
      vd = live_view_data(client_views, docks, board, client_active),
      layouts = layouts,
      ids = ids
    )
  })

  # Neither dock has reported a live layout, so every view falls back to its
  # board-stored grid: view_data is populated (no all-or-nothing block) rather
  # than NULL.
  vd0 <- isolate(res$vd())
  expect_named(vd0, c("views", "grids"))
  expect_s3_class(vd0$views, "dock_views")
  expect_identical(unname(view_names(vd0$views)), c("A", "B"))
  expect_identical(
    layout_panel_ids(vd0$grids[[res$ids[[1L]]]]), "block_panel-a"
  )
  expect_identical(
    layout_panel_ids(vd0$grids[[res$ids[[2L]]]]), "block_panel-b"
  )
  expect_identical(active_name(vd0$views), "A")

  # A view reporting a live layout upgrades to it; the other stays stored.
  g <- as_dock_grid(dock_grid("block_panel-b", "block_panel-a"))
  reported <- list(grid = grid_to_tree(g), activeGroup = "1")
  with_mock_context(ms, res$layouts$A(reported))

  vd1 <- isolate(res$vd())
  expect_identical(
    layout_panel_ids(vd1$grids[[res$ids[[1L]]]]),
    c("block_panel-b", "block_panel-a")
  )
  expect_identical(
    layout_panel_ids(vd1$grids[[res$ids[[2L]]]]), "block_panel-b"
  )
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

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(A = c("a", "b"))
  )

  res <- with_mock_context(ms, {
    board <- reactiveValues(board = brd)
    client_views <- reactiveVal(seed_view_state(board_views(brd)))
    docks <- reactiveValues()
    client_active <- reactiveVal(NULL)
    list(
      vd = live_view_data(client_views, docks, board, client_active),
      docks = docks,
      view = names(client_views())[[1L]]
    )
  })

  # First read with empty docks: the view falls back to its stored grid (seeded
  # order), and the absent-key read has subscribed.
  vd0 <- isolate(res$vd())
  expect_named(vd0, c("views", "grids"))
  expect_identical(
    layout_panel_ids(vd0$grids[[res$view]]),
    c("block_panel-a", "block_panel-b")
  )

  # Mimic reconcile creating the dock and the client reporting a reordered live
  # layout: the reactiveValues write re-triggers live_view_data, upgrading from
  # stored to the live order.
  g <- as_dock_grid(dock_grid("block_panel-b", "block_panel-a"))
  layout <- with_mock_context(
    ms, reactiveVal(list(grid = grid_to_tree(g), activeGroup = "1"))
  )
  with_mock_context(ms, res$docks[[res$view]] <- list(layout = layout))

  vd1 <- isolate(res$vd())
  expect_identical(
    layout_panel_ids(vd1$grids[[res$view]]),
    c("block_panel-b", "block_panel-a")
  )
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
      visibility = fake_visibility(board_rv)
    )
  )

  # Before reconcile runs the dock is not up, so view_data falls back to Page's
  # board-stored grid (seeded order) rather than blocking.
  vd0 <- isolate(res$view_data())
  expect_named(vd0, c("views", "grids"))
  expect_identical(
    layout_panel_ids(vd0$grids[["Page"]]),
    c("block_panel-a", "block_panel-b")
  )

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

test_that("reconcile builds only the active view's dock (#304)", {

  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(), b = new_head_block(), c = new_head_block()
    ),
    views = list(A = "a", B = "b", C = "c"),
    active = "B"
  )
  active_id <- active_view(board_views(brd))

  created <- character()
  flags <- list()

  # Stand in for create_view: record which views are built (and the no-flash
  # `active` stamp) without the DOM / module machinery.
  stub_create <- function(v_id, layout, board, update, session, docks,
                          visibility, ..., active = FALSE) {
    created <<- c(created, v_id)
    flags[[v_id]] <<- active
    docks[[v_id]] <- list(layout = function() NULL)
  }

  docks <- with_mock_context(ms, reactiveValues())
  client_views <- with_mock_context(
    ms, reactiveVal(seed_view_state(board_views(brd)))
  )
  client_active <- with_mock_context(ms, reactiveVal(NULL))
  active_dock <- with_mock_context(
    ms, reactiveValues(visibility = fake_visibility(board_block_ids(brd)))
  )
  update <- with_mock_context(ms, reactiveVal())
  session <- list(
    ns = identity,
    sendInputMessage = function(...) invisible(),
    sendCustomMessage = function(...) invisible()
  )

  reconcile <- function(board_obj) {
    with_mocked_bindings(
      with_mock_context(
        ms,
        reconcile_views(
          board_obj, update, docks, active_dock, client_active, client_views,
          session
        )
      ),
      create_view = stub_create,
      # Mimic the client acknowledging the switch so a later reconcile sees the
      # new active view.
      switch_active_view = function(active, ...) client_active(active),
      .package = "blockr.dock"
    )
  }

  board_obj <- with_mock_context(ms, reactiveValues(board = brd))
  reconcile(board_obj)

  # Only the active view's dock is built; the other two are deferred. It is
  # stamped active at insert (no view shown yet) so the first paint needs no
  # switch round-trip.
  expect_identical(created, active_id)
  expect_identical(with_mock_context(ms, names(docks)), active_id)
  expect_true(flags[[active_id]])

  # Visiting a deferred view (the board makes it active) builds its dock on
  # demand -- inactive, since switch_active_view activates it.
  visited <- setdiff(names(board_views(brd)), active_id)[[1L]]
  board_obj$board <- apply_views_active(visited, brd)
  reconcile(board_obj)

  expect_setequal(with_mock_context(ms, names(docks)), c(active_id, visited))
  expect_false(flags[[visited]])
})

test_that("visible axis follows the client's painted front tab (#328)", {

  # The visible slot -- the client-confirmed paint core's render gate waits for
  # -- rides the live `_state` layout echo, so it marks whichever tab dockView
  # actually fronts. The grid's stored active tab is not that source: a group's
  # front tab is client-owned (the last-added tab wins) and can disagree.
  local_mocked_bindings(
    restore_layout = function(...) invisible(),
    show_block_panel = function(...) invisible(),
    show_ext_panel = function(...) invisible(),
    .package = "blockr.dock"
  )

  # a and b tab together in one group; the stored grid fronts a.
  board_rv <- board_args(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    grids = list(
      Page = dock_grid(
        panels("block_panel-a", "block_panel-b", active = "block_panel-a")
      )
    )
  )
  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  vis <- with_mock_context(ms, fake_visibility(board_rv))
  with_mock_context(
    ms,
    board_server_callback(board_rv, update = reactiveVal(), visibility = vis)
  )
  ms$flushReact()

  page <- with_mock_context(ms, vid(board_rv$board, "Page"))

  # Before the client reports a painted layout, the server side alone marks
  # nothing on the visible axis.
  expect_true(is.na(isolate(vis$visible[["a"]]())))
  expect_true(is.na(isolate(vis$visible[["b"]]())))

  reported_front <- function(active) {
    grid <- as_dock_grid(
      dock_grid(panels("block_panel-a", "block_panel-b", active = active))
    )
    list(grid = grid_to_tree(grid), activeGroup = "1")
  }

  # The client paints b as the group's front tab -- disagreeing with the grid's
  # stored active (a). b is on screen, so b is the block that must render.
  do.call(
    ms$setInputs,
    set_names(list(reported_front("block_panel-b")), "Page-dock_state")
  )
  do.call(ms$setInputs, set_names(list(TRUE), "Page-dock_initialized"))
  ms$flushReact()

  expect_identical(isolate(vis$visible[["b"]]()), page)
  expect_identical(isolate(vis$required[["b"]]()), TRUE)
  expect_true(is.na(isolate(vis$visible[["a"]]())))
  expect_identical(isolate(vis$required[["a"]]()), FALSE)

  # Switching the front tab to a re-marks the visible axis -- the mark is live,
  # not a one-shot that leaves the newly fronted tab blank.
  do.call(
    ms$setInputs,
    set_names(list(reported_front("block_panel-a")), "Page-dock_state")
  )
  ms$flushReact()

  expect_identical(isolate(vis$visible[["a"]]()), page)
  expect_identical(isolate(vis$required[["a"]]()), TRUE)
  expect_true(is.na(isolate(vis$visible[["b"]]())))
  expect_identical(isolate(vis$required[["b"]]()), FALSE)
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

test_that("report_visible_observer drives the required axis over built cards", {
  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  env <- with_mock_context(ms, {
    layout_a <- reactiveVal(NULL)
    layout_b <- reactiveVal(NULL)

    docks <- new.env(parent = emptyenv())
    docks[["A"]] <- list(layout = layout_a)
    docks[["B"]] <- list(layout = layout_b)

    # a, b and d are all built (required non-NA); a/b are A's fronts, d lives
    # in B. Nothing reported on screen yet, so all parked (required FALSE).
    vis <- fake_visibility(c("a", "b", "d"))
    mark_cards_built(vis, c("a", "b", "d"))
    client_active <- reactiveVal("A")

    report_visible_observer(vis, client_active, docks)

    list(a = layout_a, b = layout_b, active = client_active, vis = vis)
  })

  ms$flushReact()
  # Before any layout report the built ledger stands; everything off screen.
  expect_setequal(built_cards(env$vis), c("a", "b", "d"))
  expect_identical(isolate(env$vis$required[["a"]]()), FALSE)

  # A shows a and b (fronts of two groups): required TRUE. d (in B) stays FALSE.
  with_mock_context(ms, env$a(
    dock_grid(panels("block_panel-a"), panels("block_panel-b"))
  ))
  ms$flushReact()
  expect_identical(isolate(env$vis$required[["a"]]()), TRUE)
  expect_identical(isolate(env$vis$required[["b"]]()), TRUE)
  expect_identical(isolate(env$vis$required[["d"]]()), FALSE)

  # Driving a and b required also paints them: report_visible marks the active
  # view's on-screen fronts on the visible axis off the same layout report.
  expect_identical(isolate(env$vis$visible[["a"]]()), "A")
  expect_identical(isolate(env$vis$visible[["b"]]()), "A")

  # A layout change on inactive B leaves A's required set alone, and must not
  # clear the paint of on-screen a/b (report_visible tracks only A's layout).
  with_mock_context(ms, env$b(dock_grid(panels("block_panel-d"))))
  ms$flushReact()
  expect_identical(isolate(env$vis$required[["a"]]()), TRUE)
  expect_identical(isolate(env$vis$visible[["a"]]()), "A")

  # A drops a (its slot is now an extension panel): a parks (required FALSE, its
  # paint cleared), b stays required TRUE with its paint intact.
  with_mock_context(ms, env$a(
    dock_grid(panels("ext_panel-editor"), panels("block_panel-b"))
  ))
  ms$flushReact()
  expect_identical(isolate(env$vis$required[["a"]]()), FALSE)
  expect_true(is.na(isolate(env$vis$visible[["a"]]())))
  expect_identical(isolate(env$vis$required[["b"]]()), TRUE)
  expect_identical(isolate(env$vis$visible[["b"]]()), "A")

  # Switching to the not-yet-arranged B: its block required, a/b off screen.
  with_mock_context(ms, env$active("B"))
  ms$flushReact()
  expect_identical(isolate(env$vis$required[["d"]]()), TRUE)
  expect_identical(isolate(env$vis$required[["a"]]()), FALSE)
  expect_identical(isolate(env$vis$required[["b"]]()), FALSE)
})

test_that("report_visible_observer coalesces set-equal reports", {
  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  env <- with_mock_context(ms, {
    layout <- reactiveVal(NULL)

    docks <- new.env(parent = emptyenv())
    docks[["A"]] <- list(layout = layout)

    # a and b are built, both A's fronts.
    vis <- fake_visibility(c("a", "b"))
    mark_cards_built(vis, c("a", "b"))
    client_active <- reactiveVal("A")

    report_visible_observer(vis, client_active, docks)

    list(layout = layout, vis = vis)
  })

  with_mock_context(ms, env$layout(
    dock_grid(panels("block_panel-a"), panels("block_panel-b"))
  ))
  ms$flushReact()

  expect_identical(isolate(env$vis$required[["a"]]()), TRUE)
  expect_identical(isolate(env$vis$required[["b"]]()), TRUE)

  # report_visible painted a and b as it drove them required.
  expect_identical(isolate(env$vis$visible[["a"]]()), "A")

  # Re-report a set-equal (reordered) layout: the observer re-runs (reactive()
  # does not dedupe), but the on-screen set is unchanged -- the required axis
  # stands, show_cards leaves an on-screen block's paint be, and the re-mark is
  # idempotent -- so the paint survives.
  with_mock_context(ms, env$layout(
    dock_grid(panels("block_panel-b"), panels("block_panel-a"))
  ))
  ms$flushReact()

  expect_identical(isolate(env$vis$required[["a"]]()), TRUE)
  expect_identical(isolate(env$vis$visible[["a"]]()), "A")
})

test_that("board_server_callback seeds visibility before the client reports", {

  board_rv <- board_args(
    blocks = c(a = new_dataset_block(), b = new_head_block())
  )

  with_mock_session({
    vis <- fake_visibility(board_rv)
    board_server_callback(board_rv, update = reactiveVal(), visibility = vis)

    # a is the fronted tab (required TRUE); b its back tab, built but off screen
    # (required FALSE). Both are built (on the required channel as the ledger).
    expect_setequal(built_cards(vis), c("a", "b"))
    expect_identical(isolate(vis$required[["a"]]()), TRUE)
    expect_identical(isolate(vis$required[["b"]]()), FALSE)
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
    vis <- fake_visibility(board_rv)
    board_server_callback(board_rv, update = reactiveVal(), visibility = vis)

    # b and d front their groups (required TRUE); a is b's back tab (FALSE). All
    # three are built (on the required channel as the ledger).
    expect_setequal(built_cards(vis), c("a", "b", "d"))
    expect_identical(isolate(vis$required[["a"]]()), FALSE)
    expect_identical(isolate(vis$required[["b"]]()), TRUE)
    expect_identical(isolate(vis$required[["d"]]()), TRUE)
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
    vis <- fake_visibility(board_rv)
    board_server_callback(board_rv, update = reactiveVal(), visibility = vis)

    expect_setequal(built_cards(vis), c("a", "b"))
    expect_identical(isolate(vis$required[["a"]]()), TRUE)
    expect_identical(isolate(vis$required[["b"]]()), TRUE)
  })
})

test_that("board_server_callback seeds cleanly on an empty board", {

  # No blocks -> an empty built set. The seed must not choke on the empty set,
  # else the board server aborts at session start and the app never becomes
  # stable -- the shinytest2 init hang this guards.
  board_rv <- board_args(extensions = new_edit_board_extension())

  with_mock_session({
    vis <- fake_visibility(board_rv)

    expect_no_error(
      board_server_callback(board_rv, update = reactiveVal(), visibility = vis)
    )
    expect_identical(built_cards(vis), character())
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
    board_server_callback(
      board_rv, update = upd, visibility = fake_visibility(board_rv)
    )
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
        visibility = fake_visibility(board_rv)
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
        visibility = fake_visibility(board_rv)
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

test_that("board_server_callback stashes served plugins on the dock (#331)", {

  board_rv <- board_args(blocks = c(a = new_dataset_block()))

  served <- custom_plugins(ctrl_block())(isolate(board_rv$board))

  with_mock_session(
    {
      res <- board_server_callback(
        board_rv,
        update = reactiveVal(),
        visibility = fake_visibility("a"),
        plugins = served
      )

      # The deferred card-build paths read the served set off this handle, so
      # its ctrl_block -- absent from board_plugins() -- must survive here.
      expect_true("ctrl_block" %in% names(res[["dock"]]$plugins))
      expect_identical(res[["dock"]]$plugins, served)
    }
  )
})

test_that("manage_dock carries the served plugins on its view dock (#331)", {

  board_rv <- board_args(blocks = c(a = new_dataset_block()))
  served <- custom_plugins(ctrl_block())(isolate(board_rv$board))

  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  res <- with_mock_context(ms, {
    manage_dock(
      "dock_main", board_rv, update = reactiveVal(),
      visibility = fake_visibility("a"), plugins = served
    )
  })

  ms$flushReact()

  expect_identical(res$plugins, served)
})

test_that("switch_active_view first-visit card uses served ctrl (#331)", {

  # The issue's repro: a block living only in an off-screen view has its card
  # built on first visit, by switch_active_view. It must build with the served
  # ctrl plugin (dropped by board_plugins()), so the control toggle is present.
  card <- NULL
  local_mocked_bindings(
    insertUI = function(selector, where, ui, ...) {
      card <<- c(card, list(as.character(ui)))
      invisible()
    },
    hide_view_ui = function(...) invisible(),
    show_view_ui = function(...) invisible(),
    update_active_dock = function(...) invisible()
  )

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block("iris"), b = new_dataset_block("mtcars")),
    grids  = list(one = dock_grid("a"), two = dock_grid("b"))
  )

  served <- custom_plugins(
    ctrl_block(ui = function(id, x) htmltools::span(class = "ctrl-sentinel"))
  )(brd)

  docks <- list(two = list(layout = function() NULL))
  active_dock <- list(
    visibility = fake_visibility(c("a", "b")),
    plugins = served
  )
  session <- list(
    ns = NS("board"),
    sendCustomMessage = function(...) invisible(),
    sendInputMessage = function(...) invisible()
  )

  switch_active_view(
    "two", docks, active_dock, reactiveVal("one"), brd, session
  )

  expect_match(
    paste(unlist(card), collapse = ""), "ctrl-sentinel", fixed = TRUE
  )
})
