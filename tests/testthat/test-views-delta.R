test_that("apply_views_add adds new view to board_layouts", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = list("a"))
  )

  upd <- augment_board_update(
    list(views = list(add = list(NewView = dock_layout("b")))),
    brd
  )

  out <- apply_board_update(brd, upd)

  expect_named(board_layouts(out), c("A", "NewView"))
  expect_identical(
    layout_panel_ids(board_layouts(out)$NewView),
    "block_panel-b"
  )
})

test_that("apply_views_rm removes a view from board_layouts", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = list("a"), B = list("b"))
  )

  upd <- augment_board_update(list(views = list(rm = "B")), brd)
  out <- apply_board_update(brd, upd)

  expect_named(board_layouts(out), "A")
  expect_identical(active_view(board_layouts(out)), "A")
})

test_that("apply_views_mod replaces a view's layout in board_layouts", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = list("a"))
  )

  upd <- augment_board_update(
    list(views = list(mod = list(A = dock_layout("a", "b")))),
    brd
  )

  out <- apply_board_update(brd, upd)

  expect_setequal(
    layout_panel_ids(board_layouts(out)$A),
    c("block_panel-a", "block_panel-b")
  )
  expect_identical(active_view(board_layouts(out)), "A")
})

test_that("apply_views: full delta round-trips through board_layouts", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = list("a"), B = list("b"))
  )

  upd <- augment_board_update(
    list(
      views = list(
        add = list(C = dock_layout("a")),
        mod = list(A = dock_layout("a", "b")),
        rm = "B",
        active = "C"
      )
    ),
    brd
  )

  out <- apply_board_update(brd, upd)

  expect_named(board_layouts(out), c("A", "C"))
  expect_identical(active_view(board_layouts(out)), "C")
  expect_setequal(
    layout_panel_ids(board_layouts(out)$A),
    c("block_panel-a", "block_panel-b")
  )
})

test_that("blocks$rm auto-augments views$mod for every affected view", {

  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(),
      b = new_head_block(),
      c = new_head_block()
    ),
    layouts = list(
      Analysis = list("a", "b", "c"),
      Overview = list("b"),
      Other = list("a", "c")
    )
  )

  upd <- list(blocks = list(rm = "b"))
  res <- augment_board_update(upd, brd)

  expect_setequal(names(res$views$mod), c("Analysis", "Overview"))

  expect_setequal(
    layout_panel_ids(res$views$mod$Analysis),
    c("block_panel-a", "block_panel-c")
  )
  expect_length(layout_panel_ids(res$views$mod$Overview), 0L)
  expect_null(res$views$mod$Other)
})

test_that("blocks$rm augment skips views in views$rm", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = list("a", "b"), B = list("b"))
  )

  upd <- list(
    blocks = list(rm = "b"),
    views = list(rm = "B")
  )
  res <- augment_board_update(upd, brd)

  expect_named(res$views$mod, "A")
  expect_identical(res$views$rm, "B")
})

test_that("augment merges user-submitted mod with block-removal cleanup", {

  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(),
      b = new_head_block(),
      c = new_head_block()
    ),
    layouts = list(Analysis = list("a", "b"))
  )

  user_mod_layout <- dock_layout("a", "b", "c")

  upd <- list(
    blocks = list(rm = "b"),
    views = list(mod = list(Analysis = user_mod_layout))
  )
  res <- augment_board_update(upd, brd)

  expect_setequal(
    layout_panel_ids(res$views$mod$Analysis),
    c("block_panel-a", "block_panel-c")
  )
})

test_that("blocks+views payload augments with refs to newly-added blocks", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(A = list("a"))
  )

  new_blk <- as_blocks(list(new1 = new_head_block()))
  upd <- list(
    blocks = list(add = new_blk),
    views = list(mod = list(A = dock_layout("a", "new1")))
  )

  augmented <- augment_board_update(upd, brd)

  expect_setequal(
    layout_panel_ids(augmented$views$mod$A),
    c("block_panel-a", "block_panel-new1")
  )
})

test_that("validate_views_delta rejects add/rm clash", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(A = list("a"), B = list("a"))
  )

  expect_error(
    augment_board_update(
      list(
        views = list(
          add = list(B = dock_layout("a")),
          rm = "B"
        )
      ),
      brd
    ),
    class = "dock_views_delta_add_rm_clash"
  )
})

test_that("validate_views_delta rejects mod referencing a name in rm", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(A = list("a"), B = list("a"))
  )

  expect_error(
    augment_board_update(
      list(
        views = list(
          mod = list(B = dock_layout("a")),
          rm = "B"
        )
      ),
      brd
    ),
    class = "dock_views_delta_mod_rm_clash"
  )
})

test_that("validate_views_delta rejects mod referencing a name in add", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(A = list("a"))
  )

  expect_error(
    augment_board_update(
      list(
        views = list(
          add = list(B = dock_layout("a")),
          mod = list(B = dock_layout("a"))
        )
      ),
      brd
    ),
    class = "dock_views_delta_mod_add_clash"
  )
})

test_that("validate_views_delta rejects active not resolving", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(A = list("a"))
  )

  expect_error(
    augment_board_update(
      list(views = list(active = "Nope")),
      brd
    ),
    class = "dock_views_delta_active_invalid"
  )

  expect_error(
    augment_board_update(
      list(
        views = list(
          rm = "A"
        )
      ),
      brd
    ),
    class = "dock_views_delta_remove_all"
  )
})

test_that("validate_views_delta rejects panel refs not in post-state", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(A = list("a"))
  )

  expect_error(
    augment_board_update(
      list(
        views = list(
          add = list(B = dock_layout("ghost"))
        )
      ),
      brd
    ),
    class = "dock_views_delta_panel_ref_invalid"
  )

  expect_error(
    augment_board_update(
      list(
        blocks = list(rm = "a"),
        views = list(
          add = list(B = dock_layout("a"))
        )
      ),
      brd
    ),
    class = "dock_views_delta_panel_ref_invalid"
  )
})

test_that("validate_views_delta rejects mod on unknown view", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(A = list("a"))
  )

  expect_error(
    augment_board_update(
      list(views = list(mod = list(Ghost = dock_layout("a")))),
      brd
    ),
    class = "dock_views_delta_mod_unknown"
  )

  expect_error(
    augment_board_update(
      list(views = list(rm = "Ghost")),
      brd
    ),
    class = "dock_views_delta_rm_unknown"
  )
})

test_that("validate_views_delta rejects add on existing view", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(A = list("a"))
  )

  expect_error(
    augment_board_update(
      list(views = list(add = list(A = dock_layout("a")))),
      brd
    ),
    class = "dock_views_delta_add_existing"
  )
})

test_that("validate_views_delta rejects unknown slice keys", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(A = list("a"))
  )

  expect_error(
    augment_board_update(
      list(views = list(bogus = list(A = dock_layout("a")))),
      brd
    ),
    class = "dock_views_delta_invalid"
  )
})

test_that("drop_panels_from_layout preserves remaining structure", {

  ly <- dock_layout("a", "b", "c", sizes = c(1, 2, 1))

  res <- drop_panels_from_layout(ly, "b")

  expect_setequal(layout_panel_ids(res), c("a", "c"))

  res_full <- drop_panels_from_layout(ly, c("a", "b", "c"))

  expect_length(layout_panel_ids(res_full), 0L)
})

test_that("empty views payload causes apply to be a no-op", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(A = list("a"))
  )

  out <- apply_board_update(brd, list())

  expect_identical(board_layouts(out), board_layouts(brd))
})

test_that("board_update lifecycle resets to NULL after views-only payload", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = list("a"), B = list("b"))
  )

  testServer(
    blockr.core:::board_server.board,
    {
      session$flushReact()

      board_update(
        list(views = list(mod = list(A = dock_layout("a", "b"))))
      )
      session$flushReact()

      expect_null(board_update())
      expect_setequal(
        layout_panel_ids(board_layouts(rv$board)$A),
        c("block_panel-a", "block_panel-b")
      )
    },
    args = list(
      x = brd,
      plugins = list(blockr.core::manage_blocks())
    )
  )
})

test_that("blocks$rm augment carries through to apply for view cleanup", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = list("a", "b"), B = list("b"))
  )

  upd <- augment_board_update(list(blocks = list(rm = "b")), brd)

  expect_named(upd$views$mod, c("A", "B"))

  out <- apply_views_mod(upd$views$mod, brd)

  expect_identical(
    layout_panel_ids(board_layouts(out)$A),
    "block_panel-a"
  )
  expect_length(
    layout_panel_ids(board_layouts(out)$B),
    0L
  )
})

test_that("apply_views_rm syncs the view_nav switcher on lifecycle removal", {

  run_rm <- function(rm_names, active) {

    brd <- new_dock_board(
      blocks = c(a = new_dataset_block(), b = new_head_block()),
      layouts = list(A = list("a"), B = list("b"), C = list("a"))
    )

    sent <- list()
    session <- list(
      ns = identity,
      sendInputMessage = function(input_id, message) {
        sent[[length(sent) + 1L]] <<- message
        invisible()
      }
    )

    dock_mgr <- new_dock_manager()
    dock_mgr$current_active <- reactiveVal(active)

    state <- dock_layouts(
      A = new_dock_layout(),
      B = new_dock_layout(),
      C = new_dock_layout()
    )
    active_view(state) <- active
    dock_mgr$vs <- reactiveValues(state = state)

    isolate(apply_views_rm(rm_names, brd, dock_mgr, session))

    sent
  }

  non_active <- run_rm("B", active = "A")
  expect_true(any(lgl_ply(non_active, function(m) identical(m$remove, "B"))))
  expect_true(any(lgl_ply(non_active, function(m) identical(m$value, "A"))))

  was_active <- run_rm("A", active = "A")
  expect_true(any(lgl_ply(was_active, function(m) identical(m$remove, "A"))))
  expect_true(any(lgl_ply(was_active, function(m) identical(m$value, "B"))))
})

test_that("apply_views_rm skips nav sync on the headless path", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = list("a"), B = list("b"))
  )

  expect_silent(out <- apply_views_rm("B", brd))
  expect_named(board_layouts(out), "A")
})
