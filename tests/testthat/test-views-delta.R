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

  views <- board_layouts(out)
  expect_setequal(unname(view_names(views)), c("A", "NewView"))
  expect_identical(
    layout_panel_ids(views[[vid(views, "NewView")]]),
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

  expect_identical(unname(view_names(board_layouts(out))), "A")
  expect_identical(active_name(out), "A")
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

  views <- board_layouts(out)
  expect_setequal(
    layout_panel_ids(views[[vid(views, "A")]]),
    c("block_panel-a", "block_panel-b")
  )
  expect_identical(active_name(out), "A")
})

test_that("apply_views: full delta round-trips through board_layouts", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = list("a"), B = list("b"))
  )

  # add C and make it active (a new view has no id yet, so `active` names
  # it by its add key), mod A, rm B. mod / rm address existing views by id.
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

  views <- board_layouts(out)
  expect_setequal(unname(view_names(views)), c("A", "C"))
  expect_identical(active_name(out), "C")
  expect_setequal(
    layout_panel_ids(views[[vid(views, "A")]]),
    c("block_panel-a", "block_panel-b")
  )
})

test_that("apply_views_add adds a view, leaving the active one untouched", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = list("a"), B = list("b"))
  )
  expect_identical(active_view(board_layouts(brd)), "A")

  # Adding a view never changes which is active — that rides the `active`
  # slot, applied separately by `apply_views_active()`.
  out <- apply_views_add(
    setNames(list(dock_layout("a")), "cid"),
    brd
  )

  views <- board_layouts(out)
  expect_setequal(names(views), c("A", "B", "cid"))
  expect_identical(active_view(views), "A")
})

test_that("a delta can add a view and activate it by its add key", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = list("a"), B = list("b"))
  )

  upd <- augment_board_update(
    list(views = list(add = list(C = dock_layout("a")), active = "C")),
    brd
  )

  # normalize_views_delta resolves the `active` add key to the minted id
  # before validation / apply see it.
  expect_identical(upd$views$active, names(upd$views$add))
  expect_false(upd$views$active %in% c("A", "B"))

  out <- apply_board_update(brd, upd)

  expect_identical(active_name(out), "C")
  expect_identical(active_view(board_layouts(out)), upd$views$active)
})

test_that("apply_views_rename writes the display name, keeps the id", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(
      view_one = dock_layout("a", name = "Old"),
      view_two = list("a")
    )
  )

  out <- apply_views_rename(setNames(list("New"), "view_one"), brd)

  views <- board_layouts(out)
  expect_identical(names(views), c("view_one", "view_two"))
  expect_identical(view_name(views[["view_one"]]), "New")
})

test_that("rename flows through apply_board_update", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(v1 = dock_layout("a", name = "Old"), v2 = list("a"))
  )

  out <- apply_board_update(
    brd,
    list(views = list(rename = setNames(list("New"), "v1")))
  )

  expect_identical(view_name(board_layouts(out)[["v1"]]), "New")
})

test_that("augment_board_update is idempotent for a view add", {

  # The update lifecycle re-runs augment until it stops changing the
  # payload (preprocess_board_update). Minting a fresh id on every pass
  # never converges and loops the view-add update, so a second augment of
  # an already-augmented add must be a no-op.
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(Page = list("a"))
  )

  upd <- list(
    views = list(add = setNames(list(dock_layout("a")), "Second"),
                 active = "Second")
  )

  once <- augment_board_update(upd, brd)
  twice <- augment_board_update(once, brd)

  expect_identical(once, twice)
  expect_identical(view_name(once$views$add[[1L]]), "Second")
})

test_that("reconcile_views syncs the nav and live state on rename", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(v1 = dock_layout("a", name = "Old"), v2 = list("a"))
  )

  sent <- list()
  session <- list(
    ns = identity,
    sendInputMessage = function(input_id, message) {
      sent[[length(sent) + 1L]] <<- message
      invisible()
    },
    sendCustomMessage = function(type, message) invisible()
  )

  dock_mgr <- new_dock_manager()
  dock_mgr$current_active <- reactiveVal(active_view(board_layouts(brd)))
  dock_mgr$vs <- reactiveValues(state = board_layouts(brd))

  # The board carries the new name; the live state still has the old one, so
  # reconcile detects the rename and relabels the nav + vs$state.
  renamed <- apply_views_rename(setNames(list("New"), "v1"), brd)

  isolate(reconcile_views(renamed, dock_mgr, session))

  expect_true(
    any(lgl_ply(sent, function(m) {
      identical(m$rename$id, "v1") && identical(m$rename$to, "New")
    }))
  )
  expect_identical(view_name(isolate(dock_mgr$vs$state)[["v1"]]), "New")
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

  expect_setequal(
    names(res$views$mod),
    c(vid(brd, "Analysis"), vid(brd, "Overview"))
  )

  expect_setequal(
    layout_panel_ids(res$views$mod[[vid(brd, "Analysis")]]),
    c("block_panel-a", "block_panel-c")
  )
  expect_length(
    layout_panel_ids(res$views$mod[[vid(brd, "Overview")]]),
    0L
  )
  expect_null(res$views$mod[[vid(brd, "Other")]])
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

  expect_named(res$views$mod, vid(brd, "A"))
  expect_identical(res$views$rm, vid(brd, "B"))
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
    layout_panel_ids(res$views$mod[[vid(brd, "Analysis")]]),
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
    layout_panel_ids(augmented$views$mod[[vid(brd, "A")]]),
    c("block_panel-a", "block_panel-new1")
  )
})

test_that("add + rm of the same name replaces the view with a fresh id", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(A = list("a"), B = list("a"))
  )

  old_b <- vid(brd, "B")

  # With identity carried by id, removing B and adding a view also named
  # "B" is a legitimate replace, not a name clash: the new view gets a
  # fresh id.
  upd <- augment_board_update(
    list(
      views = list(
        add = list(B = dock_layout("a")),
        rm = "B"
      )
    ),
    brd
  )

  out <- apply_board_update(brd, upd)
  views <- board_layouts(out)

  expect_setequal(unname(view_names(views)), c("A", "B"))
  expect_false(old_b %in% names(views))
})

test_that("validate_views_delta rejects an id in both add and rm", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(A = list("a"), B = list("a"))
  )

  id <- vid(brd, "B")

  expect_error(
    validate_views_delta(
      list(add = setNames(list(dock_layout("a")), id), rm = id),
      brd,
      list()
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

test_that("validate_views_delta rejects an id in both mod and add", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(A = list("a"))
  )

  id <- vid(brd, "A")

  expect_error(
    validate_views_delta(
      list(
        add = setNames(list(dock_layout("a")), id),
        mod = setNames(list(dock_layout("a")), id)
      ),
      brd,
      list()
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

test_that("validate_views_delta rejects adding an existing id", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(A = list("a"))
  )

  id <- vid(brd, "A")

  expect_error(
    validate_views_delta(
      list(add = setNames(list(dock_layout("a")), id)),
      brd,
      list()
    ),
    class = "dock_views_delta_add_existing"
  )
})

test_that("validate_views_delta rejects an unknown rename id", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(A = list("a"))
  )

  expect_error(
    validate_views_delta(
      list(rename = list(ghost_id = "New")),
      brd,
      list()
    ),
    class = "dock_views_delta_rename_unknown"
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

test_that("drop_panels_from_layout resets the active tab when it is dropped", {

  ly <- dock_layout(panels("a", "b", active = "b"))

  res <- drop_panels_from_layout(ly, "b")
  leaf <- res[["grid"]][["root"]][["data"]][[1L]][["data"]]

  expect_identical(unlist(leaf[["views"]]), "a")
  expect_identical(leaf[["activeView"]], "a")
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
        layout_panel_ids(board_layouts(rv$board)[[vid(rv$board, "A")]]),
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

  expect_setequal(names(upd$views$mod), c(vid(brd, "A"), vid(brd, "B")))

  out <- apply_views_mod(upd$views$mod, brd)

  expect_identical(
    layout_panel_ids(board_layouts(out)[[vid(brd, "A")]]),
    "block_panel-a"
  )
  expect_length(
    layout_panel_ids(board_layouts(out)[[vid(brd, "B")]]),
    0L
  )
})

test_that("reconcile_views syncs the view_nav switcher on removal", {

  run_rm <- function(rm_label, active_label) {

    brd <- new_dock_board(
      blocks = c(a = new_dataset_block(), b = new_head_block()),
      layouts = list(A = list("a"), B = list("b"), C = list("a"))
    )

    state <- board_layouts(brd)
    name_to_id <- setNames(names(view_names(state)), unname(view_names(state)))

    sent <- list()
    session <- list(
      ns = identity,
      sendInputMessage = function(input_id, message) {
        sent[[length(sent) + 1L]] <<- message
        invisible()
      },
      sendCustomMessage = function(type, message) invisible()
    )

    dock_mgr <- new_dock_manager()
    dock_mgr$current_active <- reactiveVal(name_to_id[[active_label]])

    active_view(state) <- name_to_id[[active_label]]
    dock_mgr$vs <- reactiveValues(state = state)

    # Remove on the board (pure), then reconcile the live session against it.
    removed <- apply_views_rm(name_to_id[[rm_label]], brd)

    isolate(reconcile_views(removed, dock_mgr, session))

    list(sent = sent, ids = name_to_id)
  }

  # Removing a non-active view drops its tab; the active selection is
  # unchanged, so no `value` message is needed.
  non_active <- run_rm("B", "A")
  expect_true(
    any(lgl_ply(non_active$sent, function(m) {
      identical(m$remove, non_active$ids[["B"]])
    }))
  )

  # Removing the active view drops its tab and switches to a survivor.
  was_active <- run_rm("A", "A")
  expect_true(
    any(lgl_ply(was_active$sent, function(m) {
      identical(m$remove, was_active$ids[["A"]])
    }))
  )
  expect_true(
    any(lgl_ply(was_active$sent, function(m) {
      identical(m$value, was_active$ids[["B"]])
    }))
  )
})

test_that("apply_views_rm is a pure board transform", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = list("a"), B = list("b"))
  )

  expect_silent(out <- apply_views_rm(vid(brd, "B"), brd))
  expect_identical(unname(view_names(board_layouts(out))), "A")
})
