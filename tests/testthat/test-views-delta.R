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
    list(views = list(mod = list(A = c("block_panel-a", "block_panel-b")))),
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

test_that("a layout in views$mod is rejected at the update boundary", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = list("a"))
  )

  # Geometry belongs to the settled-echo mirror, not the membership lifecycle:
  # a layout smuggled into `mod` is refused before it can be applied.
  expect_error(
    validate_board_update(
      list(views = list(mod = list(A = dock_layout("a", "b")))),
      brd
    ),
    class = "dock_views_mod_geometry_rejected"
  )
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
        mod = list(A = c("block_panel-a", "block_panel-b")),
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

  # Pre-populate the registry so reconcile sees the views as already
  # instantiated (no DOM surgery). The proxy's membership matches the board and
  # the live layout is pending (NULL), so the layout check is a no-op and only
  # the rename fires.
  docks <- reactiveValues()
  for (id in names(board_layouts(brd))) {
    ids <- as.character(layout_panel_ids(board_layouts(brd)[[id]]))
    docks[[id]] <- list(
      layout = function() NULL,
      live_panels = reactiveVal(ids)
    )
  }
  active_dock <- reactiveValues()
  client_active <- reactiveVal(active_view(board_layouts(brd)))
  client_views <- reactiveVal(board_layouts(brd))

  # The board carries the new name; the live state still has the old one, so
  # reconcile detects the rename and relabels the nav + client_views.
  renamed <- apply_views_rename(setNames(list("New"), "v1"), brd)
  board <- reactiveValues(board = renamed)

  isolate(
    reconcile_views(board, function(...) NULL, docks, active_dock,
                    client_active, client_views, session)
  )

  expect_true(
    any(lgl_ply(sent, function(m) {
      identical(m$rename$id, "v1") && identical(m$rename$to, "New")
    }))
  )
  expect_identical(view_name(isolate(client_views())[["v1"]]), "New")
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
    res$views$mod[[vid(brd, "Analysis")]],
    c("block_panel-a", "block_panel-c")
  )
  expect_length(res$views$mod[[vid(brd, "Overview")]], 0L)
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

  upd <- list(
    blocks = list(rm = "b"),
    views = list(
      mod = list(
        Analysis = c("block_panel-a", "block_panel-b", "block_panel-c")
      )
    )
  )
  res <- augment_board_update(upd, brd)

  expect_setequal(
    res$views$mod[[vid(brd, "Analysis")]],
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
    views = list(mod = list(A = c("block_panel-a", "block_panel-new1")))
  )

  augmented <- augment_board_update(upd, brd)

  expect_setequal(
    augmented$views$mod[[vid(brd, "A")]],
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

test_that("dropping every panel leaves a round-trippable empty layout", {

  # A grid pruned to nothing must collapse to an empty branch, the shape a
  # constructed empty layout has -- not a NULL root, which the wire conversion
  # in canonicalize_grid() cannot express. Otherwise composing or restoring a
  # view whose grid is all ghosts against empty membership aborts.
  emptied <- drop_panels_from_layout(dock_layout("a", "b"), c("a", "b"))

  expect_no_error(canonicalize_grid(emptied))
  expect_identical(layout_to_spec(emptied), layout_to_spec(new_dock_layout()))
})

test_that("drop_panels_from_layout resets the active tab when it is dropped", {

  ly <- dock_layout(panels("a", "b", active = "b"))

  res <- drop_panels_from_layout(ly, "b")
  leaf <- res[["grid"]][["root"]][["data"]][[1L]][["data"]]

  expect_identical(unlist(leaf[["views"]]), "a")
  expect_identical(leaf[["activeView"]], "a")
})

test_that("fold_live_membership diffs membership against the live panel set", {

  members <- c("block_panel-a", "block_panel-b")

  added <- fold_live_membership(members, c(members, "block_panel-c"))
  expect_setequal(added, c(members, "block_panel-c"))

  dropped <- fold_live_membership(members, "block_panel-a")
  expect_setequal(dropped, "block_panel-a")

  # Already in sync (order-insensitive) -> NULL, so the caller skips a no-op.
  expect_null(fold_live_membership(members, rev(members)))
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
        list(views = list(mod = list(A = c("block_panel-a", "block_panel-b"))))
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

    # Registry pre-populated for every view (proxy membership matching the
    # board); the DOM helpers are mocked so the test exercises reconcile's
    # nav-sync, not the live teardown / switch.
    docks <- reactiveValues()
    for (id in names(state)) {
      ids <- as.character(layout_panel_ids(state[[id]]))
      docks[[id]] <- list(
        layout = function() NULL,
        live_panels = reactiveVal(ids)
      )
    }
    active_dock <- reactiveValues()
    client_active <- reactiveVal(name_to_id[[active_label]])

    active_view(state) <- name_to_id[[active_label]]
    client_views <- reactiveVal(state)

    # Remove on the board (pure), then reconcile the live session against it.
    removed <- apply_views_rm(name_to_id[[rm_label]], brd)
    board <- reactiveValues(board = removed)

    with_mocked_bindings(
      isolate(
        reconcile_views(board, function(...) NULL, docks, active_dock,
                        client_active, client_views, session)
      ),
      remove_view = function(view_id, session, docks) {
        trim_rv(docks, view_id)
        invisible()
      },
      hide_view_ui = function(...) NULL,
      show_view_ui = function(...) NULL,
      update_active_dock = function(...) NULL
    )

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

test_that("apply_views_relayout rewrites a view's membership and geometry", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = list("a"))
  )

  # A single-panel view is re-laid-out to a two-panel arrangement: the new
  # member folds into membership and the authored geometry lands in the grid
  # slot -- both slots written by one authored command.
  upd <- augment_board_update(
    list(
      views = list(
        relayout = list(A = dock_layout("a", "b", sizes = c(0.3, 0.7)))
      )
    ),
    brd
  )

  out <- apply_board_update(brd, upd)
  aid <- vid(brd, "A")

  expect_setequal(
    view_members(board_views(out)[[aid]]),
    c("block_panel-a", "block_panel-b")
  )
  expect_setequal(
    layout_panel_ids(board_grids(out)[[aid]]),
    c("block_panel-a", "block_panel-b")
  )
  expect_identical(active_name(out), "A")
})

test_that("relayout flows through apply_board_update on an existing view", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = list("a", "b"), B = list("b"))
  )

  # Reorder A to a single-panel {a}, addressing the view by id.
  out <- apply_board_update(
    brd,
    augment_board_update(
      list(
        views = list(relayout = setNames(list(dock_layout("a")), vid(brd, "A")))
      ),
      brd
    )
  )

  expect_identical(
    layout_panel_ids(board_layouts(out)[[vid(brd, "A")]]),
    "block_panel-a"
  )
  # A sibling view and the active marker are untouched.
  expect_identical(
    layout_panel_ids(board_layouts(out)[[vid(brd, "B")]]),
    "block_panel-b"
  )
  expect_identical(active_name(out), "A")
})

test_that("relayout preserves the view's display name, never renames", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(v1 = dock_layout("a", name = "Analysis"))
  )

  # Even a name carried on the fused layout is ignored: the display name lives
  # on the view record and only `rename` writes it.
  out <- apply_board_update(
    brd,
    augment_board_update(
      list(
        views = list(
          relayout = setNames(
            list(dock_layout("a", "b", name = "Ignored")), "v1"
          )
        )
      ),
      brd
    )
  )

  expect_identical(view_name(board_views(out)[["v1"]]), "Analysis")
})

test_that("augment resolves bare ids in a relayout layout and is idempotent", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = list("a"))
  )
  aid <- vid(brd, "A")

  once <- augment_board_update(
    list(views = list(relayout = setNames(list(dock_layout("a", "b")), aid))),
    brd
  )

  expect_setequal(
    layout_panel_ids(once$views$relayout[[aid]]),
    c("block_panel-a", "block_panel-b")
  )

  # The lifecycle re-runs augment to a fixed point, so a second pass over an
  # already-resolved relayout must be a no-op.
  twice <- augment_board_update(once, brd)
  expect_identical(once, twice)
})

test_that("relayout membership is validated against the board's blocks", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(A = list("a"))
  )

  expect_error(
    augment_board_update(
      list(
        views = list(
          relayout = setNames(list(dock_layout("ghost")), vid(brd, "A"))
        )
      ),
      brd
    ),
    class = "dock_views_delta_panel_ref_invalid"
  )
})

test_that("validate_views_delta rejects relayout on an unknown view", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(A = list("a"))
  )

  expect_error(
    augment_board_update(
      list(views = list(relayout = list(Ghost = dock_layout("a")))),
      brd
    ),
    class = "dock_views_delta_relayout_unknown"
  )
})

test_that("validate_views_delta rejects relayout clashing with rm/add/mod", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(A = list("a"), B = list("a"))
  )
  aid <- vid(brd, "A")

  expect_error(
    validate_views_delta(
      list(relayout = setNames(list(dock_layout("a")), aid), rm = aid),
      brd,
      list()
    ),
    class = "dock_views_delta_relayout_rm_clash"
  )

  expect_error(
    validate_views_delta(
      list(
        relayout = setNames(list(dock_layout("a")), "C"),
        add = setNames(list(dock_layout("a")), "C")
      ),
      brd,
      list()
    ),
    class = "dock_views_delta_relayout_add_clash"
  )

  expect_error(
    validate_views_delta(
      list(
        relayout = setNames(list(dock_layout("a")), aid),
        mod = setNames(list("block_panel-a"), aid)
      ),
      brd,
      list()
    ),
    class = "dock_views_delta_relayout_mod_clash"
  )
})

test_that("a serialize after a relayout reflects the new arrangement", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = list("a"))
  )

  out <- apply_board_update(
    brd,
    augment_board_update(
      list(
        views = list(
          relayout = list(A = dock_layout("a", "b", sizes = c(0.25, 0.75)))
        )
      ),
      brd
    )
  )

  des <- blockr_deser(blockr_ser(out))
  aid <- vid(des, "A")

  expect_setequal(
    layout_panel_ids(board_layouts(des)[[aid]]),
    c("block_panel-a", "block_panel-b")
  )
  # The authored two-pane split survives as a stored grid, not the default.
  expect_false(is.null(board_grids(des)[[aid]]))
})
