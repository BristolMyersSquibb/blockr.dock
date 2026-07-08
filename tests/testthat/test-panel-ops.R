# The panel-op delivery layer (panel-ops.R) drives the same show / hide / select
# boundary the add-panel modal and tab gestures use, so these tests mock that
# boundary and assert the op fires the right dispatch with the right placement,
# is idempotent against the live panel set, and -- for a block being removed in
# the same update -- defers to core's block-removal path. The validation and
# reducer sides (validate_view_mod / apply_view_mod) are covered in
# test-views-delta.R alongside the wider delta.

# A dock stub carrying only what the ops read: an authoritative live-panel set
# the idempotency guards key on, and a placeholder proxy. The show / hide mocks
# keep `live_panels` in step so a decomposed move behaves like the real thing.
fake_dock <- function(live = character()) {
  list(proxy = "PROXY", board_ns = identity, layout = function() NULL,
       live_panels = shiny::reactiveVal(as.character(live)))
}

track_add <- function(dock, pid) {
  lp <- dock$live_panels
  lp(union(isolate(lp()), as.character(pid)))
}

track_rm <- function(dock, pid) {
  lp <- dock$live_panels
  lp(setdiff(isolate(lp()), as.character(pid)))
}

test_that("op_add_panel places a non-member at the hint, skips a member", {

  seen <- NULL

  local_mocked_bindings(
    show_block_panel = function(block, add_panel, dock, ...) {
      seen <<- list(block = block, pos = add_panel)
      track_add(dock, as_block_panel_id(block))
      invisible()
    },
    show_ext_panel = function(...) stop("ext path taken")
  )

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block())
  )
  dock <- fake_dock(live = "block_panel-a")

  op_add_panel(
    "block_panel-b", list(near = "block_panel-a", side = "right"), dock, brd
  )

  expect_identical(
    seen$pos,
    list(referencePanel = "block_panel-a", direction = "right")
  )
  expect_named(seen$block, "b")

  # A panel already live is not re-added: the fold's own capture echo of a
  # user gesture lands here as a no-op.
  seen <- NULL
  op_add_panel("block_panel-a", list(), dock, brd)
  expect_null(seen)
})

test_that("op_add_panel routes an extension panel and skips an unknown one", {

  seen <- NULL

  local_mocked_bindings(
    show_block_panel = function(...) stop("block path taken"),
    show_ext_panel = function(ext, add_panel, dock, ...) {
      seen <<- ext
      track_add(dock, as_ext_panel_id(ext))
      invisible()
    }
  )

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    extensions = new_edit_board_extension()
  )
  dock <- fake_dock()

  op_add_panel("ext_panel-edit_board_extension", list(), dock, brd)
  expect_true(is_dock_extension(seen))

  # A panel whose object is not on the board is skipped rather than throwing.
  seen <- NULL
  op_add_panel("block_panel-ghost", list(), dock, brd)
  expect_null(seen)
})

test_that("op_remove_panel removes a live panel, skips an absent one", {

  removed <- NULL

  local_mocked_bindings(
    hide_block_panel = function(id, rm_panel, dock, ...) {
      removed <<- list(id = id, rm_panel = rm_panel)
      track_rm(dock, id)
      invisible()
    },
    hide_ext_panel = function(...) stop("ext path taken")
  )

  dock <- fake_dock(live = "block_panel-a")

  op_remove_panel("block_panel-a", dock)
  expect_true(is_block_panel_id(removed$id))
  expect_true(removed$rm_panel)

  # Absent (e.g. a captured tab-close already gone from the dock) -> no-op.
  removed <- NULL
  op_remove_panel("block_panel-b", dock)
  expect_null(removed)
})

test_that("op_select_panel selects a live member, skips an absent one", {

  selected <- NULL

  local_mocked_bindings(
    select_block_panel = function(id, proxy) {
      selected <<- as.character(id)
      invisible()
    },
    select_ext_panel = function(...) stop("ext path taken")
  )

  dock <- fake_dock(live = "block_panel-a")

  op_select_panel("block_panel-a", dock)
  expect_identical(selected, "block_panel-a")

  selected <- NULL
  op_select_panel("block_panel-b", dock)
  expect_null(selected)
})

test_that("op_move_panel decomposes into remove + add-with-hint", {

  log <- character()

  local_mocked_bindings(
    hide_block_panel = function(id, rm_panel, dock, ...) {
      log <<- c(log, paste0("hide:", as.character(id)))
      track_rm(dock, id)
      invisible()
    },
    show_block_panel = function(block, add_panel, dock, ...) {
      pid <- as_block_panel_id(block)
      log <<- c(log, paste0("show:", as.character(pid), "@",
                            add_panel$referencePanel, "/", add_panel$direction))
      track_add(dock, pid)
      invisible()
    }
  )

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block())
  )
  dock <- fake_dock(live = c("block_panel-a", "block_panel-b"))

  op_move_panel(
    "block_panel-a", list(near = "block_panel-b", side = "right"), dock, brd
  )

  # Remove then re-add at the hint; membership is unchanged (still both live).
  expect_identical(
    log,
    c("hide:block_panel-a", "show:block_panel-a@block_panel-b/right")
  )
  expect_setequal(
    isolate(dock$live_panels()), c("block_panel-a", "block_panel-b")
  )
})

test_that("deliver_panel_ops applies rm -> add -> select, skipping block-rm", {

  log <- character()

  local_mocked_bindings(
    hide_block_panel = function(id, rm_panel, dock, ...) {
      log <<- c(log, paste0("rm:", as.character(id)))
      track_rm(dock, id)
      invisible()
    },
    show_block_panel = function(block, add_panel, dock, ...) {
      log <<- c(log, paste0("add:", as.character(as_block_panel_id(block))))
      track_add(dock, as_block_panel_id(block))
      invisible()
    },
    select_block_panel = function(id, proxy) {
      log <<- c(log, paste0("sel:", as.character(id)))
      invisible()
    }
  )

  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(), b = new_head_block(), c = new_head_block()
    )
  )
  dock <- fake_dock(live = c("block_panel-a", "block_panel-c"))

  deliver_panel_ops(
    list(
      rm = c("block_panel-a", "block_panel-b"),
      add = list(`block_panel-b` = list()),
      select = "block_panel-c"
    ),
    dock,
    brd,
    rm_blocks = "a"
  )

  # `block_panel-a` is the panel of a block removed in this update, so it is
  # left to core's block-removal path (delivering it too would double-remove).
  # `block_panel-b` is not live, so its rm is a no-op; then it is added and
  # `block_panel-c` selected.
  expect_identical(log, c("add:block_panel-b", "sel:block_panel-c"))
})

test_that("hint_to_position translates near / side, else the default spot", {

  local_mocked_bindings(
    determine_panel_pos = function(dock) list(direction = "right")
  )

  dock <- fake_dock()

  expect_identical(hint_to_position(NULL, dock), list(direction = "right"))
  expect_identical(hint_to_position(list(), dock), list(direction = "right"))

  expect_identical(
    hint_to_position(list(near = "block_panel-a", side = "below"), dock),
    list(referencePanel = "block_panel-a", direction = "below")
  )
  expect_identical(
    hint_to_position(list(near = "block_panel-a"), dock),
    list(referencePanel = "block_panel-a")
  )
  expect_identical(
    hint_to_position(list(side = "left"), dock),
    list(direction = "left")
  )
})

test_that("reveal_panel composes active + select for a holding view", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(one = "a", two = c("a", "b"))
  )

  # In the active view already -> stays active, just selects.
  a <- reveal_panel(brd, "a")
  expect_identical(a$views$active, "one")
  expect_identical(a$views$mod$one$select, "block_panel-a")

  # Only in another view -> switches to it and selects.
  b <- reveal_panel(brd, "b")
  expect_identical(b$views$active, "two")
  expect_identical(b$views$mod$two$select, "block_panel-b")

  # A canonical panel id passes through unchanged.
  expect_identical(reveal_panel(brd, "block_panel-a")$views$active, "one")

  # Nothing to reveal for a panel no view holds.
  brd2 <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(one = "a")
  )
  expect_null(reveal_panel(brd2, "b"))

  # An id that is neither panel, block nor extension is an error.
  expect_error(reveal_panel(brd, "nope"), class = "dock_reveal_panel_unknown")
})
