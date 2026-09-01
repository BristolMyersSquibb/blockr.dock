# The panel-op apply layer (panel-ops.R) drives the same show / hide / select
# boundary the add-panel modal and tab gestures use, so these tests mock that
# boundary and assert the op fires the right dispatch with the right placement,
# is idempotent against the live panel set, and -- for a block being removed in
# the same update -- defers to core's block-removal path. The validation and
# reducer sides (validate_view_mod / apply_view_mod) are covered in
# test-views-delta.R alongside the wider delta. Below the mocked ones, a browser
# pass drives the placement hints against a live dock, which is the only place
# the client's acceptance of what a hint names can be observed.

# A dock stub carrying only what the ops read: an authoritative live-panel set
# the idempotency guards key on, and a placeholder proxy. The show / hide mocks
# keep `live_panels` in step so the idempotency guards see the real membership.
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
    show_ext_panel = function(...) stop("ext path taken"),
    ensure_block_ui = function(...) NULL
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

  op_add_panel("ext_panel-edit_board", list(), dock, brd)
  expect_true(is_dock_extensions(seen))
  expect_identical(names(seen), "edit_board")

  # A panel whose object is not on the board is skipped rather than throwing.
  seen <- NULL
  op_add_panel("block_panel-ghost", list(), dock, brd)
  expect_null(seen)
})

test_that("op_add_panel builds an off-screen card with served ctrl (#331)", {

  # A block that lived only in an off-screen view has no card yet; op_add_panel
  # builds it before the show. It must build with the served ctrl plugin (which
  # board_plugins() drops), so the control toggle is present -- a served
  # ctrl_block whose UI drops a marker proves it rode through.
  card <- NULL
  local_mocked_bindings(
    insertUI = function(selector, where, ui, ...) {
      card <<- c(card, list(as.character(ui)))
      invisible()
    },
    show_block_panel = function(...) invisible()
  )

  brd <- new_dock_board(blocks = c(a = new_dataset_block("iris")))

  served <- custom_plugins(
    ctrl_block(ui = function(id, x) htmltools::span(class = "ctrl-sentinel"))
  )(brd)

  dock <- list(
    proxy = list(session = MockShinySession$new()),
    board_ns = NS("board"),
    live_panels = shiny::reactiveVal(character()),
    layout = function() NULL,
    prev_active_group = shiny::reactiveVal(NULL),
    visibility = fake_visibility("a"),
    plugins = served
  )

  op_add_panel("block_panel-a", list(), dock, brd)

  expect_match(
    paste(unlist(card), collapse = ""), "ctrl-sentinel", fixed = TRUE
  )
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

test_that("op_move_panel relocates a live panel to the hint in one step", {

  seen <- NULL

  local_mocked_bindings(
    move_dock_panel = function(id, position, proxy) {
      seen <<- list(id = as.character(id), position = position)
      invisible()
    }
  )

  dock <- fake_dock(live = c("block_panel-a", "block_panel-b"))

  op_move_panel(
    "block_panel-a", list(near = "block_panel-b", side = "right"), dock
  )

  # A first-class move: one dockview relocation to the hint, no remove / re-add,
  # and membership is unchanged (the panel stays live).
  expect_identical(seen$id, "block_panel-a")
  expect_identical(
    seen$position,
    list(referencePanel = "block_panel-b", direction = "right")
  )
  expect_setequal(
    isolate(dock$live_panels()), c("block_panel-a", "block_panel-b")
  )

  # A panel not live (a captured gesture already applied) -> no-op.
  seen <- NULL
  op_move_panel("block_panel-ghost", list(near = "block_panel-b"), dock)
  expect_null(seen)
})

test_that("op_resize_panel sizes a live panel, skips an absent one", {

  seen <- NULL

  local_mocked_bindings(
    resize_dock_panel = function(id, size, proxy) {
      seen <<- list(id = as.character(id), size = size)
      invisible()
    }
  )

  dock <- fake_dock(live = c("block_panel-a", "block_panel-b"))

  op_resize_panel("block_panel-a", list(size = 0.3), dock)
  expect_identical(seen$id, "block_panel-a")
  expect_identical(seen$size, 0.3)

  # A panel not live -> no-op.
  seen <- NULL
  op_resize_panel("block_panel-ghost", list(size = 0.3), dock)
  expect_null(seen)
})

test_that("op_add_panel on an inactive view places the wrapper, not the card", {

  # The card is a single board-level element shown in the active view; moving it
  # into an inactive dock would yank it off-screen, so an inactive add touches
  # the dockview wrapper only.
  seen <- NULL

  local_mocked_bindings(
    show_block_panel = function(...) stop("card moved into an inactive view"),
    add_block_panel = function(block, position, dock, ...) {
      seen <<- list(block = block, pos = position)
      track_add(dock, as_block_panel_id(block))
      invisible()
    }
  )

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block())
  )
  dock <- fake_dock(live = "block_panel-a")

  op_add_panel(
    "block_panel-b", list(near = "block_panel-a", side = "right"), dock, brd,
    active = FALSE
  )

  expect_named(seen$block, "b")
  expect_identical(
    seen$pos, list(referencePanel = "block_panel-a", direction = "right")
  )
})

test_that("op_remove_panel on an inactive view removes the wrapper only", {

  seen <- NULL

  local_mocked_bindings(
    hide_block_panel = function(...) stop("card parked from an inactive view"),
    remove_block_panel = function(id, dock) {
      seen <<- as.character(id)
      track_rm(dock, id)
      invisible()
    }
  )

  dock <- fake_dock(live = "block_panel-a")
  op_remove_panel("block_panel-a", dock, active = FALSE)

  expect_identical(seen, "block_panel-a")
})

test_that("the cascade rm delivers to inactive docks, skips the active", {

  # Core's block-removal cleans only the active dock, so an inactive dock must
  # deliver the cascade `rm` to clear the wrapper core never reaches; the active
  # dock skips it to avoid a double-remove.
  log <- character()

  local_mocked_bindings(
    remove_block_panel = function(id, dock) {
      log <<- c(log, paste0("rm:", as.character(id)))
      track_rm(dock, id)
      invisible()
    },
    hide_block_panel = function(...) stop("active dock double-removed")
  )

  brd <- new_dock_board(blocks = c(a = new_dataset_block()))
  mod <- list(rm = "block_panel-a")

  active <- fake_dock(live = "block_panel-a")
  apply_panel_ops(mod, active, brd, rm_blocks = "a", active = TRUE)

  inactive <- fake_dock(live = "block_panel-a")
  apply_panel_ops(mod, inactive, brd, rm_blocks = "a", active = FALSE)

  expect_identical(log, "rm:block_panel-a")
})

test_that("apply_panel_ops applies rm -> add -> select, skipping block-rm", {

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
    },
    ensure_block_ui = function(...) NULL
  )

  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(), b = new_head_block(), c = new_head_block()
    )
  )
  dock <- fake_dock(live = c("block_panel-a", "block_panel-c"))

  apply_panel_ops(
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

test_that("a rail hint names the edge group rather than a member panel", {

  local_mocked_bindings(
    determine_panel_pos = function(dock) list(direction = "right")
  )

  dock <- fake_dock()

  # `rail_group_id()` mints the stable id, so the position names the rail
  # without having to resolve it through a panel already sitting there -- which
  # is what lets the first panel into an empty one.
  expect_identical(
    hint_to_position(list(rail = "left"), dock),
    list(referenceGroup = "rail-left", direction = "within")
  )
  expect_identical(
    hint_to_position(list(rail = "right"), dock),
    list(referenceGroup = "rail-right", direction = "within")
  )
})

test_that("a narrow view takes an add as a tab, ignoring the side hint", {

  local_mocked_bindings(
    determine_panel_pos = function(dock) {
      list(referenceGroup = "grp1", direction = "within")
    }
  )

  dock <- fake_dock()
  dock[["narrow"]] <- TRUE

  expect_identical(
    hint_to_position(list(near = "block_panel-a", side = "below"), dock),
    list(referenceGroup = "grp1", direction = "within")
  )
  expect_identical(
    hint_to_position(list(side = "left"), dock),
    list(referenceGroup = "grp1", direction = "within")
  )

  # A narrow render folds each rail into the stack, so there is no edge group
  # to name and the hint falls through with the rest.
  expect_identical(
    hint_to_position(list(rail = "left"), dock),
    list(referenceGroup = "grp1", direction = "within")
  )
})

test_that("op_move_panel routes a rail hint to the edge group", {

  seen <- NULL

  local_mocked_bindings(
    move_dock_panel = function(pid, position, proxy) {
      seen <<- list(pid = as.character(pid), pos = position)
      invisible()
    }
  )

  dock <- fake_dock(live = "block_panel-a")

  op_move_panel("block_panel-a", list(rail = "right"), dock)

  expect_identical(
    seen,
    list(
      pid = "block_panel-a",
      pos = list(referenceGroup = "rail-right", direction = "within")
    )
  )
})

# ---- end to end -------------------------------------------------------------

# Mocking the proxy stops one hop short. It says which dispatch a hint produces,
# never whether dockView accepts what that dispatch names, nor whether the
# result comes back through the grid mirror onto the board. The `rail` hint most
# wants the difference -- it targets a dockView edge group rather than a
# splitview position, and whether the client accepts that target was the open
# question in #461 -- and one fixture carries `near` / `side` / `size` alongside
# it, so a single browser pass retires the gap for the whole grammar. Without
# one, a dockViewR bump reintroducing the edge-group refusal lands silently.
#
# No UI gesture emits a server-side `move` or `resize`, and the add-panel modal
# only ever emits an `add` anchored `within` the group clicked, so nothing in a
# dock can drive the rest of the grammar by being clicked. The `panel-ops`
# fixture supplies what can: an extension emitting one `views$mod` payload per
# button, which also exports the board's own committed grid. Read in place of
# the client echo, that export is what separates a panel that merely landed from
# one that persisted.

ops_dock <- "my_board-main-dock"

ops_app <- function(name) {

  app <- new_app_driver(
    system.file("examples", "panel-ops", "app.R", package = "blockr.dock"),
    name = name,
    seed = 42,
    load_timeout = 60 * 1000,
    timeout = 30 * 1000
  )

  # Blocks `c` and `d` sit outside the view so that an `add` has something to
  # place, which leaves only `a` and `b` carrying a card at load.
  wait_dock_loaded(app, n_blocks = 2)

  # The first thing asserted is that both rails are empty and hidden, and a
  # dock that has not mounted yet reads exactly that way -- no widget to ask,
  # no echoed state to find rails in. Gating on the client-confirmed tab set is
  # what keeps that precondition, and so every step resting on it, non-vacuous.
  wait_block_panel_tabs(app, c("block_panel-a", "block_panel-b"))

  app
}

# Fire and return. A payload that only mutates the dock updates no output
# value, so `click()`'s default wait has nothing to gate on and would spend its
# whole budget before timing out; `expect_settles()` below is the real gate.
ops_click <- function(app, btn) {
  app$click(paste0("my_board-ext_ops-", btn), wait_ = FALSE)
}

# An apply reaches the dock, the dock settles, and only then does the echo come
# back for the mirror to commit -- a round trip the app can look idle in the
# middle of, so a fixed wait either flakes or is slower than it has to be. Poll
# what the step is about instead, then assert it, so that a timeout surfaces as
# the mismatch itself: what was wanted against whatever the dock settled at.
expect_settles <- function(read, want, label, timeout = 30 * 1000) {

  deadline <- Sys.time() + timeout / 1000

  repeat {

    got <- read()

    if (identical(got, want) || Sys.time() > deadline) {
      break
    }

    Sys.sleep(0.2)
  }

  expect_identical(got, want, label = label)
}

# What the client makes of a rail: the panels off the echoed dock state, the
# visibility off the widget, which is where the derived rule (a rail holding
# panels is shown, an empty one hidden) actually runs. Optional-chained so the
# read yields NULL rather than aborting while the widget is still null.
rail_state <- function(app, edge) {

  rails <- as_dock_rails(
    new_dock_layout(app$get_value(input = paste0(ops_dock, "_state")))
  )

  visible <- app$get_js(
    sprintf(
      "HTMLWidgets.find('#%s')?.getWidget()?.isEdgeGroupVisible('%s')",
      ops_dock, edge
    )
  )

  list(
    panels = as.character(rails[[edge]][["panels"]]),
    visible = isTRUE(visible)
  )
}

# What the board committed for one edge. An emptied rail may keep its entry
# (the client echoed it a width, so it is no longer the default one
# canonicalisation drops) or lose it, and both spell the same membership --
# so read the panels rather than the presence of the entry.
stored_rail <- function(app, edge) {
  as.character(app$get_value(export = "my_board-ext_ops-stored_rails")[[edge]])
}

stored_grid <- function(app) {
  app$get_value(export = "my_board-ext_ops-stored_grid")
}

# The stored tree, one root child at a time, each as the panels it holds. The
# fixture never nests deeper than a leaf below the root, and the grouping is
# the whole point of a `side` hint -- a flattened id list reads the same
# whether a panel joined a group or split it.
root_groups <- function(app) {
  lapply(
    stored_grid(app)[["children"]],
    function(node) {
      if (is_grid_leaf(node)) node[["panels"]] else grid_tree_ids(node)
    }
  )
}

test_that("a rail hint parks a panel on an edge and takes it back off", {

  skip_on_cran()

  app <- ops_app("panel-ops-rail")
  withr::defer(app$stop())

  # Born empty and hidden on both edges: a rail's visibility is derived from
  # what it holds, so the fixture declares neither and the first payload is
  # what reveals one. A rail already showing would make the step below vacuous.
  expect_settles(
    function() rail_state(app, "right"),
    list(panels = character(), visible = FALSE),
    "the right rail at load"
  )
  expect_identical(stored_rail(app, "right"), character())

  # A `move` carrying `rail` is the hop #461 left open: it routes through
  # dockView's `moveTo`, and its target here is an empty rail, which the
  # derived rule leaves hidden.
  ops_click(app, "rail_move_b")
  expect_settles(
    function() rail_state(app, "right"),
    list(panels = "block_panel-b", visible = TRUE),
    "b moved onto the right edge"
  )
  expect_settles(
    function() stored_rail(app, "right"), "block_panel-b",
    "the board recording b on the right edge"
  )

  # An `add` carrying `rail` places a non-member there. Block `c` is on the
  # board but outside the view, so it arrives by the `addPanel` route rather
  # than by `moveTo`, and its card is built on the way in.
  ops_click(app, "rail_add_c")
  expect_settles(
    function() rail_state(app, "right"),
    list(panels = c("block_panel-b", "block_panel-c"), visible = TRUE),
    "c added to the right edge"
  )
  expect_settles(
    function() stored_rail(app, "right"),
    c("block_panel-b", "block_panel-c"),
    "the board recording both on the right edge"
  )

  # Out again, one at a time. A rail still holding a panel stays up.
  ops_click(app, "grid_move_b")
  expect_settles(
    function() rail_state(app, "right"),
    list(panels = "block_panel-c", visible = TRUE),
    "b moved back into the tree"
  )
  expect_settles(
    function() stored_rail(app, "right"), "block_panel-c",
    "the board dropping b from the right edge"
  )

  # Emptying it hides it again, by the same derived rule the fixture was born
  # under, now driven from the server rather than by a drag.
  ops_click(app, "grid_move_c")
  expect_settles(
    function() rail_state(app, "right"),
    list(panels = character(), visible = FALSE),
    "the right rail emptied"
  )
  expect_settles(
    function() stored_rail(app, "right"), character(),
    "the board recording an empty right edge"
  )

  # The hint named an edge, and only that edge moved. Every board offers both,
  # so a left rail that stayed empty and hidden throughout is what says the
  # payload picked its target rather than any rail at all.
  expect_identical(stored_rail(app, "left"), character())
  expect_identical(
    rail_state(app, "left"), list(panels = character(), visible = FALSE)
  )

  # Nothing was lost on the way out: a rail and the tree partition the panels a
  # grid places, so both moved panels are back in the tree and placed once.
  expect_setequal(
    grid_tree_ids(stored_grid(app)),
    c("ext_panel-ops", "block_panel-a", "block_panel-b", "block_panel-c")
  )

  # No commit loop: the payload path settles rather than re-committing what it
  # has just echoed.
  expect_true(isTRUE(app$get_value(export = "roundtrip_stable")))
})

test_that("the near, side and size hints place and scale inside the tree", {

  skip_on_cran()

  app <- ops_app("panel-ops-tree")
  withr::defer(app$stop())

  # The authored grid: the emitter, then a tab group holding a and b.
  expect_settles(
    function() root_groups(app),
    list("ext_panel-ops", c("block_panel-a", "block_panel-b")),
    "the authored root"
  )
  expect_settles(
    function() round(stored_grid(app)[["sizes"]], 3), c(0.35, 0.65),
    "the authored sizes"
  )

  # Together `near` and `side` split the group the anchor sits in rather than
  # joining it, so `d` becomes a sibling to the right of the group holding `a`
  # where a `within` would have made it another of that group's tabs.
  ops_click(app, "grid_add_d")
  expect_settles(
    function() root_groups(app),
    list(
      "ext_panel-ops", c("block_panel-a", "block_panel-b"), "block_panel-d"
    ),
    "d added right of a"
  )

  # The `size` hint is a ratio of the split its panel's group sits in, and the
  # `resize` verb is what consumes it -- on `add` it is only recorded. That
  # group lands on the requested 0.3, the rest share what is left, and the
  # board stores the result, so a reload comes back to it.
  ops_click(app, "grid_resize_a")
  expect_settles(
    function() round(stored_grid(app)[["sizes"]], 3), c(0.35, 0.3, 0.35),
    "a's group resized to 0.3"
  )

  expect_true(isTRUE(app$get_value(export = "roundtrip_stable")))
})
