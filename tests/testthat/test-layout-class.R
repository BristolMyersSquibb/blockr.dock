test_that("an invalid grid node reports a legible error", {
  expect_error(
    dock_grid("a", active = TRUE),
    "Unknown layout node type",
    class = "dock_layout_node_invalid"
  )
})

test_that("grid resolution accepts a bare dock_extension", {

  blks <- c(a = new_dataset_block(), b = new_head_block())
  ext <- new_edit_board_extension()

  brd <- new_dock_board(blocks = blks, extensions = ext)

  expect_setequal(
    view_members(board_views(brd)[[1L]]),
    c("block_panel-a", "block_panel-b", "ext_panel-edit_board")
  )

  brd2 <- new_dock_board(
    blocks = blks,
    extensions = ext,
    views = list(c("edit_board", "a", "b"))
  )
  expect_setequal(
    view_members(board_views(brd2)[[1L]]),
    c("block_panel-a", "block_panel-b", "ext_panel-edit_board")
  )
})

test_that("grid resolution accepts a named list of extensions", {

  blks <- c(a = new_dataset_block(), b = new_head_block())
  exts <- list(edit = new_edit_board_extension())

  brd <- new_dock_board(blocks = blks, extensions = exts)

  expect_setequal(
    view_members(board_views(brd)[[1L]]),
    c("block_panel-a", "block_panel-b", "ext_panel-edit")
  )

  brd2 <- new_dock_board(
    blocks = blks,
    extensions = exts,
    views = list(c("edit", "a", "b"))
  )
  expect_setequal(
    view_members(board_views(brd2)[[1L]]),
    c("block_panel-a", "block_panel-b", "ext_panel-edit")
  )
})

test_that("a keyed extension is addressable in a view by its list key", {

  blks <- c(a = new_dataset_block(), b = new_head_block())
  exts <- list(edit = new_edit_board_extension())

  brd <- new_dock_board(
    blocks = blks,
    extensions = exts,
    views = list(c("edit", "a", "b"))
  )
  expect_setequal(
    view_members(board_views(brd)[[1L]]),
    c("block_panel-a", "block_panel-b", "ext_panel-edit")
  )
})

test_that("grid resolution accepts a dock_extensions collection", {

  blks <- c(a = new_dataset_block(), b = new_head_block())
  exts <- new_dock_extensions(list(new_edit_board_extension()))

  brd <- new_dock_board(blocks = blks, extensions = exts)

  expect_setequal(
    view_members(board_views(brd)[[1L]]),
    c("block_panel-a", "block_panel-b", "ext_panel-edit_board")
  )
})

test_that("default_layout uses class-name convention across input forms", {

  blks <- c(a = new_dataset_block())
  expected <- dock_grid(
    "block_panel-a", rail(ext("edit_board"), position = "left")
  )

  layout_of <- function(x) default_layout(blks, x)
  grid_of <- function(x) layout_of(x)[["grids"]][[1L]]
  rails_of <- function(x) rail_panel_ids(grid_of(x)[["rails"]])

  expect_identical(grid_of(new_edit_board_extension()), expected)
  expect_identical(grid_of(list(new_edit_board_extension())), expected)
  expect_identical(
    grid_of(new_dock_extensions(list(new_edit_board_extension()))),
    expected
  )

  # The class-name convention names the rail's panel just the same.
  expect_identical(rails_of(new_edit_board_extension()), "ext_panel-edit_board")
  expect_identical(
    rails_of(list(new_edit_board_extension())), "ext_panel-edit_board"
  )
  expect_identical(
    rails_of(new_dock_extensions(list(new_edit_board_extension()))),
    "ext_panel-edit_board"
  )
})

test_that("new_dock_board(active=) selects the view", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(First = "a", Second = c("a", "b")),
    active = "Second"
  )

  expect_identical(active_name(brd), "Second")
  expect_length(view_members(board_views(brd)[["Second"]]), 2L)
})

test_that("geometry is stored without panel content", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(A = c("a", "b"), B = c("a", "b")),
    grids = list(A = dock_grid("a", "b"), B = dock_grid("a", "b"))
  )

  a <- board_grids(brd)[["A"]]
  b <- board_grids(brd)[["B"]]

  expect_named(a, c("orientation", "children", "sizes"))
  expect_false("panels" %in% names(a))
  expect_false("panels" %in% names(b))
})

test_that("as_dock_layout materialises grid + panels on demand", {

  blks <- c(a = new_dataset_block(), b = new_head_block())
  resolved <- resolve_grid(dock_grid("a", "b"), panel_id_map(blks, list()))

  payload <- as_dock_layout(resolved, blks, list())

  expect_s3_class(payload, "dock_layout")
  expect_named(payload, c("grid", "panels", "activeGroup"))
  expect_named(payload$panels, c("block_panel-a", "block_panel-b"))
  expect_identical(payload$panels[["block_panel-a"]][["title"]], "Dataset")
})

test_that("resolve_grid resolves bare ids mixed with a pre-resolved ref", {

  blks <- c(a = new_dataset_block(), b = new_head_block())
  exts <- list(edit = new_edit_board_extension())
  id_map <- panel_id_map(blks, exts)

  mixed <- resolve_grid(dock_grid("a", "b", ext("edit")), id_map)

  expect_identical(
    layout_panel_ids(mixed),
    c("block_panel-a", "block_panel-b", "ext_panel-edit")
  )

  expect_identical(mixed, resolve_grid(dock_grid("a", "b", "edit"), id_map))
  expect_identical(
    mixed,
    resolve_grid(dock_grid(blk("a"), blk("b"), ext("edit")), id_map)
  )
})

test_that("a grid mixing bare ids with a ref keeps every member on the board", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    extensions = list(edit = new_edit_board_extension()),
    views = list(Setup = c("a", "b", "edit")),
    grids = list(Setup = dock_grid("a", "b", ext("edit")))
  )

  expect_setequal(
    layout_panel_ids(board_grids(brd)[["Setup"]]),
    c("block_panel-a", "block_panel-b", "ext_panel-edit")
  )
})

test_that("sizes propagate to grid children and nested branches", {

  grid <- dock_grid("a", "b", sizes = c(0.3, 0.7))
  expect_equal(grid$sizes, c(0.3, 0.7))

  grid2 <- dock_grid(
    "a",
    group("b", "c", sizes = c(0.4, 0.6)),
    sizes = c(0.3, 0.7)
  )
  expect_equal(grid2$sizes, c(0.3, 0.7))
  expect_equal(grid2$children[[2L]]$sizes, c(0.4, 0.6))
})

test_that("panels() sets the active tab in a tabbed leaf", {

  grid <- dock_grid(panels("a", "b", "c", active = "b"))
  leaf <- grid$children[[1L]]
  expect_identical(leaf$active, "b")
  expect_identical(leaf$panels, c("a", "b", "c"))
})

test_that("panels() with single id is allowed but redundant", {

  expect_silent(panels("a"))
  grid <- dock_grid(panels("a"))
  expect_identical(layout_panel_ids(grid), "a")
})

test_that("orientation flips the top-level grid", {

  grid <- dock_grid("a", "b", orientation = "vertical")
  expect_identical(grid$orientation, "vertical")
})

test_that("dock_grid rejects invalid sizes vector", {

  expect_error(
    dock_grid("a", "b", sizes = c(0.5)),
    class = "dock_layout_sizes_invalid"
  )
  expect_error(
    dock_grid("a", "b", sizes = c(-0.5, 1.5)),
    class = "dock_layout_sizes_invalid"
  )
})

test_that("panels() rejects active that isn't one of the IDs", {

  expect_error(
    panels("a", "b", active = "c"),
    class = "dock_panels_active_invalid"
  )
})

test_that("lock_panels flips tabComponent both ways (#124)", {

  blks <- c(a = new_dataset_block())

  # as_dock_layout reads is_dock_locked() when building panels, so the
  # `tabComponent` it emits already reflects the option in effect at call
  # time. lock_panels is the explicit re-coercion applied at restore time
  # to bring saved panels in line with the *current* lock state.
  unlocked_payload <- withr::with_options(
    list(blockr.locked = NULL),
    as_dock_layout(dock_grid("a"), blocks = blks)
  )

  saved_tabs <- vapply(
    unlocked_payload[["panels"]], `[[`, character(1L), "tabComponent"
  )
  expect_true(all(saved_tabs == "manual"))

  locked_view <- lock_panels(unlocked_payload, locked = TRUE)
  restored_tabs <- vapply(
    locked_view[["panels"]], `[[`, character(1L), "tabComponent"
  )
  expect_true(all(restored_tabs == "custom"))
  expect_true(
    all(vapply(
      locked_view[["panels"]],
      function(p) is.null(p[["params"]][["removeCallback"]]),
      logical(1L)
    ))
  )

  unlocked_view <- lock_panels(locked_view, locked = FALSE)
  expect_true(all(
    vapply(unlocked_view[["panels"]], `[[`, character(1L), "tabComponent") ==
      "manual"
  ))
  expect_true(all(vapply(
    unlocked_view[["panels"]],
    function(p) {
      rc <- p[["params"]][["removeCallback"]]
      isTRUE(rc[["__IS_FUNCTION__"]]) &&
        grepl("panel-to-remove", rc[["source"]], fixed = TRUE)
    },
    logical(1L)
  )))
})

test_that("lock_panels restores callback on save-locked -> restore-unlocked", {

  blks <- c(a = new_dataset_block())

  locked_payload <- withr::with_options(
    list(blockr.locked = TRUE),
    as_dock_layout(dock_grid("a"), blocks = blks)
  )

  saved_callbacks <- lapply(
    locked_payload[["panels"]],
    function(p) p[["params"]][["removeCallback"]]
  )
  expect_true(all(vapply(saved_callbacks, is.null, logical(1L))))

  unlocked_view <- lock_panels(locked_payload, locked = FALSE)
  for (p in unlocked_view[["panels"]]) {
    expect_identical(p[["tabComponent"]], "manual")
    rc <- p[["params"]][["removeCallback"]]
    expect_true(isTRUE(rc[["__IS_FUNCTION__"]]))
    expect_match(rc[["source"]], "panel-to-remove", fixed = TRUE)
  }
})

test_that("dock_grid has a tree-style print method", {

  expect_snapshot(print(dock_grid("a", "b")))

  expect_snapshot(
    print(
      dock_grid(
        group("data", "filt", "head"), "assistant_extension",
        sizes = c(0.6, 0.4)
      )
    )
  )

  expect_snapshot(
    print(dock_grid("a", panels("b", "c", "edit", active = "c")))
  )

  expect_snapshot(
    print(
      dock_grid(
        "x", "y", "z",
        orientation = "vertical", sizes = c(0.5, 0.3, 0.2)
      )
    )
  )

  expect_snapshot(print(dock_grid("top", group("a", group("b", "c")))))

  expect_snapshot(print(dock_grid()))
})

test_that("dock_grid format strips panel-id prefixes unless bare = FALSE", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    extensions = list(edit = new_edit_board_extension())
  )

  grid <- active_view_grid(brd)

  bare <- format(grid)
  full <- format(grid, bare = FALSE)

  expect_false(any(grepl("block_panel-|ext_panel-", bare)))
  expect_true(any(grepl("block_panel-a", full, fixed = TRUE)))
  expect_true(any(grepl("block_panel-b", full, fixed = TRUE)))
})

test_that("dock_grid format marks the focused panel", {

  grid <- dock_grid("a", panels("b", "c", active = "c"))
  grid[["focus"]] <- "c"

  out <- format(grid)

  expect_true(any(grepl("focus", out, fixed = TRUE)))
})

test_that("empty dock_grid formats as a single line", {

  out <- format(dock_grid())

  expect_length(out, 1L)
  expect_match(out, "empty", fixed = TRUE)
})

test_that("print.dock_grid returns its input invisibly", {

  grid <- dock_grid("a", "b")

  expect_identical(expect_invisible(print(grid)), grid)
})

test_that("str_value.dock_grid lists the panel object ids", {

  grid <- dock_grid("ext_panel-edit_board", "block_panel-a")

  expect_identical(str_value(grid), "<dock_grid> edit_board, a")

  expect_output(
    str(grid),
    "<dock_grid> edit_board, a",
    fixed = TRUE
  )
})

test_that("str_value.dock_grid marks an empty grid", {

  expect_identical(str_value(dock_grid()), "<dock_grid> (empty)")
})

test_that("stack_grid flattens nesting into one vertical stack of groups", {

  grid <- dock_grid(
    "ext_panel-dag",
    panels("block_panel-a", "block_panel-b", active = "block_panel-b"),
    sizes = c(0.3, 0.7)
  )

  stacked <- stack_grid(grid)

  expect_identical(stacked[["orientation"]], "vertical")
  expect_length(stacked[["children"]], 2L)

  # Every panel survives, and a second pass changes nothing.
  expect_setequal(grid_panel_ids(stacked), grid_panel_ids(grid))
  expect_equal(stack_grid(stacked), stacked)
})

test_that("stack_grid keeps each group's panels and its open tab", {

  # The point of stacking over merging: a dashboard's authored grouping is
  # what makes ten panels navigable, so the tab groups have to survive.
  grid <- dock_grid(
    "ext_panel-dag",
    group("block_panel-a", panels("block_panel-b", "block_panel-c")),
    panels("block_panel-d", "block_panel-e", active = "block_panel-e")
  )

  kids <- stack_grid(grid)[["children"]]

  expect_length(kids, 4L)
  expect_identical(kids[[1L]][["panels"]], "ext_panel-dag")
  expect_identical(kids[[2L]][["panels"]], "block_panel-a")
  expect_identical(kids[[3L]][["panels"]], c("block_panel-b", "block_panel-c"))
  expect_identical(kids[[3L]][["active"]], "block_panel-b")
  expect_identical(kids[[4L]][["active"]], "block_panel-e")
})

test_that("grid_leaf_heights reads a leaf's height share off the wide grid", {

  # A horizontal split divides width and leaves height alone; only the
  # vertical ones on a leaf's path reduce it. Direction alternates with depth,
  # so the root orientation fixes every level below.
  expect_equal(grid_leaf_heights(dock_grid("a", "b", "c")), c(1, 1, 1))

  expect_equal(
    grid_leaf_heights(dock_grid("a", "b", orientation = "vertical")),
    c(0.5, 0.5)
  )

  # Root horizontal, so `a` keeps full height while the branch beside it
  # splits vertically, and the pair inside that splits horizontally again.
  expect_equal(
    grid_leaf_heights(
      dock_grid("a", group("b", group("c", "d"), sizes = c(0.25, 0.75)))
    ),
    c(1, 0.25, 0.75, 0.75)
  )
})

test_that("stack rows keep the height the wide layout gave them, capped", {

  withr::local_options(blockr.narrow_group_fraction = 0.8)

  grid <- dock_grid("a", group("b", group("c", "d"), sizes = c(0.25, 0.75)))

  # The full-height column is capped; the quarter-height row stays a quarter
  # rather than being padded out to a uniform row.
  expect_equal(stack_row_heights(grid), c(0.8, 0.25, 0.75, 0.75))

  # The grid carries those normalised, so each row lands back on its share
  # once the container is their sum.
  expect_equal(
    stack_grid(grid)[["sizes"]],
    c(0.8, 0.25, 0.75, 0.75) / 2.55
  )

  expect_equal(stack_grid(new_dock_grid()), new_dock_grid())
})

test_that("stack_grid folds a rail in as a row rather than stranding it", {

  withr::local_options(blockr.narrow_group_fraction = 0.8)

  # The tree and the rails partition the placed panels, so a rail dropped on
  # the way into the stack would strand its members: they are absent from the
  # tree, and nothing downstream would mount them.
  grid <- dock_grid(
    "block_panel-a",
    group("block_panel-b", "block_panel-c", sizes = c(0.25, 0.75)),
    rail("ext_panel-dag", "ext_panel-edit", position = "left")
  )

  stacked <- stack_grid(grid)

  expect_setequal(grid_panel_ids(stacked), grid_panel_ids(grid))
  expect_null(stacked[["rails"]])

  # The rail arrives after the tree's rows, keeping its panels and open tab.
  expect_length(stacked[["children"]], 4L)
  expect_identical(
    stacked[["children"]][[4L]],
    list(
      panels = c("ext_panel-dag", "ext_panel-edit"),
      active = "ext_panel-dag"
    )
  )

  # It has no authored height -- its stored size is a width along its edge --
  # so it takes a full row at the cap.
  expect_equal(stack_row_heights(grid), c(0.8, 0.25, 0.75, 0.8))
})

test_that("a stacked grid is the fixed point of the dockView round trip", {

  # The mirror is off on a narrow board, but the stacked grid still has to be
  # what the client echoes back, or `view_data` and the restore push disagree.
  blks <- c(a = new_dataset_block(), b = new_head_block())

  stacked <- stack_grid(
    resolve_grid(
      dock_grid("a", "b", sizes = c(0.3, 0.7)),
      panel_id_map(blks, list())
    )
  )

  expect_equal(as_dock_grid(as_dock_layout(stacked, blks, list())), stacked)
})
