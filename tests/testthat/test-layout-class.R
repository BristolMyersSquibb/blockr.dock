test_that("panel layout", {
  expect_snapshot(draw_panel_tree(NULL))
  expect_snapshot(draw_panel_tree(c("a", "b", "c")))
  expect_snapshot(draw_panel_tree(list("a", list("b", "c"))))
})

test_that("layout resolution accepts a bare dock_extension", {

  blks <- c(a = new_dataset_block(), b = new_head_block())
  ext <- new_edit_board_extension()

  brd <- new_dock_board(blocks = blks, extensions = ext)
  ly <- active_layout(brd)

  expect_true(is_dock_layout(ly))
  expect_setequal(
    layout_panel_ids(ly),
    c("block_panel-a", "block_panel-b", "ext_panel-edit_board_extension")
  )

  brd2 <- new_dock_board(
    blocks = blks,
    extensions = ext,
    layouts = list("edit_board_extension", c("a", "b"))
  )
  expect_setequal(
    layout_panel_ids(active_layout(brd2)),
    c("block_panel-a", "block_panel-b", "ext_panel-edit_board_extension")
  )
})

test_that("layout resolution accepts a named list of extensions", {

  blks <- c(a = new_dataset_block(), b = new_head_block())
  exts <- list(edit = new_edit_board_extension())

  brd <- new_dock_board(blocks = blks, extensions = exts)
  ly <- active_layout(brd)

  expect_true(is_dock_layout(ly))
  expect_setequal(
    layout_panel_ids(ly),
    c("block_panel-a", "block_panel-b", "ext_panel-edit_board_extension")
  )

  brd2 <- new_dock_board(
    blocks = blks,
    extensions = exts,
    layouts = list("edit_board_extension", c("a", "b"))
  )
  expect_setequal(
    layout_panel_ids(active_layout(brd2)),
    c("block_panel-a", "block_panel-b", "ext_panel-edit_board_extension")
  )
})

test_that("layout resolution accepts a dock_extensions collection", {

  blks <- c(a = new_dataset_block(), b = new_head_block())
  exts <- new_dock_extensions(list(new_edit_board_extension()))

  brd <- new_dock_board(blocks = blks, extensions = exts)
  ly <- active_layout(brd)

  expect_true(is_dock_layout(ly))
  expect_setequal(
    layout_panel_ids(ly),
    c("block_panel-a", "block_panel-b", "ext_panel-edit_board_extension")
  )

  brd2 <- new_dock_board(
    blocks = blks,
    extensions = exts,
    layouts = list("edit_board_extension", c("a", "b"))
  )
  expect_setequal(
    layout_panel_ids(active_layout(brd2)),
    c("block_panel-a", "block_panel-b", "ext_panel-edit_board_extension")
  )
})

test_that("default_layout uses class-name convention across input forms", {

  blks <- c(a = new_dataset_block())
  expected <- dock_layout("edit_board_extension", "a")

  expect_identical(
    default_layout(blks, new_edit_board_extension()),
    expected
  )

  expect_identical(
    default_layout(blks, list(edit = new_edit_board_extension())),
    expected
  )

  expect_identical(
    default_layout(blks, new_dock_extensions(list(new_edit_board_extension()))),
    expected
  )
})

test_that("dock_layout constructor with active flag selects the view", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(
      First = list("a"),
      Second = dock_layout("a", "b", active = TRUE)
    )
  )

  expect_identical(active_view(board_layouts(brd)), "Second")
  expect_length(layout_panel_ids(active_layout(brd)), 2L)
})

test_that("layout is stored without panels (no duplication across views)", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(
      A = list("a", "b"),
      B = list("a", "b")
    )
  )

  views <- board_layouts(brd)

  expect_named(views$A, c("grid", "activeGroup"))
  expect_false("panels" %in% names(views$A))
  expect_false("panels" %in% names(views$B))
})

test_that("dock_layout marks an arrangement active via attribute", {

  v <- dock_layout("a", "b", active = TRUE)
  expect_true(is_dock_layout(v))
  expect_setequal(layout_panel_ids(v), c("a", "b"))
  expect_true(isTRUE(attr(v, "active")))

  expect_null(attr(dock_layout("a"), "active"))
  expect_null(attr(dock_layout("a", active = FALSE), "active"))
})

test_that("dockview_payload materialises grid + panels on demand", {

  blks <- c(a = new_dataset_block(), b = new_head_block())
  ly <- dock_layout("a", "b")
  resolved <- resolve_dock_layout(blks, list(), ly)

  payload <- dockview_payload(resolved, blks, list())

  expect_named(payload, c("grid", "panels", "activeGroup"))
  expect_named(payload$panels, c("block_panel-a", "block_panel-b"))
  expect_identical(payload$panels[["block_panel-a"]][["title"]], "Dataset")
})

test_that("sizes propagate to grid leaves and nested branches", {

  ly <- dock_layout("a", "b", sizes = c(0.3, 0.7))
  expect_equal(ly$grid$root$data[[1L]]$size, 0.3)
  expect_equal(ly$grid$root$data[[2L]]$size, 0.7)

  ly2 <- dock_layout(
    "a",
    group("b", "c", sizes = c(0.4, 0.6)),
    sizes = c(0.3, 0.7)
  )
  outer <- ly2$grid$root$data
  expect_equal(outer[[1L]]$size, 0.3)
  expect_equal(outer[[2L]]$size, 0.7)
  expect_equal(outer[[2L]]$data[[1L]]$size, 0.4)
  expect_equal(outer[[2L]]$data[[2L]]$size, 0.6)
})

test_that("panels() sets the active tab in a tabbed leaf", {

  ly <- dock_layout(panels("a", "b", "c", active = "b"))
  leaf <- ly$grid$root$data[[1L]]
  expect_identical(leaf$data$activeView, "b")
  expect_identical(unlist(leaf$data$views), c("a", "b", "c"))
})

test_that("panels() with single id is allowed but redundant", {

  expect_silent(panels("a"))
  ly <- dock_layout(panels("a"))
  expect_identical(layout_panel_ids(ly), "a")
})

test_that("orientation flips the top-level grid", {

  ly <- dock_layout("a", "b", orientation = "vertical")
  expect_identical(ly$grid$orientation, "VERTICAL")
})

test_that("dock_layout rejects invalid sizes vector", {

  expect_error(
    dock_layout("a", "b", sizes = c(0.5)),
    class = "dock_layout_sizes_invalid"
  )
  expect_error(
    dock_layout("a", "b", sizes = c(-0.5, 1.5)),
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

  # dockview_payload reads is_dock_locked() when building panels, so the
  # `tabComponent` it emits already reflects the option in effect at call
  # time. lock_panels is the explicit re-coercion applied at restore time
  # to bring saved panels in line with the *current* lock state.
  unlocked_payload <- withr::with_options(
    list(blockr.dock_is_locked = NULL),
    dockview_payload(dock_layout("a"), blocks = blks)
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
    list(blockr.dock_is_locked = TRUE),
    dockview_payload(dock_layout("a"), blocks = blks)
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

test_that("dock_layout has a tree-style print method", {

  expect_snapshot(print(dock_layout("a", "b")))

  expect_snapshot(
    print(
      dock_layout(
        group("data", "filt", "head"), "assistant_extension",
        sizes = c(0.6, 0.4)
      )
    )
  )

  expect_snapshot(
    print(dock_layout("a", panels("b", "c", "edit", active = "c")))
  )

  expect_snapshot(
    print(
      dock_layout(
        "x", "y", "z",
        orientation = "vertical", sizes = c(0.5, 0.3, 0.2)
      )
    )
  )

  expect_snapshot(print(dock_layout("top", group("a", group("b", "c")))))

  expect_snapshot(print(dock_layout()))
})

test_that("dock_layout format strips panel-id prefixes unless bare = FALSE", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    extensions = list(edit = new_edit_board_extension())
  )

  ly <- active_layout(brd)

  bare <- format(ly)
  full <- format(ly, bare = FALSE)

  expect_false(any(grepl("block_panel-|ext_panel-", bare)))
  expect_true(any(grepl("ext_panel-edit_board_extension", full, fixed = TRUE)))
  expect_true(any(grepl("block_panel-a", full, fixed = TRUE)))
})

test_that("dock_layout format marks the focused panel", {

  ly <- as_dock_layout(
    list(
      orientation = "horizontal",
      children = list("a", list(panels = list("b", "c"), active = "c")),
      focus = "c"
    )
  )

  out <- format(ly)

  expect_match(out[length(out)], "focus", fixed = TRUE)
  expect_false(any(grepl("a (active", out, fixed = TRUE)))
})

test_that("empty dock_layout formats as a single line", {

  out <- format(dock_layout())

  expect_length(out, 1L)
  expect_match(out, "empty", fixed = TRUE)
})

test_that("print.dock_layout returns its input invisibly", {

  ly <- dock_layout("a", "b")

  expect_identical(expect_invisible(print(ly)), ly)
})
