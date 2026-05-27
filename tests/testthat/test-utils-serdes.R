test_that("ser/des utils", {

  board1 <- new_dock_board()

  expect_identical(
    board1,
    blockr_deser(blockr_ser(board1)),
    ignore_function_env = TRUE
  )

  board2 <- new_dock_board(extensions = new_edit_board_extension())

  expect_identical(
    board2,
    blockr_deser(blockr_ser(board2)),
    ignore_function_env = TRUE
  )
})

test_that("dock_layouts serialization round-trip", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(
      Tab1 = list("a", "b"),
      Tab2 = dock_layout("a", active = TRUE)
    )
  )

  ser <- blockr_ser(brd)
  des <- blockr_deser(ser)

  ly <- des[["layouts"]]
  expect_s3_class(ly, "dock_layouts")
  expect_named(ly, c("Tab1", "Tab2"))
  expect_identical(active_view(ly), "Tab2")
})

test_that("serialized dock_layout carries no panels", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(Page = list("a", "b"))
  )

  ser <- blockr_ser(brd)
  payload <- ser[["payload"]][["layouts"]][["payload"]][["views"]][[1L]]

  expect_named(payload[["payload"]], c("grid", "activeGroup"))
  expect_false("panels" %in% names(payload[["payload"]]))
})

# Layout features encoded in the grid tree (sizes, activeView,
# orientation, nested group sizes) must survive ser/des untouched —
# otherwise a saved custom arrangement would degrade to defaults on
# load.
test_that("dock_layout custom sizes round-trip through ser/des", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(Page = dock_layout("a", "b", sizes = c(0.3, 0.7)))
  )
  ly <- blockr_deser(blockr_ser(brd))[["layouts"]][["Page"]]
  expect_equal(ly$grid$root$data[[1L]]$size, 0.3)
  expect_equal(ly$grid$root$data[[2L]]$size, 0.7)
})

test_that("panels(active = ...) round-trips through ser/des", {
  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(),
      b = new_head_block(),
      c = new_head_block()
    ),
    layouts = list(
      Page = dock_layout(panels("a", "b", "c", active = "b"))
    )
  )
  ly <- blockr_deser(blockr_ser(brd))[["layouts"]][["Page"]]
  expect_identical(
    ly$grid$root$data[[1L]]$data$activeView,
    "block_panel-b"
  )
})

test_that("orientation round-trips through ser/des", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(
      Page = dock_layout("a", "b", orientation = "vertical")
    )
  )
  ly <- blockr_deser(blockr_ser(brd))[["layouts"]][["Page"]]
  expect_identical(ly$grid$orientation, "VERTICAL")
})

test_that("nested group() sizes round-trip through ser/des", {
  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(),
      b = new_head_block(),
      c = new_head_block()
    ),
    layouts = list(
      Page = dock_layout(
        "a",
        group("b", "c", sizes = c(0.4, 0.6)),
        sizes = c(0.3, 0.7)
      )
    )
  )
  ly <- blockr_deser(blockr_ser(brd))[["layouts"]][["Page"]]
  outer <- ly$grid$root$data
  expect_equal(outer[[1L]]$size, 0.3)
  expect_equal(outer[[2L]]$size, 0.7)
  expect_equal(outer[[2L]]$data[[1L]]$size, 0.4)
  expect_equal(outer[[2L]]$data[[2L]]$size, 0.6)
})

# Forward compat: a payload saved by a pre-Option B build still carries
# the old wire-shape `panels` map. The new deserializer must read the
# grid + activeGroup and silently drop the stale panels (which would be
# re-derived from blocks at restore time anyway).
test_that("legacy dock_layout payload with panels still loads", {

  legacy_payload <- list(
    object = "dock_layout",
    payload = list(
      grid = list(
        root = list(
          type = "branch",
          data = list(
            list(
              type = "leaf",
              data = list(
                views = list("block_panel-a"),
                activeView = "block_panel-a",
                id = "1"
              ),
              size = 0.5
            )
          ),
          size = 1
        ),
        orientation = "HORIZONTAL"
      ),
      panels = list(
        `block_panel-a` = list(
          id = "block_panel-a",
          title = "Stale title from save time"
        )
      ),
      activeGroup = "1"
    )
  )

  ly <- blockr_deser(legacy_payload)
  expect_s3_class(ly, "dock_layout")
  expect_false("panels" %in% names(ly))
  expect_identical(layout_panel_ids(ly), "block_panel-a")
  expect_identical(ly$grid$orientation, "HORIZONTAL")
  expect_identical(ly$activeGroup, "1")
})
