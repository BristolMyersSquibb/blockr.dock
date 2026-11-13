dock_state_of <- function(board) {
  plugin_state(board_plugins(board)[["blockr.dock-dock_plugin"]])
}

test_that("a board of core's holds the dock's state among its plugins", {

  board <- new_board(
    blocks = c(a = new_dataset_block()),
    plugins = list(new_dock_plugin(views = list(main = dock_view("a")))),
    id = "my_board",
    name = "My board"
  )

  expect_s3_class(board, "board", exact = TRUE)
  expect_identical(board_id(board), "my_board")
  expect_named(board_plugins(board), "blockr.dock-dock_plugin")
  expect_named(dock_state_of(board), c("views", "folders", "active", "visible"))
})

test_that("a board holds no state for the dock where the script gives none", {

  board <- new_board(
    blocks = c(a = new_dataset_block()),
    plugins = list(new_dock_plugin())
  )

  expect_length(dock_state_of(board), 0L)
  expect_length(blockr_ser(board)$board$plugins, 0L)

  expect_identical(board_views(board), list())
  expect_null(active_view(board))
})

test_that("a grid names a plugin by the id of its class, unchecked", {

  # The board holds no plugin of either class: the Edit board plugin keeps no
  # state, and serve() may add the class of the other.
  board <- new_board(
    blocks = c(a = new_dataset_block()),
    plugins = list(
      new_dock_plugin(
        views = list(
          v = dock_view(
            dock_grid(
              "a",
              plg("blockr.core-edit_board_plugin"),
              plg("blockr.dag-dag_plugin")
            )
          )
        )
      )
    )
  )

  expect_identical(
    grid_panels(dock_state_of(board)$views$v$grid),
    list(
      list(block = "a"),
      list(plugin = "blockr.core-edit_board_plugin"),
      list(plugin = "blockr.dag-dag_plugin")
    )
  )
})

test_that("board_views() reads views that new_dock_plugin() takes back", {

  dock <- new_dock_plugin(
    views = list(
      main = dock_view(
        dock_grid(
          panels("a", "b", active = "b"),
          rail(plg("blockr.core-edit_board_plugin"), collapsed = TRUE)
        ),
        name = "Main"
      ),
      side = dock_view("b", folder = "more")
    ),
    folders = list(more = dock_folder("More")),
    active = "side",
    visible = list(b = "outputs")
  )

  blocks <- c(a = new_dataset_block(), b = new_head_block())

  full <- new_board(blocks = blocks, plugins = list(dock))

  views <- board_views(full)

  expect_named(views, c("main", "side"))
  expect_identical(view_name(views$side), "Side")
  expect_identical(views$side$folder, "more")
  expect_true(is_dock_grid(views$main$grid))
  expect_identical(
    grid_panels(views$main$grid),
    list(blk("a"), blk("b"), plg("blockr.core-edit_board_plugin"))
  )
  expect_identical(active_view(full), "side")

  again <- new_board(
    blocks = blocks,
    plugins = list(
      new_dock_plugin(
        views = views,
        folders = list(more = dock_folder("More")),
        active = active_view(full),
        visible = list(b = "outputs")
      )
    )
  )

  expect_identical(dock_state_of(again), dock_state_of(full))

  # A board of one view, as CEDX's script of one view builds one.
  one <- new_board(
    blocks = blocks,
    plugins = list(
      new_dock_plugin(views = board_views(full)["main"], active = "main")
    )
  )

  expect_identical(
    dock_state_of(one)$views$main$grid,
    dock_state_of(full)$views$main$grid
  )
})

test_that("the readers read a board that holds no state for the dock", {

  board <- new_board(blocks = c(a = new_dataset_block()))

  expect_identical(board_views(board), list())
  expect_null(active_view(board))

  expect_error(board_views(list()), class = "dock_board_invalid")
})

test_that("every board fixture has its R values", {
  expect_setequal(names(board_fixture_values()), board_fixtures())
})

test_that("R writes the JSON board of every fixture as the fixture holds it", {

  values <- board_fixture_values()

  for (name in board_fixtures()) {

    fixture <- read_json_strictly(board_fixture(name))
    written <- read_json_strictly(write_json(blockr_ser(values[[name]]$board)))

    # The versions of the packages that wrote a fixture go unchecked.
    parts <- setdiff(names(fixture), c("description", "packages"))

    # Not expect_equal(), whose tolerance takes a number marked with `I()` for
    # the same number unmarked, which would let a scalar pass for an array.
    expect_identical(
      written[parts],
      fixture[parts],
      label = paste("the JSON board R writes for", name),
      expected.label = "the fixture's"
    )
  }
})
