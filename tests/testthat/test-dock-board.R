dock_state_of <- function(board) {
  plugin_state(board_plugins(board)[["blockr.dock-dock_plugin"]])
}

test_that("new_dock_board() builds a board of core's with the dock's state", {

  board <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    views = list(main = dock_view("a")),
    id = "my_board",
    name = "My board"
  )

  expect_s3_class(board, "board", exact = TRUE)
  expect_identical(board_id(board), "my_board")
  expect_identical(board_name(board), "My board")
  expect_named(board_plugins(board), "blockr.dock-dock_plugin")
  expect_named(dock_state_of(board), c("views", "folders", "active", "visible"))
})

test_that("a board holds no state for the dock where the script gives none", {

  board <- new_dock_board(blocks = c(a = new_dataset_block()))

  expect_length(dock_state_of(board), 0L)
  expect_length(blockr_ser(board)$board$plugins, 0L)

  expect_identical(board_views(board), list())
  expect_null(active_view(board))
})

test_that("a bare id names a block, or else the plugin of that name", {

  board <- new_dock_board(
    blocks = c(a = new_dataset_block(), edit = new_head_block()),
    plugins = list(
      edit = new_edit_board_plugin(),
      files = new_file_storage_plugin()
    ),
    views = list(
      v = dock_view(
        dock_grid(
          "a", "edit", "files",
          plg("edit"), plg("blockr.dag-dag_plugin")
        )
      )
    )
  )

  expect_identical(
    grid_panels(dock_state_of(board)$views$v$grid),
    list(
      list(block = "a"),
      list(block = "edit"),
      list(plugin = "blockr.core-file_storage_plugin"),
      list(plugin = "blockr.core-edit_board_plugin"),
      list(plugin = "blockr.dag-dag_plugin")
    )
  )

  # A reference made with blk() names a block, never a plugin.
  expect_error(
    new_dock_board(
      plugins = list(files = new_file_storage_plugin()),
      views = list(v = dock_view(blk("files")))
    ),
    "The grid of the view v names the block files, which the board lacks.",
    fixed = TRUE,
    class = "dock_board_block_unknown"
  )
})

test_that("the default grid of a view tells a plugin's panel by its name", {

  board <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    plugins = list(edit = new_edit_board_plugin()),
    views = list(v = dock_view(c("a", "edit")))
  )

  edit <- list(plugin = "blockr.core-edit_board_plugin")

  expect_identical(
    dock_state_of(board)$views$v$grid$children,
    list(
      list(panels = list(edit), active = edit),
      list(panels = list(list(block = "a")), active = list(block = "a"))
    )
  )
})

test_that("the board holds the plugins without the names of the script", {

  board <- new_dock_board(plugins = list(edit = new_edit_board_plugin()))

  expect_named(
    board_plugins(board),
    c("blockr.dock-dock_plugin", "blockr.core-edit_board_plugin")
  )
  expect_named(dock_plugins(board), "blockr.core-edit_board_plugin")

  # A plugin alone, and a list without names.
  expect_named(
    board_plugins(new_dock_board(plugins = new_edit_board_plugin())),
    c("blockr.dock-dock_plugin", "blockr.core-edit_board_plugin")
  )
  expect_named(
    dock_plugins(new_dock_board(plugins = list(new_edit_board_plugin()))),
    "blockr.core-edit_board_plugin"
  )
})

test_that("new_dock_board() refuses each block that the board lacks", {

  expect_error(
    new_dock_board(
      blocks = c(a = new_dataset_block()),
      views = list(
        v = dock_view(dock_grid("a", "x")),
        w = dock_view(list("y", blk("z")))
      ),
      visible = list(q = "inputs")
    ),
    paste(
      "The grid of the view v names the block x, which the board lacks.",
      "The grid of the view w names the blocks y and z, which the board",
      "lacks. The argument `visible` names the block q, which the board lacks."
    ),
    fixed = TRUE,
    class = "dock_board_block_unknown"
  )
})

test_that("new_dock_board() refuses no reference to a plugin", {

  board <- new_dock_board(
    views = list(v = dock_view(plg("blockr.dag-dag_plugin")))
  )

  expect_identical(
    grid_panels(dock_state_of(board)$views$v$grid),
    list(list(plugin = "blockr.dag-dag_plugin"))
  )
})

test_that("new_dock_board() refuses plugins that it cannot tell apart", {

  expect_error(
    new_dock_board(
      plugins = list(x = new_edit_board_plugin(), x = new_file_storage_plugin())
    ),
    "while x names more than one.",
    fixed = TRUE,
    class = "dock_board_plugins_invalid"
  )
  expect_error(
    new_dock_board(plugins = list("a")),
    class = "dock_board_plugins_invalid"
  )
  expect_error(
    new_dock_board(
      plugins = list(new_edit_board_plugin(), new_edit_board_plugin())
    ),
    class = "board_plugins_invalid"
  )
})

test_that("a board holds the colours and option values of the script", {

  board <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    stacks = stacks(s = new_stack("a"), t = new_stack(color = "#ffcc88"))
  )

  expect_null(stack_color(board_stacks(board)[["s"]]))
  expect_identical(stack_color(board_stacks(board)[["t"]]), "#ffcc88")
  expect_length(board_options(board), 0L)

  board <- new_dock_board(options = new_compact_option(TRUE))
  expect_identical(board_option_ids(board), "compact")
})

test_that("board_views() reads views that new_dock_board() takes back", {

  full <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    plugins = list(edit = new_edit_board_plugin()),
    views = list(
      main = dock_view(
        dock_grid(
          panels("a", "b", active = "b"),
          rail(plg("edit"), collapsed = TRUE)
        ),
        name = "Main"
      ),
      side = dock_view("b", folder = "more")
    ),
    folders = list(more = dock_folder("More")),
    active = "side",
    visible = list(b = "outputs")
  )

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

  again <- new_dock_board(
    blocks = board_blocks(full),
    plugins = dock_plugins(full),
    views = views,
    folders = list(more = dock_folder("More")),
    active = active_view(full),
    visible = list(b = "outputs")
  )

  expect_identical(dock_state_of(again), dock_state_of(full))

  # A board of one view, as CEDX's script of one view builds one.
  one <- new_dock_board(
    blocks = board_blocks(full)[c("a", "b")],
    plugins = dock_plugins(full),
    views = board_views(full)["main"],
    active = "main"
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
  expect_length(dock_plugins(board), 0L)

  expect_error(board_views(list()), class = "dock_board_invalid")
  expect_error(dock_plugins(list()))
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
