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

test_that("plg() names a plugin by its name in `plugins` or by its class", {

  # A bare id names a block, and never a plugin of that name.
  board <- new_board(
    blocks = c(a = new_dataset_block(), edit = new_head_block()),
    plugins = list(
      new_dock_plugin(
        views = list(
          v = dock_view(
            dock_grid(
              "a", "edit",
              plg("edit"), plg("files"), plg("blockr.dag-dag_plugin")
            )
          )
        )
      ),
      edit = new_edit_board_plugin(),
      files = new_file_storage_plugin()
    )
  )

  expect_identical(
    grid_panels(dock_state_of(board)$views$v$grid),
    list(
      list(block = "a"),
      list(block = "edit"),
      list(plugin = "blockr.core-edit_board_plugin"),
      list(plugin = "blockr.core-file_storage_plugin"),
      list(plugin = "blockr.dag-dag_plugin")
    )
  )

  # The dock plugin alone holds the names, until it is on a board.
  dock <- new_dock_plugin(views = list(v = dock_view(plg("edit"))))

  expect_identical(
    grid_panels(plugin_state(dock)$views$v$grid),
    list(list(plugin = "edit"))
  )
})

test_that("the default grid of a view tells a plugin's panel by its name", {

  board <- new_board(
    blocks = c(a = new_dataset_block()),
    plugins = list(
      new_dock_plugin(views = list(v = dock_view(list("a", plg("edit"))))),
      edit = new_edit_board_plugin()
    )
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

test_that("a plugin placed by its name and by its class is placed twice", {

  expect_error(
    new_board(
      plugins = list(
        new_dock_plugin(
          views = list(
            v = dock_view(
              dock_grid(
                plg("edit"),
                rail(plg("blockr.core-edit_board_plugin"))
              )
            )
          )
        ),
        edit = new_edit_board_plugin()
      )
    ),
    paste(
      "The grid of the view v places the plugin blockr.core-edit_board_plugin",
      "more than once."
    ),
    fixed = TRUE,
    class = "dock_grid_panel_duplicated"
  )
})

test_that("the dock refuses a plg() that names no plugin and no class", {

  expect_error(
    new_board(
      plugins = list(
        new_dock_plugin(
          views = list(
            v = dock_view(dock_grid(plg("dga"), plg("edit"))),
            w = dock_view(list(plg("x"), plg("blockr.dag-dag_plugin")))
          )
        ),
        files = new_file_storage_plugin()
      )
    ),
    paste(
      "The grid of the view v names the plugins dga and edit, which the",
      "board's plugins do not name. The grid of the view w names the plugin",
      "x, which the board's plugins do not name."
    ),
    fixed = TRUE,
    class = "dock_plugin_name_unknown"
  )

  # The id of a plugin class goes unchecked, since serve() may add the class,
  # as do the Edit board plugin's, which keeps no state.
  board <- new_board(
    plugins = list(
      new_dock_plugin(
        views = list(
          v = dock_view(
            list(
              plg("blockr.dag-dag_plugin"),
              plg("blockr.core-edit_board_plugin")
            )
          )
        )
      )
    )
  )

  expect_identical(
    grid_panels(dock_state_of(board)$views$v$grid),
    list(
      list(plugin = "blockr.dag-dag_plugin"),
      list(plugin = "blockr.core-edit_board_plugin")
    )
  )
})

test_that("the dock refuses each block that the board lacks", {

  expect_error(
    new_board(
      blocks = c(a = new_dataset_block()),
      plugins = list(
        new_dock_plugin(
          views = list(
            v = dock_view(dock_grid("a", "x")),
            w = dock_view(list("y", blk("z")))
          ),
          visible = list(q = "inputs")
        )
      )
    ),
    paste(
      "The grid of the view v names the block x, which the board lacks.",
      "The grid of the view w names the blocks y and z, which the board",
      "lacks. The argument `visible` names the block q, which the board lacks."
    ),
    fixed = TRUE,
    class = "dock_board_block_unknown"
  )

  # A state without views.
  expect_error(
    new_board(
      blocks = c(a = new_dataset_block()),
      plugins = list(
        new_dock_plugin(visible = list(a = "inputs", b = "inputs"))
      )
    ),
    "The argument `visible` names the block b, which the board lacks.",
    fixed = TRUE,
    class = "dock_board_block_unknown"
  )
})

test_that("board_views() reads views that new_dock_plugin() takes back", {

  dock <- new_dock_plugin(
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

  blocks <- c(a = new_dataset_block(), b = new_head_block())

  full <- new_board(
    blocks = blocks,
    plugins = list(dock, edit = new_edit_board_plugin())
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
