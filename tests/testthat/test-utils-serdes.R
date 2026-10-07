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

# A dock board's option values -- board-owned (board_name) and block-
# contributed (page_size, baked on by board_server's effective option set)
# alike -- must come from the saved board, never leak in from the pre-restore
# board. Restore deserializes the saved board and returns it as-is.
test_that("restoring a dock board keeps the saved board-option values", {

  saved <- new_dock_board(blocks = c(a = new_dataset_block()))
  board_options(saved) <- new_board_options(
    new_board_name_option(value = "Saved"),
    new_page_size_option(value = 25L)
  )

  current <- new_dock_board(blocks = c(a = new_dataset_block()))
  board_options(current) <- new_board_options(
    new_board_name_option(value = "Current"),
    new_page_size_option(value = 99L)
  )

  restored <- NULL
  restore_board(current, blockr_ser(saved), function(x) restored <<- x)

  opts <- board_options(restored)
  expect_identical(board_option_value(opts[["board_name"]]), "Saved")
  expect_identical(board_option_value(opts[["page_size"]]), 25L)
})

test_that("views round-trip through serialization", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(Tab1 = c("a", "b"), Tab2 = "a"),
    active = "Tab2"
  )

  des <- blockr_deser(blockr_ser(brd))

  views <- board_views(des)
  expect_s3_class(views, "dock_views")
  expect_identical(unname(view_names(views)), c("Tab1", "Tab2"))
  expect_identical(active_name(views), "Tab2")
})

test_that("a multi-view board round-trips identically through ser/des", {

  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(),
      b = new_head_block(),
      c = new_head_block()
    ),
    views = list(
      analysis = dock_view(c("a", "b", "c"), name = "Analysis"),
      overview = dock_view("a", name = "Overview")
    ),
    grids = list(
      analysis = dock_grid(
        "a",
        panels("b", "c", active = "c"),
        sizes = c(0.3, 0.7)
      )
    ),
    active = "overview"
  )

  des <- blockr_deser(blockr_ser(brd))

  # ids (keys), display names, the active marker and every view's membership
  # and geometry survive a full serialize / deserialize cycle.
  expect_identical(board_views(des), board_views(brd))
  expect_identical(board_grids(des), board_grids(brd))
})

test_that("rails round-trip through ser/des", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    extensions = new_edit_board_extension(),
    views = list(V = c("a", "b", "edit_board")),
    grids = list(
      V = dock_grid(
        "a", "b",
        rail(ext("edit_board"), position = "right", size = 300,
             collapsed_size = 20)
      )
    )
  )

  des <- blockr_deser(blockr_ser(brd))

  expect_identical(board_grids(des), board_grids(brd))
  expect_identical(board_views(des), board_views(brd))
})

test_that("a board saved before rails existed still restores", {

  # The pre-rails format carries `views` and `grids` and no `rails` key at all,
  # with the extension placed in the grid like any other panel. Two things have
  # to hold at once. The saved *layout* is reproduced -- the extension stays in
  # the grid rather than being quietly re-homed into a rail -- while the rail
  # *capability* is still there, because whether a dock offers an edge to pin a
  # panel to must never depend on what a save happened to carry.
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    extensions = new_edit_board_extension(),
    views = list(V = c("a", "b", "edit_board")),
    grids = list(
      V = dock_grid("ext_panel-edit_board", "block_panel-a", "block_panel-b")
    )
  )

  old <- blockr_ser(brd)
  old[["payload"]][["grids"]][["payload"]][["V"]][["rails"]] <- NULL

  des <- blockr_deser(old)

  # Nothing is written into the grid the save did not carry.
  expect_null(board_grids(des)[["V"]][["rails"]])
  expect_identical(board_grids(des), board_grids(brd))
  expect_identical(board_views(des), board_views(brd))
  expect_s3_class(validate_board(des), "dock_board")

  # But the edges are there to drag to, empty and so hidden, and the saved
  # layout is untouched: the extension is still placed by the grid.
  rails <- active_view_grid(des)[["rails"]]

  expect_named(rails, c("left", "right"))
  expect_identical(rail_panel_ids(rails), character())
  expect_true(
    "ext_panel-edit_board" %in% grid_tree_ids(active_view_grid(des))
  )
})

test_that("a rail survives a real JSON encode/decode round-trip", {

  # The stored form goes out through core's `write_json` settings, so a rail's
  # panel list must survive being read back as a length-one JSON array.
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    extensions = new_edit_board_extension(),
    views = list(V = c("a", "edit_board")),
    grids = list(V = dock_grid("a", rail(ext("edit_board"))))
  )

  reparsed <- jsonlite::fromJSON(
    jsonlite::toJSON(blockr_ser(brd), null = "null"),
    simplifyDataFrame = FALSE, simplifyMatrix = FALSE
  )

  expect_identical(board_grids(blockr_deser(reparsed)), board_grids(brd))
})

test_that("dock_views preserve a non-alphabetical key order through ser/des", {

  # View order is implicit in the list's key sequence -- no separate slot -- so
  # it survives save / restore only by JSON key ordering. Pin that with ids in
  # reverse-alphabetical order (and a non-first active): a layer that sorted
  # keys would reorder these and fail.
  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(),
      b = new_head_block(),
      c = new_head_block()
    ),
    views = list(zebra = "a", mango = "b", apple = "c"),
    active = "mango"
  )

  des <- blockr_deser(blockr_ser(brd))

  expect_identical(names(board_views(des)), c("zebra", "mango", "apple"))
  expect_identical(active_view(board_views(des)), "mango")
})

test_that("serialized dock_views records view id, name and active", {

  # Fixed ids (the list keys) keep the wire shape deterministic so the id
  # (object key) / name (field) / active split stays visible and
  # regression-guarded.
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(
      view_one = dock_view("a", name = "Analysis"),
      view_two = dock_view("b", name = "Overview")
    ),
    active = "view_two"
  )

  views <- blockr_ser(brd)[["payload"]][["views"]][["payload"]]

  expect_identical(views[["active"]], "view_two")
  expect_identical(names(views[["views"]]), c("view_one", "view_two"))
  expect_identical(views[["views"]][["view_one"]][["name"]], "Analysis")
  expect_identical(views[["views"]][["view_two"]][["name"]], "Overview")
  expect_identical(
    unlist(views[["views"]][["view_one"]][["payload"]]),
    "block_panel-a"
  )
})

test_that("a grid serializes to our compact form", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(Page = c("a", "b")),
    grids = list(Page = dock_grid("a", "b", sizes = c(0.3, 0.7)))
  )

  payload <- blockr_ser(brd)[["payload"]][["grids"]][["payload"]][[1L]]

  # Our compact form: orientation / children / sizes, never dockView's tree
  # (`grid` / `root`), resolved `panels`, or `activeGroup`.
  expect_true(all(c("orientation", "children", "sizes") %in% names(payload)))
  expect_false(
    any(c("grid", "root", "panels", "activeGroup") %in% names(payload))
  )
  expect_equal(payload[["sizes"]], c(0.3, 0.7))
  expect_identical(payload[["children"]][[1L]][["panels"]], "block_panel-a")
})

test_that("custom sizes round-trip through ser/des", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(Page = c("a", "b")),
    grids = list(Page = dock_grid("a", "b", sizes = c(0.3, 0.7)))
  )
  grid <- board_grids(blockr_deser(blockr_ser(brd)))[[1L]]
  expect_equal(grid[["sizes"]], c(0.3, 0.7))
})

test_that("panels(active = ...) round-trips through ser/des", {
  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(), b = new_head_block(), c = new_head_block()
    ),
    views = list(Page = c("a", "b", "c")),
    grids = list(Page = dock_grid(panels("a", "b", "c", active = "b")))
  )
  grid <- board_grids(blockr_deser(blockr_ser(brd)))[[1L]]
  expect_identical(grid[["children"]][[1L]][["active"]], "block_panel-b")
})

test_that("orientation round-trips through ser/des", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(Page = c("a", "b")),
    grids = list(Page = dock_grid("a", "b", orientation = "vertical"))
  )
  grid <- board_grids(blockr_deser(blockr_ser(brd)))[[1L]]
  expect_identical(grid[["orientation"]], "vertical")
})

test_that("nested group() sizes round-trip through ser/des", {
  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(), b = new_head_block(), c = new_head_block()
    ),
    views = list(Page = c("a", "b", "c")),
    grids = list(
      Page = dock_grid(
        "a", group("b", "c", sizes = c(0.4, 0.6)), sizes = c(0.3, 0.7)
      )
    )
  )
  grid <- board_grids(blockr_deser(blockr_ser(brd)))[[1L]]
  expect_equal(grid[["sizes"]], c(0.3, 0.7))
  expect_equal(grid[["children"]][[2L]][["sizes"]], c(0.4, 0.6))
})

test_that("a grid survives a real JSON encode/decode round-trip", {
  # blockr.core writes via toJSON(null = "null") / fromJSON(simplifyVector =
  # TRUE), which collapses all-scalar arrays to atomic vectors. Exercise that
  # path so a boxing / simplify regression can't ship silently.
  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(), b = new_head_block(), c = new_head_block()
    ),
    views = list(Page = c("a", "b", "c")),
    grids = list(
      Page = dock_grid(
        "a", group("b", "c", sizes = c(0.4, 0.6)), sizes = c(0.3, 0.7)
      )
    )
  )
  ser <- blockr_ser(brd)
  json <- jsonlite::toJSON(ser, null = "null")
  back <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE,
                             simplifyMatrix = FALSE)
  grid <- board_grids(blockr_deser(back))[[1L]]

  expect_equal(grid[["sizes"]], c(0.3, 0.7))
  expect_equal(grid[["children"]][[2L]][["sizes"]], c(0.4, 0.6))
  expect_setequal(
    layout_panel_ids(grid),
    c("block_panel-a", "block_panel-b", "block_panel-c")
  )
})

test_that("a single-tab leaf reaches dockView as a JSON array", {
  # dockView reads a leaf's `views` as an array; handed a bare string it throws
  # `Cannot read properties of undefined (reading 'id')` and blanks the page.
  # Shiny encodes the restore payload with `auto_unbox = TRUE`, which collapses
  # a length-1 character vector to a scalar, so `views` has to reach the seam
  # as a list. Every outbound path runs through `grid_to_tree()`, which is what
  # guarantees that.
  blks <- as_blocks(list(a = new_dataset_block(), b = new_head_block()))

  wire_views <- function(grid) {
    leaf <- grid_leaves(unclass(as_dock_layout(grid, blocks = blks))[["grid"]])
    as.character(
      jsonlite::toJSON(leaf[[1L]][["views"]], auto_unbox = TRUE)
    )
  }

  expect_identical(wire_views(dock_grid("block_panel-a")), '["block_panel-a"]')
  expect_identical(
    wire_views(dock_grid(panels("block_panel-a", "block_panel-b"))),
    '["block_panel-a","block_panel-b"]'
  )

  # A live echo arrives back through `fromJSON`, which does collapse the
  # single-element array to a bare character vector -- re-expanding has to
  # restore the array before the payload goes out again.
  lay <- as_dock_layout(dock_grid("block_panel-a"), blocks = blks)
  echo <- jsonlite::fromJSON(
    jsonlite::toJSON(unclass(lay), auto_unbox = TRUE),
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  )

  expect_type(grid_leaves(echo[["grid"]])[[1L]][["views"]], "character")
  expect_identical(
    wire_views(as_dock_grid(as_dock_layout(echo))),
    '["block_panel-a"]'
  )

  # The persistence path: a saved board decodes through the same collapse.
  brd <- new_dock_board(
    blocks = blks,
    views = list(Page = "a"),
    grids = list(Page = dock_grid("a"))
  )
  back <- blockr_deser(
    jsonlite::fromJSON(
      jsonlite::toJSON(blockr_ser(brd), null = "null"),
      simplifyDataFrame = FALSE,
      simplifyMatrix = FALSE
    )
  )

  expect_identical(
    wire_views(board_grids(back)[[1L]]),
    '["block_panel-a"]'
  )
})

test_that("focus round-trips through the dockView seam", {
  blks <- as_blocks(
    list(a = new_dataset_block(), b = new_head_block(), c = new_head_block())
  )

  # dockView's focus is a group, so the focused panel is that group's open
  # tab -- `focus` and the leaf's `active` name the same panel.
  grid <- dock_grid(
    "block_panel-a",
    panels("block_panel-b", "block_panel-c", active = "block_panel-c")
  )
  grid[["focus"]] <- "block_panel-c"

  # Expanding to a dockView layout carries the focus as an activeGroup id;
  # collapsing back recovers the focused panel.
  lay <- as_dock_layout(grid, blocks = blks)
  expect_true(is.character(lay[["activeGroup"]]))
  expect_identical(as_dock_grid(lay)[["focus"]], "block_panel-c")
})

test_that("dockView pixel sizes normalise to ratios on collapse", {
  # A live dockView echo carries absolute pixel sizes; collapsing to our
  # canonical grid normalises them to 0-1 ratios.
  leaf <- function(id, size) {
    list(type = "leaf",
         data = list(views = list(id), activeView = id, id = "1"),
         size = size)
  }
  echo <- list(
    grid = list(
      root = list(
        type = "branch",
        data = list(leaf("block_panel-a", 300), leaf("block_panel-b", 700)),
        size = 1000
      ),
      orientation = "HORIZONTAL"
    ),
    activeGroup = "1"
  )
  grid <- as_dock_grid(as_dock_layout(echo))
  expect_equal(grid[["sizes"]], c(0.3, 0.7))
})

test_that("layout_panel_ids and panel_obj_ids are inverse-ish", {
  grid <- resolve_grid(
    dock_grid("a", "b"),
    panel_id_map(c(a = new_dataset_block(), b = new_head_block()), list())
  )
  pids <- layout_panel_ids(grid)
  expect_setequal(pids, c("block_panel-a", "block_panel-b"))
  expect_setequal(panel_obj_ids(pids), c("a", "b"))
})

probe_ext <- function(report = TRUE, title = "Untitled", ...) {
  new_dock_extension(
    server = function(id, ...) function(input, output, session) list(),
    ui = function(id) tagList(),
    name = "Outline",
    class = "probe_extension",
    report = report,
    title = title,
    ...
  )
}

probe_ext_board <- function() {
  new_dock_board(
    blocks = c(a = new_dataset_block()),
    extensions = list(outline = probe_ext())
  )
}

ser_dock_board <- function(board, ...) {
  serialize_board(
    board,
    blocks = list(),
    id = NULL,
    dock = NULL,
    view_data = NULL,
    ...,
    session = NULL
  )
}

ext_payload <- function(ser, name) {
  ser[["payload"]][["extensions"]][["payload"]][[name]][["payload"]]
}

test_that("an all-hidden card survives the save as hidden (#426)", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    views = list(Page = "a")
  )

  restored <- function(reported) {

    ser <- isolate(
      serialize_board(
        brd,
        blocks = list(a = list(server = list(visible = reactiveVal(reported)))),
        id = NULL,
        dock = NULL,
        view_data = NULL,
        session = NULL
      )
    )

    back <- jsonlite::fromJSON(
      jsonlite::toJSON(ser, null = "null"),
      simplifyDataFrame = FALSE,
      simplifyMatrix = FALSE
    )

    visible_sections(board_blocks(blockr_deser(back))[["a"]])
  }

  # JSON carries an empty selection as `[]`, which decodes to an empty list --
  # so the answer is normalised to the character shape every reader expects.
  expect_identical(restored(character()), character())

  expect_identical(restored("outputs"), "outputs")

  # A card that has not reported yet stores a value indistinguishable from an
  # absent attribute, and reads back as unset: both sections open.
  expect_setequal(restored(NULL), c("inputs", "outputs"))
})

test_that("extension state reaches the serialized board", {

  ser <- ser_dock_board(
    probe_ext_board(),
    actions = list(),
    extensions = list(
      outline = list(state = list(report = FALSE, title = "Q3"))
    )
  )

  expect_identical(
    ext_payload(ser, "outline"),
    list(report = FALSE, title = "Q3")
  )
})

test_that("sibling plugin arguments are not walked as extensions", {

  ser <- ser_dock_board(
    probe_ext_board(),
    actions = list(state = list(title = "not an extension")),
    extensions = list(outline = list(state = list(title = "mine")))
  )

  expect_identical(ext_payload(ser, "outline"), list(title = "mine"))
  expect_null(ser[["payload"]][["extensions"]][["payload"]][["actions"]])
})

test_that("reactive extension state is evaluated on serialization", {

  isolate({
    ser <- ser_dock_board(
      probe_ext_board(),
      actions = list(),
      extensions = list(
        outline = list(state = list(title = reactiveVal("live")))
      )
    )
    expect_identical(ext_payload(ser, "outline"), list(title = "live"))
  })
})

test_that("extension state round-trips back into a restored extension", {

  ser <- ser_dock_board(
    probe_ext_board(),
    actions = list(),
    extensions = list(
      outline = list(state = list(report = FALSE, title = "Q3"))
    )
  )

  des <- blockr_deser(ser[["payload"]][["extensions"]])

  expect_s3_class(des[["outline"]], "probe_extension")
  expect_false(des[["outline"]][["report"]])
  expect_identical(des[["outline"]][["title"]], "Q3")
})

test_that("a stateless extension still serializes its constructor", {

  brd <- probe_ext_board()

  expect_ctor_only <- function(ser) {
    node <- ser[["payload"]][["extensions"]][["payload"]][["outline"]]
    expect_length(node[["payload"]], 0L)
    expect_false(is.null(node[["constructor"]]))
  }

  expect_ctor_only(ser_dock_board(brd, actions = list(), extensions = list()))
  expect_ctor_only(ser_dock_board(brd, actions = list()))
})
