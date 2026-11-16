test_that("the dock plugin is a frontend of its own class", {

  x <- new_dock_plugin()

  expect_s3_class(
    x,
    c("dock_plugin", "frontend_plugin", "plugin"),
    exact = TRUE
  )
  expect_identical(plugin_class_id(x), "blockr.dock-dock_plugin")
})

test_that("the dock's state has version 1 and a schema beside its module", {

  expect_identical(plugin_version(new_dock_plugin()), 1L)

  schema <- read_json_schema("js/dock.schema.json", package = "blockr.dock")

  expect_named(
    schema[["properties"]],
    c("views", "folders", "active", "visible")
  )
})

test_that("core finds the dock's plugin class in the package's listing", {

  plugins <- available_plugins()

  expect_identical(
    plugins[plugins[["package"]] == "blockr.dock", "id"],
    "dock_plugin"
  )
})

test_that("the dock plugin holds no state where the script gives none", {

  expect_length(plugin_state(new_dock_plugin()), 0L)
  expect_length(
    plugin_state(new_dock_plugin(list(), folders = NULL, visible = NULL)),
    0L
  )
})

test_that("the state holds each view's grid, name, folder and index", {

  dock <- new_dock_plugin(
    views = list(
      overview = dock_view(
        dock_grid(c("a", "b"), plg("blockr.dag-dag_plugin"), sizes = c(3, 7)),
        name = "Overview"
      ),
      ae = dock_view("d", name = "Adverse events", folder = "safety")
    ),
    folders = list(safety = dock_folder("Safety")),
    active = "overview",
    visible = list(b = "outputs")
  )

  dag <- list(plugin = "blockr.dag-dag_plugin")

  expect_identical(
    plugin_state(dock),
    list(
      views = list(
        overview = list(
          name = "Overview",
          folder = NULL,
          index = "a0",
          grid = list(
            orientation = "horizontal",
            children = list(
              list(
                panels = list(list(block = "a"), list(block = "b")),
                active = list(block = "a")
              ),
              list(panels = list(dag), active = dag)
            ),
            sizes = c(0.3, 0.7)
          )
        ),
        ae = list(
          name = "Adverse events",
          folder = "safety",
          index = "a0",
          grid = list(
            orientation = "horizontal",
            children = list(
              list(panels = list(list(block = "d")), active = list(block = "d"))
            ),
            sizes = 1
          )
        )
      ),
      folders = list(
        safety = list(name = "Safety", folder = NULL, index = "a1")
      ),
      active = "overview",
      visible = list(b = "outputs")
    )
  )
})

test_that("R draws the id of each view and folder that the script leaves out", {

  withr::local_options(blockr.rand_id = "letters_8")

  state <- plugin_state(
    new_dock_plugin(
      views = list(dock_view("a"), main = dock_view("b")),
      folders = list(dock_folder("F"))
    )
  )

  ids <- names(state$views)

  expect_match(ids[[1L]], "^[a-z]{8}$")
  expect_identical(ids[[2L]], "main")
  expect_match(names(state$folders), "^[a-z]{8}$")

  # A view without a name is named after its id, and the first view is active.
  expect_identical(state$views[[1L]]$name, id_to_sentence_case(ids[[1L]]))
  expect_identical(state$active, ids[[1L]])
})

test_that("a view without a name is named after its id", {

  state <- plugin_state(
    new_dock_plugin(views = list(adverse_events = dock_view("a")))
  )

  expect_identical(state$views$adverse_events$name, "Adverse events")

  # An id from which the name would hold nothing but spaces names the view.
  state <- plugin_state(new_dock_plugin(views = list(`_` = dock_view("a"))))
  expect_identical(state$views$`_`$name, "_")
})

test_that("the views of a folder come before its folders, in order", {

  state <- plugin_state(
    new_dock_plugin(
      views = list(
        v1 = dock_view("a"),
        v2 = dock_view("b", folder = "f1"),
        v3 = dock_view("c"),
        v4 = dock_view("d", folder = "f1")
      ),
      folders = list(
        f1 = dock_folder("F1"),
        f2 = dock_folder("F2", folder = "f1"),
        f3 = dock_folder("F3")
      )
    )
  )

  expect_identical(
    chr_xtr(state$views, "index"),
    c("a0", "a0", "a1", "a1")
  )
  expect_identical(chr_xtr(state$folders, "index"), c("a2", "a2", "a3"))
})

test_that("the first view of the views menu is active where none is named", {

  state <- plugin_state(
    new_dock_plugin(
      views = list(inner = dock_view("a", folder = "f"), top = dock_view("b")),
      folders = list(f = dock_folder("F"))
    )
  )

  expect_identical(state$active, "top")

  # The first view can sit in a folder, inside another.
  state <- plugin_state(
    new_dock_plugin(
      views = list(inner = dock_view("a", folder = "g")),
      folders = list(f = dock_folder("F"), g = dock_folder("G", folder = "f"))
    )
  )

  expect_identical(state$active, "inner")

  # Without views, no view is active, and the dock adds one as it starts.
  state <- plugin_state(new_dock_plugin(visible = list(a = "inputs")))

  expect_identical(state$views, set_names(list(), character()))
  expect_true("active" %in% names(state))
  expect_null(state$active)
})

test_that("a view given panels gets the default grid over them", {

  state <- plugin_state(
    new_dock_plugin(
      views = list(v = dock_view(list("a", plg("x"), blk("b"), plg("y"))))
    )
  )

  expect_identical(
    state$views$v$grid,
    list(
      orientation = "horizontal",
      children = list(
        list(
          panels = list(list(plugin = "x"), list(plugin = "y")),
          active = list(plugin = "x")
        ),
        list(
          panels = list(list(block = "a"), list(block = "b")),
          active = list(block = "a")
        )
      ),
      sizes = c(0.5, 0.5)
    )
  )

  # A view given no panels shows nothing.
  state <- plugin_state(new_dock_plugin(views = list(v = dock_view(NULL))))

  expect_identical(
    state$views$v$grid,
    list(orientation = "horizontal", children = list(), sizes = numeric())
  )
})

test_that("the state holds the rails of a grid, without their positions", {

  state <- plugin_state(
    new_dock_plugin(
      views = list(
        v = dock_view(dock_grid("a", rail(plg("x"), "b", collapsed = TRUE)))
      )
    )
  )

  expect_identical(
    state$views$v$grid$rails,
    list(
      left = list(
        panels = list(list(plugin = "x"), list(block = "b")),
        active = list(plugin = "x"),
        collapsed = TRUE,
        size = 500
      )
    )
  )
})

test_that("visible lists only the cards that hide a section", {

  state <- plugin_state(
    new_dock_plugin(
      visible = list(
        a = "outputs",
        b = c("outputs", "inputs"),
        c = character(),
        d = c("inputs", "inputs")
      )
    )
  )

  expect_identical(
    state$visible,
    list(a = "outputs", c = character(), d = "inputs")
  )
})

test_that("the constructor refuses a grid that places a panel twice", {

  expect_error(
    new_dock_plugin(views = list(v = dock_view(dock_grid("a", rail("a"))))),
    "The grid of the view v places the block a more than once.",
    fixed = TRUE,
    class = "dock_grid_panel_duplicated"
  )

  expect_error(
    new_dock_plugin(
      views = list(
        v = dock_view(dock_grid("a", blk("a"), plg("x"), panels(plg("x")))),
        w = dock_view(c("b", "b"))
      )
    ),
    paste(
      "The grid of the view v places the block a and the plugin x more than",
      "once. The grid of the view w places the block b more than once."
    ),
    fixed = TRUE,
    class = "dock_grid_panel_duplicated"
  )
})

test_that("the constructor refuses a folder it lacks or one in itself", {

  expect_error(
    new_dock_plugin(views = list(v = dock_view("a", folder = "f"))),
    "while the view v names f.",
    fixed = TRUE,
    class = "dock_folder_unknown"
  )
  expect_error(
    new_dock_plugin(
      views = list(v = dock_view("a", folder = "f")),
      folders = list(g = dock_folder("G", folder = "h"))
    ),
    "while the view v names f and the folder g names h.",
    fixed = TRUE,
    class = "dock_folder_unknown"
  )

  expect_error(
    new_dock_plugin(folders = list(f = dock_folder("F", folder = "f"))),
    "while the folder f does, through the folders that hold it.",
    fixed = TRUE,
    class = "dock_folder_in_itself"
  )
  expect_error(
    new_dock_plugin(
      folders = list(
        f = dock_folder("F", folder = "g"),
        g = dock_folder("G", folder = "f"),
        h = dock_folder("H", folder = "f")
      )
    ),
    "while the folders f and g do,",
    fixed = TRUE,
    class = "dock_folder_in_itself"
  )
})

test_that("the constructor refuses an active view that it lacks", {

  expect_error(
    new_dock_plugin(views = list(v = dock_view("a")), active = "w"),
    "names the view w, which `views` lacks.",
    fixed = TRUE,
    class = "dock_active_invalid"
  )
  expect_error(new_dock_plugin(active = "w"), class = "dock_active_invalid")
  expect_error(
    new_dock_plugin(active = c("a", "b")),
    class = "dock_active_invalid"
  )
})

test_that("the constructor refuses sections that a card lacks", {

  expect_error(
    new_dock_plugin(visible = list(a = "ctrl", b = c("outputs", "plots"))),
    "while `visible` names ctrl and plots.",
    fixed = TRUE,
    class = "dock_visible_invalid"
  )

  expect_error(
    new_dock_plugin(visible = list("outputs")),
    class = "dock_visible_invalid"
  )
  expect_error(
    new_dock_plugin(visible = list(a = "inputs", a = "outputs")),
    class = "dock_visible_invalid"
  )
  expect_error(
    new_dock_plugin(visible = list(a = 1)),
    class = "dock_visible_invalid"
  )
  expect_error(
    new_dock_plugin(visible = c(a = "outputs")),
    class = "dock_visible_invalid"
  )
})

test_that("the constructor takes views and folders as written, once each", {

  expect_error(
    new_dock_plugin(views = list(v = dock_view("a"), v = dock_view("b"))),
    "while v names more than one.",
    fixed = TRUE,
    class = "dock_nav_ids_invalid"
  )
  expect_error(
    new_dock_plugin(folders = list(f = dock_folder("F"), f = dock_folder("G"))),
    class = "dock_nav_ids_invalid"
  )

  expect_error(
    new_dock_plugin(views = dock_view("a")),
    class = "dock_views_invalid"
  )
  expect_error(
    new_dock_plugin(views = list(dock_view("a"), dock_grid("b"))),
    "its entry 2 is not",
    class = "dock_views_invalid"
  )
  expect_error(
    new_dock_plugin(folders = list("F")),
    class = "dock_folders_invalid"
  )
})

test_that("a view and a folder take a name that holds more than spaces", {

  expect_error(dock_view("a", name = " "), class = "dock_view_name_invalid")
  expect_error(dock_view("a", name = 1), class = "dock_view_name_invalid")
  expect_error(dock_folder(""), class = "dock_view_name_invalid")
  expect_error(dock_view("a", folder = ""), class = "dock_folder_invalid")
  expect_error(dock_folder("F", folder = 1), class = "dock_folder_invalid")

  expect_output(
    print(dock_view("a", name = "A", folder = "f")),
    "<dock_view> A (in f)",
    fixed = TRUE
  )
  expect_identical(
    format(dock_view(dock_grid("a", "b"), name = "A", folder = "f")),
    c(
      "<dock_view> A (in f, horizontal)",
      "\u251c\u2500 50%: a",
      "\u2514\u2500 50%: b"
    )
  )
  expect_identical(
    format(dock_view(dock_grid(), name = "A")),
    "<dock_view> A (empty)"
  )
  expect_output(print(dock_folder("F")), "<dock_folder> F", fixed = TRUE)
})

test_that("the plugin prints its views and folders in the views menu's order", {

  # The script lists the view Lab first, while the views menu lists it last,
  # in the folder Labs inside the folder Safety.
  dock <- new_dock_plugin(
    views = list(
      lab = dock_view(c("e", "f"), folder = "labs"),
      main = dock_view(
        dock_grid(panels("a", "b"), "c", rail(plg("dag")), sizes = c(3, 7)),
        name = "Main"
      ),
      ae = dock_view("d", name = "Adverse events", folder = "safety")
    ),
    folders = list(
      safety = dock_folder("Safety"),
      labs = dock_folder("Labs", folder = "safety")
    ),
    active = "ae",
    visible = list(b = "outputs", c = character())
  )

  expect_snapshot(print(dock))

  expect_identical(
    format(new_dock_plugin()),
    "<dock_plugin> blockr.dock-dock_plugin"
  )
})

test_that("the print marks the sections of each card that hides one", {

  # The card of b shows its outputs alone and that of c its inputs alone,
  # while no view places z, whose card shows neither.
  dock <- new_dock_plugin(
    views = list(
      main = dock_view(
        dock_grid(
          panels("a", plg("edit"), "b", active = "b"),
          rail("c", collapsed = TRUE)
        )
      )
    ),
    visible = list(b = "outputs", c = "inputs", z = character())
  )

  expect_snapshot(print(dock))

  expect_snapshot(print(new_dock_plugin(visible = list(a = "inputs"))))
})

test_that("the print orders the views by their keys in any collation", {

  # The collation of en_US.UTF-8 puts "aa", the key of the 37th view, before
  # "aA", the key of the 11th, while the client compares the keys by their
  # code units.
  skip_on_os("windows")
  withr::local_collate("en_US.UTF-8")

  ids <- sprintf("v%02d", 1:37)
  views <- set_names(rep(list(dock_view("a")), 37L), ids)

  lines <- format(new_dock_plugin(views = views))
  lines <- lines[grepl("<dock_view>", lines, fixed = TRUE)]

  expect_identical(lines, paste0(ids, "<dock_view> ", toupper(ids)))
})

test_that("a board prints the dock plugin that it holds", {

  dock <- new_dock_plugin(views = list(main = dock_view("a")))

  board <- new_board(
    blocks = c(a = new_dataset_block("iris")),
    plugins = list(dock)
  )

  expect_output(
    print(board),
    paste(format(dock), collapse = "\n"),
    fixed = TRUE
  )
})

test_that("view_name() reads the name of a view", {

  expect_identical(view_name(dock_view("a", name = "Main")), "Main")
  expect_null(view_name(dock_view("a")))
  expect_error(view_name(list()), class = "dock_view_invalid")
})

test_that("view_blocks() reads the blocks of a view in the grid's order", {

  grid <- dock_grid(
    "c",
    group(panels(blk("a"), plg("dag")), "d"),
    rail(plg("blockr.dag-dag_plugin")),
    rail("b", position = "right")
  )

  expect_identical(view_blocks(dock_view(grid)), c("c", "a", "d", "b"))

  expect_identical(
    view_blocks(dock_view(list(plg("dag"), "b", blk("a")))),
    c("b", "a")
  )

  expect_identical(view_blocks(dock_view(plg("dag"))), character())
  expect_identical(view_blocks(dock_view(list())), character())

  expect_error(view_blocks(list()), class = "dock_view_invalid")
})

test_that("the index keys are those that fractional-indexing gives", {

  # The keys that fractional-indexing 4.0.0 gives for
  # generateNKeysBetween(null, null, n).
  expect_identical(index_keys(0L), character())

  keys <- index_keys(64L)

  expect_identical(
    keys[c(1L, 10L, 11L, 36L, 37L, 62L, 63L, 64L)],
    c("a0", "a9", "aA", "aZ", "aa", "az", "b00", "b01")
  )
  expect_identical(index_keys(3907L)[3906:3907], c("bzz", "c000"))

  # The client compares the keys by their code units.
  expect_identical(sort(keys, method = "radix"), keys)
})
