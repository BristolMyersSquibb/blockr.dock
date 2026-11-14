# The fixtures of boards that dock app scripts build, in inst/board/fixtures,
# which both test suites read, as core's are.
# Each is a board as constructor calls, here, which gives every id, so that
# none is drawn at random, and the JSON board that `blockr_ser()` writes for it,
# with a description beside it.
# R's suite checks the JSON board, and the client's opens it with the dock
# plugin's class and keeps the dock's state as it is.
# The JSON board records the versions of the packages that wrote it, which
# R's suite does not check, so that a new version of a package leaves the
# fixtures as they are.
# To write the fixtures again after a change, run write_board_fixtures() with
# the package loaded by pkgload::load_all().

board_fixture_dir <- function() {
  system.file("board", "fixtures", package = "blockr.dock")
}

board_fixtures <- function() {
  sub("\\.json$", "", list.files(board_fixture_dir(), pattern = "\\.json$"))
}

board_fixture <- function(name) {
  file <- file.path(board_fixture_dir(), paste0(name, ".json"))
  paste(readLines(file, encoding = "UTF-8", warn = FALSE), collapse = "\n")
}

# The JSON that R writes for a value, as core's `encode_message()` writes it:
# every vector of length one bare, unless `I()` marks it as an array.
write_json <- function(x, pretty = FALSE) {
  yyjsonr::write_json_str(
    x,
    opts = yyjsonr::opts_write_json(auto_unbox = TRUE, pretty = pretty)
  )
}

# Reads JSON keeping every array an array, as a length-one vector marked with
# `I()`, and every number a double, since JSON tells no integer from a double,
# so that two texts read identically only where they hold the same JSON.
read_json_strictly <- function(x) {

  as_double <- function(x) {

    if (is.list(x)) {
      x[] <- lapply(x, as_double)
    } else if (is.integer(x)) {
      storage.mode(x) <- "double"
    }

    x
  }

  as_double(
    yyjsonr::read_json_str(
      x,
      opts = yyjsonr::opts_read_json(
        obj_of_arrs_to_df = FALSE,
        arr_of_objs_to_df = FALSE,
        arr_of_arrs_to_matrix = FALSE,
        length1_array_asis = TRUE,
        int64 = "double",
        num_specials = "string"
      )
    )
  )
}

# The R values of each fixture, by its file name: a description and the board
# as constructor calls.
board_fixture_values <- function() {
  list(
    dock = list(
      description = paste(
        "A board that a dock app script builds with new_board() and the dock",
        "plugin, with every id given, so that none is drawn at random. The",
        "dock plugin's state holds two views and a folder. The view main sits",
        "at the top and has the index a0, the folder more sits at the top",
        "after it with a1, and the view side sits in the folder more with a0.",
        "The grid of main holds a split of one tab group, whose one share R",
        "writes as an array by the schema of the dock's state, and a rail on",
        "the left edge with the panel of core's Edit board plugin. The script",
        "names that plugin edit in the board's plugins, and the state names",
        "it by the id of its class, blockr.core-edit_board_plugin. The plugin",
        "keeps no state, so the board holds none for it. The view side, the",
        "active view, gets the default grid over the one panel that the",
        "script gives it. The card of b shows its output alone. The stack has",
        "no color, and the board holds the value of the board option compact",
        "that the script sets, and no other. The client opens the board with",
        "the dock plugin's class and keeps the dock's state as it is."
      ),
      board = new_board(
        blocks = c(
          a = new_dataset_block("iris", block_name = "Iris"),
          b = new_head_block(n = 3L, block_name = "Head")
        ),
        links = links(ab = new_link("a", "b", "data")),
        stacks = stacks(s1 = new_stack(c("a", "b"), "Input")),
        options = new_compact_option(TRUE),
        plugins = list(
          new_dock_plugin(
            views = list(
              main = dock_view(
                dock_grid(
                  group(panels("a", "b", active = "b")),
                  rail(plg("edit"), collapsed = TRUE, size = 300)
                ),
                name = "Main"
              ),
              side = dock_view("b", folder = "more")
            ),
            folders = list(more = dock_folder("More")),
            active = "side",
            visible = list(b = "outputs")
          ),
          edit = new_edit_board_plugin()
        ),
        id = "curious_otter",
        name = "Curious otter"
      )
    )
  )
}

write_board_fixtures <- function(dir = file.path("inst", "board",
                                                 "fixtures")) {

  values <- board_fixture_values()

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  for (name in names(values)) {

    fixture <- c(
      list(description = values[[name]]$description),
      blockr_ser(values[[name]]$board)
    )

    writeLines(
      write_json(fixture, pretty = TRUE),
      file.path(dir, paste0(name, ".json")),
      useBytes = TRUE
    )
  }

  invisible(dir)
}
