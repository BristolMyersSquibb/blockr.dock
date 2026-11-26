#' The dock plugin
#'
#' The dock is a frontend, the plugin that draws the page.
#' Its client draws the page as a React app, with dockview for the layout.
#' The constructor `new_dock_plugin()` makes the dock plugin, of class
#' `c("dock_plugin", "frontend_plugin", "plugin")`, whose plugin class has the
#' id `blockr.dock-dock_plugin` (see [blockr.core::new_plugin()]).
#' The dock keeps its state in the board: its views, the folders that group
#' them, the active view and the sections that each block's card shows.
#' The schema of that state ships beside the class's module, as
#' `js/dock.schema.json`, and R writes the state by it.
#'
#' The constructor writes the state that an app script gives, once, as the
#' script runs, and the script gives the plugin to [blockr.core::new_board()]
#' in `plugins` (see [board_views()]):
#'
#' * Each view in `views`, written with [dock_view()], gets its grid, or the
#'   default grid over the panels that it is given.
#'   A view's id is its name in the list, and R draws the id of each view that
#'   the list leaves unnamed, in the shape that the blockr option `rand_id`
#'   names (see [blockr.core::rand_names()]).
#'   A view without a name is named after its id.
#' * Each folder in `folders`, written with [dock_folder()], has its name in
#'   the list as its id, or an id that R draws.
#' * Each view and folder gets an `index`, which orders it among the views and
#'   folders of the folder that holds it.
#'   In each folder, and at the top, the views come first and the folders
#'   after them, each in the order that the script gives.
#' * The view that `active` names is the one that the board opens on.
#'   Where `active` names none, it is the first view of the views menu.
#' * The argument `visible` gives the sections that a block's card shows, by
#'   the block's id: `"inputs"`, `"outputs"` or neither.
#'   A card without an entry shows both, so R drops an entry that names both.
#'
#' Where the script gives no views, folders or `visible`, the plugin holds no
#' state, and a board that holds it holds none for the dock.
#' The client then gives the board the class's default, which holds no view,
#' and the dock adds a view as it starts.
#' In a grid, a bare id names a block, and [plg()] names a plugin by the
#' name that the script gives it in the board's `plugins`, or by the id of its
#' class.
#'
#' The constructor refuses, naming each, a grid that places a panel twice.
#' A panel on a rail and in a tab group counts as twice.
#' It also refuses a view or folder that names a folder that `folders` lacks,
#' and a folder that sits in itself, through the folders that hold it.
#' Last, it refuses an `active` that names a view that `views` lacks, and a
#' section other than `"inputs"` and `"outputs"`.
#' It knows nothing of the board that holds it, so the dock completes its
#' state as `new_board()` builds the board, and refuses there what only the
#' board can tell, such as a grid that names a block that the board lacks
#' (see [board_views()]).
#'
#' The plugin prints the views menu as a tree, in which each folder shows its
#' name, with what it holds below it, and each view its id, with "(active)"
#' for the active view.
#' Each view follows, after its id, with its grid, in the order of the menu.
#' The print marks the panel of each block whose card hides a section with the
#' sections that the card shows, "i" for its inputs, "o" for its outputs and
#' "n" for neither, and its first line explains the marks.
#' A card that no view places, as where the script gives `visible` and no
#' views, comes on the last line, with its mark.
#' A board that holds the plugin prints it the same way.
#'
#' The package lists the class in the `._BLOCKR_MODULES_` that it exports,
#' with its name, "Dock", and its module, `js/dock.js` (see
#' [blockr.core::read_blocks_yml()]).
#' So [blockr.core::available_plugins()] gives the class wherever blockr.dock
#' is available, and [blockr.core::serve()] runs the dock in place of core's
#' basic frontend.
#' An app script gives the items of the dock's navbar to `serve()` in
#' `navbar`.
#' The dock takes them through its method of [blockr.core::frontend_args()]
#' (see [default_navbar()]).
#'
#' The dock draws its cards, its navbar, its options sidebar and the frame of
#' its panels with the theme variables of blockr.ui, and keeps the default of
#' each, since its look is blockr.ui's.
#' So the class has no method of [blockr.core::frontend_theme()], and takes
#' core's, which returns a theme with no values.
#'
#' The dock reads four board options: its own, `compact`, `view_tabs` and
#' `sync_rails` (see [new_compact_option()]), and core's `dark_mode`.
#' The class names them through its method of [blockr.core::plugin_options()],
#' by their constructors.
#' R sends their ids in its reply to `hello`.
#' So the client gives every board that lacks a value of one of them its
#' default as the board opens, before the dock draws it.
#' The package lists each of its own board options in `._BLOCKR_MODULES_`
#' too, with its module, as `js/compact-option.js`, beside which the schema of
#' its value ships, as `js/compact-option.schema.json`.
#'
#' @param views The views, each written with [dock_view()], by id, or `NULL`
#'   for none.
#' @param folders The folders, each written with [dock_folder()], by id.
#' @param active The id of the view that the board opens on, or `NULL` for the
#'   first view of the views menu.
#' @param visible The sections that each block's card shows, by the block's
#'   id, as `list(a = "outputs")`.
#'
#' @return A `plugin` object, as [blockr.core::new_plugin()] returns it.
#'
#' @examples
#' new_dock_plugin()
#'
#' dock <- new_dock_plugin(
#'   views = list(
#'     overview = dock_view(dock_grid("a", "b"), name = "Overview"),
#'     ae = dock_view("c", folder = "safety")
#'   ),
#'   folders = list(safety = dock_folder("Safety")),
#'   visible = list(b = "outputs")
#' )
#'
#' dock
#'
#' names(blockr.core::plugin_state(dock))
#'
#' @export
new_dock_plugin <- function(views = NULL, folders = list(), active = NULL,
                            visible = list()) {
  new_plugin(
    dock_state(views, folders, active, visible),
    "dock_plugin",
    c("dock_plugin", "frontend_plugin")
  )
}

# The board options that the dock reads, by their constructors: the dock's
# own, and core's `dark_mode`.
# R sends their ids in the class's entry of its reply to `hello`.
# The client puts them on the board as the board opens.
#' @export
plugin_options.dock_plugin <- function(x, ...) {
  list(
    new_compact_option,
    new_view_tabs_option,
    new_sync_rails_option,
    new_dark_mode_option
  )
}

# The print shows the views menu as a tree, in which a folder shows its name
# and a view its id, and then each view in the menu's order, after its id.
# The menu orders the views and folders of each folder by their `index`, and
# the method compares the keys by their bytes, as the client compares their
# code units.
# The collation of a locale would put "aa" before "aA".
# The first line explains the marks of the sections that a card shows, where
# a card hides one, and the cards that no view places come on the last line.
#' @export
format.dock_plugin <- function(x, ...) {

  state <- plugin_state(x)

  views <- state[["views"]]
  folders <- state[["folders"]]
  visible <- state[["visible"]]

  view_holders <- chr_ply(views, holder_of)
  folder_holders <- chr_ply(folders, holder_of)

  # What sits in the folder `folder`, or at the top for "", in the order of
  # the views menu: each view as its id, and each folder as a list of its id
  # and of what it holds.
  menu <- function(folder) {

    in_views <- view_holders == folder
    in_folders <- folder_holders == folder

    items <- c(
      as.list(names(views)[in_views]),
      lapply(
        names(folders)[in_folders],
        function(id) list(id = id, items = menu(id))
      )
    )

    index <- c(
      chr_xtr(views[in_views], "index"),
      chr_xtr(folders[in_folders], "index")
    )

    items[order(index, method = "radix")]
  }

  # The lines of the views menu, as a tree.
  menu_lines <- function(items) {
    format_tree(
      lapply(
        items,
        function(item) {

          if (is.character(item)) {
            return(
              paste0(
                item,
                grid_attrs(if (identical(item, state[["active"]])) "active")
              )
            )
          }

          c(
            format(dock_folder(folders[[item[["id"]]]][["name"]])),
            menu_lines(item[["items"]])
          )
        }
      )
    )
  }

  # The ids of the views of the menu, depth first.
  menu_views <- function(items) {
    unlst(
      lapply(
        items,
        function(item) {
          if (is.character(item)) item else menu_views(item[["items"]])
        }
      )
    )
  }

  items <- menu("")

  dock_views <- lapply(
    views,
    function(view) {
      new_dock_view(
        grid = grid_from_state(view[["grid"]]),
        name = view[["name"]]
      )
    }
  )

  view_lines <- lapply(
    menu_views(items),
    function(id) {
      lines <- format(dock_views[[id]], visible = visible)
      lines[1L] <- paste0(id, lines[1L])
      lines
    }
  )

  unplaced <- setdiff(names(visible), unlst(lapply(dock_views, view_blocks)))

  c(
    paste0(
      "<", class(x)[1L], "> ", plugin_class_id(x),
      if (length(visible)) {
        " (sections: i inputs, o outputs, n neither, unmarked both)"
      }
    ),
    menu_lines(items),
    unlst(view_lines),
    if (length(unplaced)) {
      paste0(
        "cards in no view: ",
        paste0(
          unplaced,
          chr_ply(
            unplaced,
            function(id) grid_attrs(sections = section_mark(id, visible))
          ),
          collapse = ", "
        )
      )
    }
  )
}

#' @rdname new_dock_plugin
#' @format NULL
#' @usage NULL
#' @export
._BLOCKR_MODULES_ <- list( # nolint: object_name_linter.
  plugins = list(
    new_dock_plugin = list(
      id = "dock_plugin",
      name = "Dock",
      module = "js/dock.js"
    )
  ),
  options = list(
    compact = list(module = "js/compact-option.js"),
    view_tabs = list(module = "js/view-tabs-option.js"),
    sync_rails = list(module = "js/sync-rails-option.js")
  )
)
