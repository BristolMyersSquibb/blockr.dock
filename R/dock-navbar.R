#' Navbar items
#'
#' The navbar of a dock board is drawn from a `navbar_items` collection, the
#' dock's own controls included. An item is created by `navbar_item()`, items
#' are combined with `c()`, `[` reorders or drops them by id and `[[` extracts
#' one. An app hands the navbar to [blockr.core::serve()] as `navbar`, a
#' function of the board and its plugins that returns the items, as it does
#' `plugins` and `options`.
#'
#' The default items, returned by `default_navbar_items()`, are, in order: the
#' piece the `preserve_board` plugin draws (id `"preserve_board"`), a spacer
#' (`"spacer"`), the busy indicator (`"busy"`), the view menu (`"views"`), the
#' read-only indicator (`"read_only"`), drawn only on a locked board, and the
#' board options (`"options"`). The generic `blockr_app_navbar()` gives the
#' default by board class, and `custom_navbar()` returns a navbar function that
#' appends a fixed set of items to it, dropping any default item that shares an
#' id with one of them. A plugin's piece is placed by `plugin_navbar_item()`,
#' which the defaults use for the `preserve_board` plugin.
#'
#' An item whose `ui` draws a single [shiny::uiOutput()] or
#' [shiny::textOutput()] takes no room in the bar while that output is empty,
#' as an avatar would with nobody signed in.
#'
#' @param id Item id, unique within the navbar
#' @param ui A function called as `ui(id, board)` with `id` as
#' `NS(board_id, item_id)` and the board, returning the item's UI, or `NULL` to
#' leave the item out of the bar
#' @param server `NULL` or a function called as `server(id, board)` with the
#' item id and the board's reactive values, once per session, from the board
#' server, so that a [shiny::moduleServer()] it starts shares the UI's namespace
#' @param fill Whether the item takes up the free space of the bar
#' @param shrink Whether the item gives way first on a narrow bar. The items
#' with neither `fill` nor `shrink` keep their width.
#'
#' @examples
#' help <- navbar_item(
#'   "help",
#'   function(id, board) {
#'     shiny::tags$a(
#'       href = "https://bristolmyerssquibb.github.io/blockr.dock/",
#'       "Help"
#'     )
#'   }
#' )
#'
#' brd <- new_dock_board()
#' items <- c(default_navbar_items(brd, blockr.core::board_plugins(brd)), help)
#' names(items)
#' names(items[c("help", "options")])
#'
#' if (interactive()) {
#'   blockr.core::serve(brd, navbar = custom_navbar(help))
#' }
#'
#' @return The constructor `navbar_item()` returns a `navbar_item` object, and
#' so does `plugin_navbar_item()`, unless its plugin is `NULL`, which returns
#' `NULL`. The functions `default_navbar_items()` and `blockr_app_navbar()`
#' return a `navbar_items` collection, as do `c()` and `[` on items, and
#' `custom_navbar()` returns a function of the board and its plugins that
#' returns one. The checks `is_navbar_item()` and `is_navbar_items()` return a
#' boolean.
#'
#' @rdname navbar
#' @export
navbar_item <- function(id, ui, server = NULL, fill = FALSE, shrink = FALSE) {
  validate_navbar_item(
    structure(
      list(id = id, ui = ui, server = server, fill = fill, shrink = shrink),
      class = "navbar_item"
    )
  )
}

#' @param x Object. For `default_navbar_items()` and `blockr_app_navbar()`, the
#' board, and for `custom_navbar()`, the items to append: a `navbar_item`, a
#' `navbar_items` collection or a list of items.
#' @rdname navbar
#' @export
is_navbar_item <- function(x) {
  inherits(x, "navbar_item")
}

validate_navbar_item <- function(x) {

  if (!is_navbar_item(x) || !is.list(x)) {
    blockr_abort(
      "Expecting a navbar item to inherit from `navbar_item` and be a list.",
      class = "navbar_item_structure_invalid"
    )
  }

  if (!is_string(x[["id"]]) || !nzchar(x[["id"]])) {
    blockr_abort(
      "Expecting a navbar item id to be a non-empty string.",
      class = "navbar_item_id_invalid"
    )
  }

  if (!is.function(x[["ui"]])) {
    blockr_abort(
      "Expecting a navbar item UI to be a function.",
      class = "navbar_item_ui_invalid"
    )
  }

  if (!is.null(x[["server"]]) && !is.function(x[["server"]])) {
    blockr_abort(
      "Expecting a navbar item server to be `NULL` or a function.",
      class = "navbar_item_server_invalid"
    )
  }

  if (!is_bool(x[["fill"]]) || !is_bool(x[["shrink"]])) {
    blockr_abort(
      "Expecting `fill` and `shrink` of a navbar item to be booleans.",
      class = "navbar_item_flex_invalid"
    )
  }

  x
}

#' @rdname navbar
#' @export
is_navbar_items <- function(x) {
  inherits(x, "navbar_items")
}

new_navbar_items <- function(x = list()) {
  validate_navbar_items(structure(unname(x), class = "navbar_items"))
}

validate_navbar_items <- function(x) {

  if (!is_navbar_items(x) || !is.list(x)) {
    blockr_abort(
      "Expecting navbar items to inherit from `navbar_items` and be a list.",
      class = "navbar_items_structure_invalid"
    )
  }

  if (!all(lgl_ply(x, is_navbar_item))) {
    blockr_abort(
      "Expecting navbar items to contain `navbar_item` objects only.",
      class = "navbar_items_contents_invalid"
    )
  }

  for (item in x) {
    validate_navbar_item(item)
  }

  ids <- names(x)
  dups <- unique(ids[duplicated(ids)])

  if (length(dups)) {
    blockr_abort(
      "Navbar item ids must be unique; duplicated: {dups}.",
      class = "navbar_items_ids_invalid"
    )
  }

  x
}

as_navbar_items <- function(x) {

  if (is_navbar_items(x)) {
    validate_navbar_items(x)
  } else if (is_navbar_item(x)) {
    new_navbar_items(list(x))
  } else if (is.list(x) && is.null(attr(x, "class"))) {
    new_navbar_items(x)
  } else {
    blockr_abort(
      "Cannot use an object of class {class(x)} as navbar items.",
      class = "navbar_items_coercion_invalid"
    )
  }
}

#' @export
names.navbar_items <- function(x) {
  chr_xtr(x, "id")
}

#' @export
c.navbar_item <- function(...) {

  items <- lapply(
    Filter(Negate(is.null), list(...)),
    function(x) unclass(as_navbar_items(x))
  )

  new_navbar_items(coal(unlst(items), list()))
}

#' @export
c.navbar_items <- c.navbar_item

#' @export
`[.navbar_items` <- function(x, i, ...) {

  if (missing(i)) {
    return(x)
  }

  if (is.character(i)) {
    i <- navbar_item_pos(x, i)
  }

  new_navbar_items(unclass(x)[i])
}

#' @export
`[[.navbar_items` <- function(x, i, ...) {

  if (is.character(i)) {
    i <- navbar_item_pos(x, i)
  }

  unclass(x)[[i]]
}

navbar_item_pos <- function(x, ids) {

  unknown <- setdiff(ids, names(x))

  if (length(unknown)) {
    blockr_abort(
      "Unknown navbar item{?s} {unknown}.",
      class = "navbar_items_subset_invalid"
    )
  }

  match(ids, names(x))
}

#' @param plugin A board plugin, or `NULL`
#' @rdname navbar
#' @export
plugin_navbar_item <- function(plugin) {

  if (is.null(plugin)) {
    return(NULL)
  }

  stopifnot(is_plugin(plugin))

  # With the plugin's id as its own, the item is drawn under the namespace core
  # serves the plugin under, so the plugin needs no server of the item's.
  navbar_item(
    plugin_id(plugin),
    coal(plugin_ui(plugin), function(id, board) NULL),
    shrink = TRUE
  )
}

#' @param plugins Board plugins
#' @rdname navbar
#' @export
default_navbar_items <- function(x, plugins) {

  # A spacer item rather than `fill` on the plugin's piece, so that a board
  # without the plugin keeps the rest of the bar on the right.
  c(
    new_navbar_items(),
    if ("preserve_board" %in% names(plugins)) {
      plugin_navbar_item(plugins[["preserve_board"]])
    },
    navbar_item(
      "spacer",
      function(id, board) tags$span(class = "blockr-navbar-spacer"),
      fill = TRUE
    ),
    navbar_item("busy", busy_navbar_ui),
    navbar_item("views", views_navbar_ui),
    navbar_item("read_only", read_only_navbar_ui),
    navbar_item("options", options_navbar_ui)
  )
}

#' @rdname navbar
#' @export
blockr_app_navbar <- function(x, plugins) {
  UseMethod("blockr_app_navbar")
}

#' @export
blockr_app_navbar.dock_board <- function(x, plugins) {
  default_navbar_items(x, plugins)
}

#' @rdname navbar
#' @export
custom_navbar <- function(x) {

  custom <- as_navbar_items(x)

  function(x, plugins) {

    default <- blockr_app_navbar(x, plugins)

    c(default[setdiff(names(default), names(custom))], custom)
  }
}

# The items a navbar function returns, as both app methods resolve it from the
# board and plugins they are given, so that the page and the server see the
# same items.
resolve_navbar <- function(navbar, x, plugins) {

  if (!is.function(navbar)) {
    blockr_abort(
      "Expecting `navbar` to be a function of the board and its plugins.",
      class = "navbar_fun_invalid"
    )
  }

  res <- navbar(x, plugins)

  if (!is_navbar_items(res)) {
    blockr_abort(
      "Expecting the navbar function to return a `navbar_items` collection, ",
      "but got {class(res)} instead.",
      class = "navbar_items_structure_invalid"
    )
  }

  validate_navbar_items(res)
}

navbar_ui <- function(id, items, board) {
  div(
    class = "blockr-navbar",
    style = sprintf("--blockr-spinner-delay: %dms;", spinner_delay_ms()),
    lapply(items, navbar_item_ui, id, board)
  )
}

# An item whose `ui` returns `NULL` is left out of the bar. The wrapper of one
# whose output renders empty collapses in CSS instead, since flexbox would count
# its empty box in the bar's `gap`.
navbar_item_ui <- function(item, id, board) {

  ui <- item[["ui"]](NS(id, item[["id"]]), board)

  if (is.null(ui)) {
    return(NULL)
  }

  div(
    class = c(
      "blockr-navbar-item",
      if (item[["fill"]]) "blockr-navbar-item-fill",
      if (item[["shrink"]]) "blockr-navbar-item-shrink"
    ),
    `data-navbar-item` = item[["id"]],
    ui
  )
}

# Run once per session from the board server callback, under the board's
# namespace, so that an item's server and its UI share `NS(board_id, item_id)`.
navbar_server <- function(items, board) {

  for (item in items) {
    if (not_null(item[["server"]])) {
      item[["server"]](item[["id"]], board)
    }
  }

  invisible()
}

# The dock's own items keep their logic in the board server, so the markup that
# server addresses, such as the view menu's input, carries the board's namespace
# rather than the item's: drop the item's part of the id.
navbar_board_id <- function(id) {
  sub(paste0(ns.sep, "[^", ns.sep, "]*$"), "", id)
}

# Busy spinner. Always rendered and always visible, driven purely by CSS off
# the `.shiny-busy` class Shiny toggles on <html> during a flush -- no server
# observer. Idle it is a faint, closed ring; a flush scoped to real block
# evaluation (a bare panel switch does not qualify) paints a darker arc onto it
# and spins it. It sits ahead of the view menu, where its 16px ring is not
# juxtaposed against the smaller options button; because it is always painted
# (never shown/hidden) its constant slot shifts no neighbour. The busy
# appearance is held for `--blockr-spinner-delay` ms (set on the navbar), so a
# sub-threshold flush never flickers it. The ring spins inside a static slot
# that carries a hover tooltip naming the state (idle / computing), so the label
# does not turn with it. Announced like the lock indicator.
busy_navbar_ui <- function(id, board) {
  tags$span(
    class = "blockr-navbar-spinner-slot",
    tags$span(
      class = "blockr-navbar-spinner",
      role = "status",
      `aria-label` = "Busy"
    )
  )
}

# View menu in the navbar -- always present, since boards always carry a
# `dock_views` collection (single-view boards have one auto-named "Page" view).
# The menu needs only structure (ids, names, active), not geometry. A short rule
# sets it apart from the controls after it.
views_navbar_ui <- function(id, board) {
  tagList(
    view_nav_ui(navbar_board_id(id), board_views(board)),
    tags$span(class = "blockr-navbar-rule", `aria-hidden` = "true")
  )
}

read_only_navbar_ui <- function(id, board) {

  if (!is_dock_locked()) {
    return(NULL)
  }

  tags$span(
    class = "blockr-lock-indicator",
    `data-blockr-tooltip` = "Editing is disabled by this deployment.",
    `aria-label` = "Read-only mode",
    role = "status",
    bsicons::bs_icon("lock-fill"),
    tags$span(
      class = "blockr-lock-indicator-label",
      "Read-only"
    )
  )
}

# Pure-JS open trigger via `data-blockr-sidebar-target`. The settings sidebar's
# body is pre-rendered into its mount (see board_ui.dock_board()), so no server
# observer is needed: clicking the button toggles the panel client-side. A plain
# `tags$button` rather than an `actionButton`, since there is no `input$<id>` to
# wire.
options_navbar_ui <- function(id, board) {
  tags$button(
    type = "button",
    class = "btn action-button blockr-navbar-icon-btn",
    `data-blockr-sidebar-target` = NS(navbar_board_id(id), "settings_sidebar"),
    `aria-label` = "Board options",
    `data-blockr-tooltip` = "Board options",
    # The side panel it opens, not a gear: the gear stands for a block's
    # settings everywhere else.
    bsicons::bs_icon("layout-sidebar-reverse")
  )
}
