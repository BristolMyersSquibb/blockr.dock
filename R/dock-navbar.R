#' Navbar items
#'
#' The bar across the top of the dock holds the navbar items that an app
#' script gives [blockr.core::serve()] in `navbar`, in their order.
#' R hands them to the dock's method of [blockr.core::frontend_args()], which
#' takes `default_navbar()` where the script passes none.
#' The method checks every item as the app starts, and stops the app with an
#' error that names an item that the dock cannot draw.
#' It returns the items in `navbar`, which core sends to the client in its
#' reply to `hello`.
#' So no board and no file holds the bar, and every board that the app opens
#' shows the same one.
#'
#' The default bar, which `default_navbar()` gives, holds the dock's own
#' parts, each under its id, from left to right: the logo (`logo`), the
#' board's name (`name`), the controls of the app's storage plugin
#' (`storage`), Undo and Redo (`undo_redo`), the spacer (`spacer`), which takes
#' the free space, the views menu (`views`), the read-only pill (`read_only`)
#' and the options button (`options`).
#' No function makes a part, so a script takes one from `default_navbar()`.
#' Besides the dock's own parts, an item is one of these:
#'
#' * A link, which `navbar_link()` makes, and which opens in a new tab.
#' * A menu of links and further menus, which `navbar_menu()` makes.
#' * One of the storage plugin's controls, which `navbar_storage()` places
#'   apart from the rest by the control's id.
#'   The `storage` part draws the controls that no such item places.
#' * A module that a package offers, which `navbar_item()` names by its id,
#'   `{pkg}-{name}`, and which draws the item.
#'   R checks that the id is a non-empty string, but not that a package offers
#'   the module, which the client loads as it draws the bar.
#'
#' Items combine with `c()`, `[` reorders or drops them by id or position,
#' and `[[` extracts one, while `names()` gives their ids.
#' The function `custom_navbar()` gives the default bar without the items
#' whose ids `drop` names, with `items` appended.
#' It drops each default item that shares an id with one of `items` too, so
#' that such an item takes the default's place at the end.
#' Ids are unique among the items of the bar, and among the items of each
#' menu.
#'
#' Each item is a list of its `kind`, its `id` and the fields of its kind,
#' which R writes as a JSON object for the client: `part`, `storage`, `link`
#' with `label` and `href`, `menu` with `label` and `items`, and `module` with
#' `module`.
#'
#' @param id The item's id, a non-empty string.
#'   For `navbar_storage()`, the id of the storage plugin's control.
#' @param label The text that the link or the menu shows, a non-empty string
#' @param href The URL that the link opens, a non-empty string
#' @param ... For `navbar_menu()`, the links and menus that it holds, which
#'   combine as with `c()`.
#'   For `frontend_args()`, the other arguments of [blockr.core::serve()],
#'   which the dock refuses.
#' @param module The id of a module that a package offers, as
#'   `"blockr.topline-deck_button"`
#' @param items The items that `custom_navbar()` appends: an item, a
#'   `navbar_items` collection or a list of items
#' @param drop The ids of the default items that `custom_navbar()` leaves out
#' @param x An object.
#'   For `frontend_args()`, the dock plugin, as [new_dock_plugin()] makes it.
#' @param navbar The items of the bar: an item, a `navbar_items` collection or
#'   a list of items
#'
#' @examples
#' issues <- "https://github.com/BristolMyersSquibb/blockr/issues/new"
#'
#' help <- navbar_menu(
#'   "help",
#'   "Help",
#'   navbar_link("issues", "Report an issue", issues)
#' )
#'
#' items <- custom_navbar(help, drop = "name")
#' names(items)
#' names(items[c("help", "options")])
#'
#' if (interactive()) {
#'   blockr.core::serve(blockr.core::new_board(), navbar = items)
#' }
#'
#' @return The functions `navbar_link()`, `navbar_menu()`, `navbar_storage()`
#' and `navbar_item()` return a `navbar_item` object.
#' The functions `default_navbar()` and `custom_navbar()` return a
#' `navbar_items` collection, as do `c()` and `[` on items.
#' The checks `is_navbar_item()` and `is_navbar_items()` return a boolean.
#' The dock's method of `frontend_args()` returns a list that holds the items
#' in `navbar`, each as a list of its fields.
#'
#' @rdname navbar
#' @export
navbar_link <- function(id, label, href) {
  new_navbar_item("link", id, label = label, href = href)
}

#' @rdname navbar
#' @export
navbar_menu <- function(id, label, ...) {
  new_navbar_item("menu", id, label = label, items = c(new_navbar_items(), ...))
}

#' @rdname navbar
#' @export
navbar_storage <- function(id) {
  new_navbar_item("storage", id)
}

#' @rdname navbar
#' @export
navbar_item <- function(id, module) {
  new_navbar_item("module", id, module = module)
}

# One of the dock's own parts of the bar, by its id, as `default_navbar()`
# gives it.
navbar_part <- function(id) {
  new_navbar_item("part", id)
}

new_navbar_item <- function(kind, id, ...) {
  validate_navbar_item(
    structure(list(kind = kind, id = id, ...), class = "navbar_item")
  )
}

# The kinds of navbar items that the dock draws, each with the fields that an
# item of the kind holds besides its kind and its id.
navbar_kinds <- list(
  part = character(),
  link = c("label", "href"),
  menu = c("label", "items"),
  storage = character(),
  module = "module"
)

# The dock's own parts of the bar, by id, in the order of the default bar.
navbar_parts <- c(
  "logo", "name", "storage", "undo_redo", "spacer", "views", "read_only",
  "options"
)

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

  id <- x[["id"]]

  if (!is_nonempty_string(id)) {
    blockr_abort(
      "Expecting a navbar item id to be a non-empty string.",
      class = "navbar_item_id_invalid"
    )
  }

  kind <- x[["kind"]]

  if (!is_string(kind) || !kind %in% names(navbar_kinds)) {
    blockr_abort(
      "The navbar item \"{id}\" has an unknown kind. ",
      "The kinds are {dQuote(names(navbar_kinds), FALSE)}.",
      class = "navbar_item_kind_invalid"
    )
  }

  fields <- c("kind", "id", navbar_kinds[[kind]])

  if (length(x) != length(fields) || !setequal(names(x), fields)) {
    fields <- paste0("`", fields, "`")
    blockr_abort(
      "The navbar item \"{id}\" is a {kind}, so it holds exactly the fields ",
      "{fields}.",
      class = "navbar_item_structure_invalid"
    )
  }

  switch(
    kind,
    part = validate_navbar_part(x),
    link = validate_navbar_link(x),
    menu = validate_navbar_menu(x),
    module = validate_navbar_module(x)
  )

  x
}

validate_navbar_part <- function(x) {

  if (!x[["id"]] %in% navbar_parts) {
    blockr_abort(
      "The dock has no part \"{x$id}\" to draw on the navbar. ",
      "Its parts are {dQuote(navbar_parts, FALSE)}.",
      class = "navbar_item_part_invalid"
    )
  }

  invisible(x)
}

validate_navbar_link <- function(x) {

  validate_navbar_label(x)

  if (!is_nonempty_string(x[["href"]])) {
    blockr_abort(
      "Expecting the `href` of the navbar link \"{x$id}\" to be a non-empty ",
      "string.",
      class = "navbar_item_href_invalid"
    )
  }

  invisible(x)
}

validate_navbar_menu <- function(x) {

  validate_navbar_label(x)

  if (!is_navbar_items(x[["items"]])) {
    blockr_abort(
      "Expecting the items of the navbar menu \"{x$id}\" to be a ",
      "`navbar_items` collection.",
      class = "navbar_item_menu_invalid"
    )
  }

  items <- validate_navbar_items(x[["items"]])
  kinds <- chr_xtr(items, "kind")
  other <- dQuote(names(items)[!kinds %in% c("link", "menu")], FALSE)

  if (length(other)) {
    blockr_abort(
      "The navbar menu \"{x$id}\" holds {other}, which ",
      "{?is neither a link nor a menu/are neither links nor menus}.",
      class = "navbar_item_menu_invalid"
    )
  }

  invisible(x)
}

validate_navbar_label <- function(x) {

  if (!is_nonempty_string(x[["label"]])) {
    blockr_abort(
      "Expecting the label of the navbar item \"{x$id}\" to be a non-empty ",
      "string.",
      class = "navbar_item_label_invalid"
    )
  }

  invisible(x)
}

validate_navbar_module <- function(x) {

  if (!is_nonempty_string(x[["module"]])) {
    blockr_abort(
      "Expecting the module of the navbar item \"{x$id}\" to be the id of a ",
      "module, a non-empty string.",
      class = "navbar_item_module_invalid"
    )
  }

  invisible(x)
}

is_nonempty_string <- function(x) {
  is_string(x) && !is.na(x) && nzchar(x)
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
  dups <- dQuote(unique(ids[duplicated(ids)]), FALSE)

  if (length(dups)) {
    blockr_abort(
      "More than one navbar item has {?the id/the ids} {dups}.",
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

  unknown <- dQuote(setdiff(ids, names(x)), FALSE)

  if (length(unknown)) {
    blockr_abort(
      "Unknown navbar item{?s} {unknown}.",
      class = "navbar_items_subset_invalid"
    )
  }

  match(ids, names(x))
}

#' @rdname navbar
#' @export
default_navbar <- function() {
  new_navbar_items(lapply(navbar_parts, navbar_part))
}

#' @rdname navbar
#' @export
custom_navbar <- function(items = list(), drop = character()) {

  items <- as_navbar_items(items)
  default <- default_navbar()

  if (!is.character(drop) || anyNA(drop)) {
    blockr_abort(
      "Expecting `drop` to give the ids of default navbar items as strings.",
      class = "navbar_items_subset_invalid"
    )
  }

  unknown <- dQuote(setdiff(drop, names(default)), FALSE)

  if (length(unknown)) {
    blockr_abort(
      "In `drop`, {unknown} {?names no default navbar item/name no default ",
      "navbar items}. The default items are {dQuote(names(default), FALSE)}.",
      class = "navbar_items_subset_invalid"
    )
  }

  c(default[setdiff(names(default), c(drop, names(items)))], items)
}

#' @rdname navbar
#' @export
frontend_args.dock_plugin <- function(x, ..., navbar = default_navbar()) {

  # R reads the other arguments unevaluated, as core's method does, so that
  # one that calls a function of a package that the app has not loaded fails
  # with this error too.
  others <- as.list(substitute(list(...)))[-1L]

  if (length(others)) {

    nms <- coal(names(others), character(length(others)))
    nms <- ifelse(nzchar(nms), paste0("`", nms, "`"), "an unnamed argument")

    blockr_abort(
      "The app's frontend, {plugin_class_id(x)}, takes no argument of ",
      "`serve()` but `navbar`, and was given {nms}.",
      class = "frontend_args_invalid"
    )
  }

  list(navbar = navbar_json(as_navbar_items(navbar)))
}

# The items as R writes them as JSON: each a bare list of its kind, its id and
# the fields of its kind, with the items of a menu written in turn.
navbar_json <- function(items) {
  lapply(
    unclass(items),
    function(item) {

      item <- unclass(item)

      if (identical(item[["kind"]], "menu")) {
        item[["items"]] <- navbar_json(item[["items"]])
      }

      item
    }
  )
}
