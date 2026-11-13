#' Navbar items
#'
#' An app script gives [blockr.core::serve()] the items of the dock's navbar,
#' the bar across the top of the dock, in `navbar`.
#' The bar shows them in their order.
#' R hands them to the dock's method of [blockr.core::frontend_args()].
#' Where the script passes no `navbar`, the method takes `default_navbar()`.
#' The method checks every item as the app starts.
#' If the dock cannot draw an item, the method stops the app with an error
#' that names the item.
#' The method returns the items in `navbar`, and core sends them to the
#' client in its reply to `hello`.
#' So no board and no file holds the bar.
#' Every board that the app opens shows the same bar.
#'
#' The function `default_navbar()` gives the default bar.
#' It holds the dock's own parts, each under its id, from left to right: the
#' logo (`logo`), the board's name (`name`), the controls of the app's
#' storage plugin (`storage`), Undo and Redo (`undo_redo`), the spacer
#' (`spacer`), the views menu (`views`), the read-only pill (`read_only`) and
#' the options button (`options`).
#' The spacer takes the free space, so the items after it sit at the right
#' end of the bar.
#' No function makes a part, so a script takes one from `default_navbar()`.
#' Besides the dock's own parts, an item is one of these:
#'
#' * A link, from `navbar_link()`.
#'   It opens in a new tab.
#' * A menu of links and further menus, from `navbar_menu()`.
#' * One of the storage plugin's controls, from `navbar_storage()`, by the
#'   control's id.
#'   The dock draws that control in the item's place, and the `storage` part
#'   draws the other controls.
#' * A module that a package offers, from `navbar_item()`, by the module's
#'   id, `{pkg}-{name}`.
#'   The module draws the item.
#'   R checks that the id is a non-empty string.
#'   It does not check that a package offers the module.
#'   The client loads the module as it draws the bar.
#'
#' Items combine with `c()`.
#' The operator `[` reorders or drops them, by id or by position.
#' The operator `[[` extracts one, and `names()` gives their ids.
#' Ids are unique among the items of the bar, and among the items of each
#' menu.
#'
#' The function `custom_navbar()` starts from the default bar.
#' It leaves out each item whose id is in `drop`.
#' Then it appends `items`.
#' An item of `items` with the id of a default item replaces that default
#' item, at the end of the bar.
#'
#' Each item is a list of its `kind`, its `id` and the fields of its kind.
#' R writes it as a JSON object for the client.
#' The kinds are `part`, `storage`, `link` with `label` and `href`, `menu`
#' with `label` and `items`, and `module` with `module`.
#'
#' @param id The item's id, a non-empty string.
#'   For `navbar_storage()`, the id of the storage plugin's control.
#' @param label The text of the link or the menu, a non-empty string
#' @param href The URL that the link opens, a non-empty string
#' @param ... For `navbar_menu()`, its links and menus, which combine as with
#'   `c()`.
#'   For `frontend_args()`, the other arguments of [blockr.core::serve()].
#'   The dock refuses them.
#' @param module The id of a module that a package offers, as
#'   `"blockr.topline-deck_button"`
#' @param items The items to append: an item, a `navbar_items` collection or
#'   a list of items
#' @param drop The ids of default items to leave out
#' @param x An object.
#'   For `frontend_args()`, the dock plugin, from [new_dock_plugin()].
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
#' The dock's method of `frontend_args()` returns a list with the items in
#' `navbar`, each as a list of its fields.
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

# The kinds of navbar items that the dock draws. Each kind gives the fields
# of its items besides `kind` and `id`.
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

  # R reads the other arguments without evaluating them, as core's method
  # does. So an argument that calls a function of a package that the app has
  # not loaded fails with this error too.
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

# The items as R writes them as JSON. Each item is a bare list of its kind,
# its id and the fields of its kind, and so is each item of a menu.
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
