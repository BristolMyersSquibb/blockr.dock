#' Navbar items
#'
#' The navbar of the dock is drawn from a `navbar_items` collection, the
#' dock's own parts included.
#' An item is created by `navbar_item()`, items are combined with `c()`, `[`
#' reorders or drops them by id and `[[` extracts one.
#'
#' The default items, returned by `default_navbar_items()`, are, in order: the
#' blockr logo, which is also the busy indicator (id `"logo"`), a spacer
#' (`"spacer"`), the view menu (`"views"`), the read-only indicator
#' (`"read_only"`) and the board options (`"options"`).
#' The function `custom_navbar()` appends a fixed set of items to them,
#' dropping any default item that shares an id with one of them.
#'
#' @param id Item id, unique within the navbar
#' @param fill Whether the item takes up the free space of the bar
#' @param shrink Whether the item gives way first on a narrow bar. The items
#' with neither `fill` nor `shrink` keep their width.
#'
#' @examples
#' help <- navbar_item("help")
#'
#' items <- c(default_navbar_items(), help)
#' names(items)
#' names(items[c("help", "options")])
#'
#' names(custom_navbar(help))
#'
#' @return The constructor `navbar_item()` returns a `navbar_item` object.
#' The functions `default_navbar_items()` and `custom_navbar()` return a
#' `navbar_items` collection, as do `c()` and `[` on items.
#' The checks `is_navbar_item()` and `is_navbar_items()` return a boolean.
#'
#' @rdname navbar
#' @export
navbar_item <- function(id, fill = FALSE, shrink = FALSE) {
  validate_navbar_item(
    structure(
      list(id = id, fill = fill, shrink = shrink),
      class = "navbar_item"
    )
  )
}

#' @param x Object. For `custom_navbar()`, the items to append: a
#' `navbar_item`, a `navbar_items` collection or a list of items.
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

#' @rdname navbar
#' @export
default_navbar_items <- function() {
  c(
    new_navbar_items(),
    navbar_item("logo"),
    navbar_item("spacer", fill = TRUE),
    navbar_item("views"),
    navbar_item("read_only"),
    navbar_item("options")
  )
}

#' @rdname navbar
#' @export
custom_navbar <- function(x) {

  custom <- as_navbar_items(x)
  default <- default_navbar_items()

  c(default[setdiff(names(default), names(custom))], custom)
}
