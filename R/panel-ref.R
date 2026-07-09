#' Typed panel references
#'
#' `blk()` and `ext()` construct references to a block or extension panel for
#' the `views$mod` panel-op grammar (see
#' [board_update][blockr.core::board_update]). They are the public currency: a
#' caller names a block or extension by its
#' **id**, never the `block_panel-` / `ext_panel-` wire prefix -- the ref is the
#' codec, and `as.character()` on one yields the canonical panel-id encoding.
#'
#' Each ref optionally carries its own placement hint, so a verb's operands are
#' an unnamed list of self-describing refs:
#' `add = list(blk("a", near = "b", side = "right"), ext("dag"))`. Which hint
#' fields are meaningful depends on the verb -- `add` consumes `near` / `side` /
#' `size`, `move` consumes `near` / `side` -- and a hint on a ref used where no
#' placement happens (`rm`, `select`, a `near` anchor, or the `dock_grid()` /
#' `panels()` authoring DSL) is a loud error. Because the hints are constructor
#' arguments, a misspelled one (`blk("a", sise = 0.4)`) fails at the call site
#' with R's own unused-argument error, before any payload exists.
#'
#' Bare id strings are accepted as sugar wherever a ref is, resolved block-first
#' with a hard error only on a true cross-namespace clash (an id that is both a
#' block and an extension), which then demands a typed ref.
#'
#' @param id A block or extension id (not the wire-prefixed panel id).
#' @param near A ref or bare id to anchor placement against.
#' @param side Placement direction relative to `near`: one of `within`, `left`,
#'   `right`, `above`, `below`.
#' @param size Target size ratio in (0, 1) -- consumed by `resize`, and recorded
#'   on `add` for when the `set_size` floor lands (#320).
#' @param x An object.
#' @param ... Ignored.
#'
#' @return `blk()` / `ext()` return a `panel_ref`. `as.character()` on one
#'   returns its canonical panel id, and `is_panel_ref()` returns a boolean.
#'
#' @examples
#' blk("my_block", near = "other_block", side = "right")
#' ext("dag")
#' as.character(blk("my_block"))
#'
#' @rdname panel-ref
#' @export
blk <- function(id, near = NULL, side = NULL, size = NULL) {
  new_panel_ref(id, "block", near, side, size)
}

#' @rdname panel-ref
#' @export
ext <- function(id, near = NULL, side = NULL, size = NULL) {
  new_panel_ref(id, "ext", near, side, size)
}

new_panel_ref <- function(id, type, near = NULL, side = NULL, size = NULL) {

  stopifnot(is_string(id))

  structure(
    list(id = id, type = type, near = near, side = side, size = size),
    class = "panel_ref"
  )
}

#' @rdname panel-ref
#' @export
is_panel_ref <- function(x) {
  inherits(x, "panel_ref")
}

#' @rdname panel-ref
#' @export
as.character.panel_ref <- function(x, ...) {

  if (identical(x[["type"]], "block")) {
    as.character(as_block_panel_id(x[["id"]]))
  } else {
    as.character(as_ext_panel_id(x[["id"]]))
  }
}

#' @export
print.panel_ref <- function(x, ...) {
  cat("<panel_ref>", as.character(x), "\n", sep = " ")
  invisible(x)
}

# The placement hint a ref carries, NULLs dropped. `near` is still a ref / bare
# id here; augment resolves it to a canonical panel id alongside the ref itself.
panel_ref_hint <- function(x) {
  drop_nulls(list(near = x[["near"]], side = x[["side"]], size = x[["size"]]))
}
