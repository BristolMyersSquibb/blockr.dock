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
#' `as_panel_ref()` is the programmatic form of that sugar: given a bare `id`
#' and the `block_ids` / `ext_ids` partitioning a board's namespace, it returns
#' the correctly-typed `blk()` / `ext()` ref (carrying any placement hint),
#' applying the same block-first precedence and the same cross-namespace clash
#' error. An already wire-prefixed panel id, or an existing `panel_ref`,
#' resolves to the matching ref unchanged.
#'
#' @param id A block or extension id. For `blk()` / `ext()` a bare id (not the
#'   wire-prefixed panel id); `as_panel_ref()` additionally accepts an already
#'   wire-prefixed panel id or a `panel_ref`.
#' @param block_ids,ext_ids Character vectors partitioning the id namespace that
#'   `as_panel_ref()` resolves a bare `id` against.
#' @param near A ref or bare id to anchor placement against.
#' @param side Placement direction relative to `near`: one of `within`, `left`,
#'   `right`, `above`, `below`.
#' @param size Target size ratio in (0, 1) -- consumed by `resize`, and recorded
#'   on `add` for a later size-on-create pass.
#' @param x An object.
#' @param ... Ignored.
#'
#' @return `blk()`, `ext()` and `as_panel_ref()` return a `panel_ref`.
#'   `as.character()` on one returns its canonical panel id, and
#'   `is_panel_ref()` returns a boolean.
#'
#' @examples
#' blk("my_block", near = "other_block", side = "right")
#' ext("dag")
#' as.character(blk("my_block"))
#'
#' as_panel_ref("my_block", block_ids = "my_block")
#' as_panel_ref("dag", block_ids = "my_block", ext_ids = "dag")
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
as_panel_ref <- function(id, block_ids = character(), ext_ids = character(),
                         near = NULL, side = NULL, size = NULL) {

  if (is_panel_ref(id)) {
    return(id)
  }

  stopifnot(is_string(id))

  if (maybe_block_panel_id(id) || maybe_ext_panel_id(id)) {

    pid <- as_dock_panel_id(id)
    id <- as_obj_id(pid)
    use_ext <- is_ext_panel_id(pid)

  } else {

    in_blk <- id %in% block_ids
    in_ext <- id %in% ext_ids

    if (in_blk && in_ext) {
      blockr_abort(
        paste0(
          "Id `", id, "` names both a block and an extension -- disambiguate ",
          "with blk() or ext()."
        ),
        class = "dock_panel_ref_clash"
      )
    }

    use_ext <- in_ext && !in_blk
  }

  if (use_ext) {
    ext(id, near = near, side = side, size = size)
  } else {
    blk(id, near = near, side = side, size = size)
  }
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

# A ref used in the layout-authoring DSL (`panels()` / `dock_grid()` / a `views`
# entry) contributes its canonical panel id; a bare string passes through. A
# placement hint is meaningless where no `add` / `move` happens, so it errors.
as_panel_string <- function(x) {

  if (!is_panel_ref(x)) {
    return(x)
  }

  if (length(panel_ref_hint(x))) {
    blockr_abort(
      "A panel reference in the layout DSL cannot carry a placement hint.",
      class = "dock_layout_ref_hint"
    )
  }

  as.character(x)
}
