#' Extension metadata
#'
#' The `description` argument of [new_dock_extension()] accepts either a bare
#' string or a structured `ext_meta` object built with `new_ext_meta()`. The
#' structured form carries model-facing documentation for a client driving the
#' extension through `modify_extension`: a free-text `description`, per-variable
#' `arguments` documentation keyed by externally controllable variable (see
#' [external_ctrl_vars()]), worked `examples` and free-text `guidance` on how to
#' drive it. `is_ext_meta()` checks the class and `ext_meta()` reads the whole
#' metadata off an extension, coercing a bare-string description to an
#' `ext_meta` whose only populated slot is `description`; the individual
#' components are read with `ext_desc()`, `ext_args()`, `ext_examples()` and
#' `ext_guidance()`. `extension_description()` remains as a deprecated alias of
#' `ext_desc()`.
#'
#' Per-variable `arguments` reuse blockr.core's block-argument specification:
#' pass a named character vector (variable to description) for the common case,
#' or a [new_arg_specs()] object to attach a machine-readable `type` (via the
#' `arg_*()` constructors) and a worked `example` to each variable.
#'
#' @param description Free-text summary of what the extension is
#' @param arguments Per-variable documentation for the externally controllable
#'   variables, either a named character vector (variable to description) or a
#'   [new_arg_specs()] object; keyed by controllable variable
#' @param examples List of worked configurations, each a named list keyed by
#'   controllable variable (a `modify_extension`-shaped payload)
#' @param guidance Free-text steering on how to drive the extension, distinct
#'   from the human-facing `description`
#' @param x An `ext_meta` object for `is_ext_meta()`, a `dock_extension` for
#'   `ext_meta()` and the per-component accessors
#'
#' @examples
#' new_ext_meta(
#'   "Workflow diagram of the board's blocks and links.",
#'   arguments = c(positions = "JSON object mapping block id to {x, y} coords.")
#' )
#'
#' ext_meta(new_edit_board_extension())
#'
#' @return `new_ext_meta()` returns an `ext_meta` object (a list with entries
#' `description`, `arguments`, `examples` and `guidance`), `is_ext_meta()` a
#' boolean and `ext_meta()` the normalized `ext_meta` an extension carries. The
#' per-component accessors return that component: `ext_desc()` a string or
#' `NULL`, `ext_args()` an `arg_specs`, `ext_examples()` a list and
#' `ext_guidance()` a string or `NULL`.
#'
#' @name ext-meta
#' @export
new_ext_meta <- function(description = NULL, arguments = NULL,
                         examples = list(), guidance = NULL) {

  if (not_null(description) && !is_string(description)) {
    blockr_abort(
      "An `ext_meta` description must be a string or `NULL`.",
      class = "ext_meta_description_invalid"
    )
  }

  if (not_null(guidance) && !is_string(guidance)) {
    blockr_abort(
      "An `ext_meta` guidance must be a string or `NULL`.",
      class = "ext_meta_guidance_invalid"
    )
  }

  if (!is.list(examples)) {
    blockr_abort(
      "An `ext_meta` examples must be a list.",
      class = "ext_meta_examples_invalid"
    )
  }

  structure(
    list(
      description = description,
      arguments = as_ext_arguments(arguments),
      examples = examples,
      guidance = guidance
    ),
    class = "ext_meta"
  )
}

#' @rdname ext-meta
#' @export
is_ext_meta <- function(x) {
  inherits(x, "ext_meta")
}

#' @rdname ext-meta
#' @export
ext_meta <- function(x) {

  stopifnot(is_dock_extension(x))

  meta <- attr(x, "description")

  if (is_ext_meta(meta)) {
    meta
  } else {
    new_ext_meta(description = meta)
  }
}

#' @rdname ext-meta
#' @export
ext_desc <- function(x) {
  ext_meta(x)[["description"]]
}

#' @rdname ext-meta
#' @export
ext_args <- function(x) {
  ext_meta(x)[["arguments"]]
}

#' @rdname ext-meta
#' @export
ext_examples <- function(x) {
  ext_meta(x)[["examples"]]
}

#' @rdname ext-meta
#' @export
ext_guidance <- function(x) {
  ext_meta(x)[["guidance"]]
}

#' @rdname ext-meta
#' @export
extension_description <- function(x) {

  blockr_warn(
    "`extension_description()` is deprecated; use `ext_desc()` instead.",
    class = "deprecated_extension_description",
    frequency = "once",
    frequency_id = "blockr_deprecated_extension_description"
  )

  ext_desc(x)
}

as_ext_arguments <- function(x) {

  if (is.null(x)) {
    return(new_arg_specs())
  }

  if (is_arg_specs(x)) {
    return(x)
  }

  if (!is.character(x) || is.null(names(x)) || any(!nzchar(names(x)))) {
    blockr_abort(
      "`ext_meta` arguments must be a named character vector or an ",
      "`arg_specs` object.",
      class = "ext_meta_arguments_invalid"
    )
  }

  as_arg_specs(x)
}

validate_ext_meta <- function(x, ctrl) {

  extra <- setdiff(names(x[["arguments"]]), ctrl)

  if (length(extra)) {
    blockr_abort(
      "`ext_meta` documents non-controllable variable{?s} {extra}.",
      class = "ext_meta_arguments_not_ctrl"
    )
  }

  for (ex in x[["examples"]]) {

    if (!is.list(ex) || is.null(names(ex)) || any(!nzchar(names(ex)))) {
      blockr_abort(
        "Each `ext_meta` example must be a named list keyed by controllable ",
        "variable.",
        class = "ext_meta_example_invalid"
      )
    }

    unknown <- setdiff(names(ex), ctrl)

    if (length(unknown)) {
      blockr_abort(
        "`ext_meta` example references non-controllable variable{?s} ",
        "{unknown}.",
        class = "ext_meta_example_not_ctrl"
      )
    }
  }

  invisible(x)
}
