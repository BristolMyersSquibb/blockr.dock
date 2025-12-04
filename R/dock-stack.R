#' Colored stacks
#'
#' While stacks created via [blockr.core::new_stack()] do not keep track of
#' a color attribute, a `dock_stack` object does. Such objects can be created
#' via `new_dock_stack()`. The color attribute can be extracted using
#' `stack_color()` and set with `stack_color<-()`. A new color suggestion,
#' based on existing colors, is available through `suggest_new_colors()`.
#'
#' @param ... Passed to [blockr.core::new_stack()]
#' @param color String-valued color value (using hex encoding)
#'
#' @return The constructor `new_dock_stack()` returns a "dock_stack" object,
#' which is a stack object as returned by [blockr.core::new_stack()], with an
#' additional color attribute. Inheritance can be checked using
#' `is_dock_stack()`, which returns a scalar logical and the color attribute
#' can be set and retrieved using `stack_color<-()` (returns the modified stack
#' object invisibly) and `stack_color()` (returns a string), respectively.
#' Stack objects may be coerced to "dock_stack" using `as_dock_stack()` and
#' finally, a utility function `suggest_new_colors()` which returns a character
#' vector of new colors, based on an existing palette.
#'
#' @rdname stack
#' @export
new_dock_stack <- function(..., color = suggest_new_colors()) {
  new_stack(..., color = color, ctor = "new_dock_stack", pkg = pkg_name(),
            class = "dock_stack")
}

#' @param x object
#' @rdname stack
#' @export
is_dock_stack <- function(x) {
  inherits(x, "dock_stack")
}

#' @export
validate_stack.dock_stack <- function(x) {

  col <- stack_color(NextMethod())

  if (!is_string(col) || !is_hex_color(col)) {
    blockr_abort(
      "Expecting stack color as string-valued hex color.",
      class = "invalid_stack_color"
    )
  }

  x
}

#' @rdname stack
#' @export
stack_color <- function(x) {
  UseMethod("stack_color")
}

#' @export
stack_color.stack <- function(x) {
  "#FFFFFF"
}

#' @export
stack_color.stacks <- function(x) {
  chr_ply(x, stack_color)
}

#' @export
stack_color.board <- function(x) {
  stack_color(board_stacks(x))
}

#' @param colors Currently used color values
#' @param n Number of new colors to generate
#' @rdname stack
#' @export
suggest_new_colors <- function(colors = character(), n = 1) {

  color_fun <- blockr_option("stack_color", next_color)

  stopifnot(is.function(color_fun), is.character(colors), is_count(n))

  res <- character()

  for (i in seq_len(n)) {
    res <- c(res, color_fun(c(colors, res)))
  }

  res
}

#' @importFrom blockr.dock stack_color
#' @export
stack_color.dock_stack <- function(x) {
  attr(x, "color")
}

#' @param value Replacement value
#' @rdname stack
#' @export
`stack_color<-` <- function(x, value) {
  stopifnot(is_dock_stack(x), is_string(value), is_hex_color(value))
  attr(x, "color") <- value
  x
}

#' @rdname stack
#' @export
as_dock_stack <- function(x, ...) {
  UseMethod("as_dock_stack")
}

#' @export
as_dock_stack.dock_stack <- function(x, ...) {
  x
}

#' @export
as_dock_stack.stack <- function(x, color = suggest_new_colors(), ...) {

  attr(x, "color") <- color

  class(x) <- c("dock_stack", class(x))

  validate_stack(x)
}

#' @export
as_dock_stack.list <- function(x, ...) {
  do.call(new_dock_stack, x)
}

#' @export
format.dock_stack <- function(x, ...) {
  res <- NextMethod()
  c(res[c(1L, 2L)], paste0("Color: \"", stack_color(x), "\""), res[-c(1L, 2L)])
}

#' @export
as.list.dock_stack <- function(x, ...) {
  list(
    blocks = as.character(x),
    name = stack_name(x),
    color = stack_color(x)
  )
}

#' @export
blockr_deser.dock_stack <- function(x, data, ...) {
  as_dock_stack(data[["payload"]])
}

as_dock_stacks <- function(x) {

  x <- as_stacks(x)

  todo <- lgl_ply(x, Negate(is_dock_stack))

  if (!any(todo)) {
    return(x)
  }

  new_col <- suggest_new_colors(
    chr_ply(x[!todo], stack_color),
    n = sum(todo)
  )

  x[todo] <- set_names(
    map(as_dock_stack, x[todo], new_col),
    names(x[todo])
  )

  x
}
