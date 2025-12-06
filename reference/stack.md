# Colored stacks

While stacks created via
[`blockr.core::new_stack()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_stack.html)
do not keep track of a color attribute, a `dock_stack` object does. Such
objects can be created via `new_dock_stack()`. The color attribute can
be extracted using `stack_color()` and set with `stack_color<-()`. A new
color suggestion, based on existing colors, is available through
`suggest_new_colors()`.

## Usage

``` r
new_dock_stack(..., color = suggest_new_colors())

is_dock_stack(x)

stack_color(x)

suggest_new_colors(colors = character(), n = 1)

stack_color(x) <- value

as_dock_stack(x, ...)
```

## Arguments

- ...:

  Passed to
  [`blockr.core::new_stack()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_stack.html)

- color:

  String-valued color value (using hex encoding)

- x:

  object

- colors:

  Currently used color values

- n:

  Number of new colors to generate

- value:

  Replacement value

## Value

The constructor `new_dock_stack()` returns a "dock_stack" object, which
is a stack object as returned by
[`blockr.core::new_stack()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_stack.html),
with an additional color attribute. Inheritance can be checked using
`is_dock_stack()`, which returns a scalar logical and the color
attribute can be set and retrieved using `stack_color<-()` (returns the
modified stack object invisibly) and `stack_color()` (returns a string),
respectively. Stack objects may be coerced to "dock_stack" using
`as_dock_stack()` and finally, a utility function `suggest_new_colors()`
which returns a character vector of new colors, based on an existing
palette.
