# Dock layout

The arrangement of panels in a dock can be specified using a
`dock_layout` object. A default layout is available via `default_grid()`
which results in two panel groups, the one on the left containing all
extension panels and the one on the right all block panels.
Complementing the low-level constructor `new_dock_layout()`, a
high-level entry point `create_dock_layout()` will create panels for
extensions and blocks, which can then be arranged via a nested list of
character vectors passed as `grid` argument.

## Usage

``` r
new_dock_layout(grid = NULL, panels = NULL, active_group = NULL)

default_grid(blocks, extensions)

create_dock_layout(
  blocks = list(),
  extensions = list(),
  grid = default_grid(blocks, extensions)
)

is_dock_layout(x)

validate_dock_layout(x, blocks = character())

as_dock_layout(x, ...)
```

## Arguments

- grid, panels, active_group:

  Layout components

- blocks, extensions:

  Dock board components

- x:

  Object

- ...:

  Generic consistency

## Value

The constructor `new_dock_layout()`, as does the high-level utility
`create_dock_layout()`, as well as the coercion function
`as_dock_layout()`, all return a `dock_layout` object. A helper function
for specifying a default grid is available as `default_grid()`, which
returns a list of character vectors. The validator
`validate_dock_layout()` returns its input and throws errors as
side-effect and inheritance can be checked using `is_dock_layout` which
returns a boolean.

## Examples

``` r
blks <- c(
  a = blockr.core::new_dataset_block(),
  b = blockr.core::new_head_block()
)

exts <- list(
  edit = new_edit_board_extension()
)

grid <- list("edit", list("a", "b"))

layout <- create_dock_layout(blks, exts, grid)
is_dock_layout(layout)
#> [1] TRUE
```
