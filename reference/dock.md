# Dock board

Using the docking layout manager provided by dockViewR, a `dock_board`
extends
[`blockr.core::new_board()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_board.html).
In addition to the attributes contained in a core board, this also
includes dock extensions (as `extensions`) and the panel arrangement (as
`layout`).

## Usage

``` r
new_dock_board(
  blocks = list(),
  links = list(),
  stacks = list(),
  ...,
  extensions = new_dock_extensions(),
  layout = default_grid(blocks, extensions),
  options = dock_board_options(),
  ctor = NULL,
  pkg = NULL,
  class = character()
)

is_dock_board(x)

as_dock_board(x, ...)

dock_layout(x)

dock_layout(x) <- value

dock_extensions(x)

dock_extensions(x) <- value

dock_ext_ids(x)

dock_board_options()
```

## Arguments

- blocks:

  Set of blocks

- links:

  Set of links

- stacks:

  Set of stacks

- ...:

  Further (metadata) attributes

- extensions:

  Dock extensions

- layout:

  Dock layout

- options:

  Board-level user settings

- ctor, pkg:

  Constructor information (used for serialization)

- class:

  Board sub-class

- x:

  Board object

- value:

  Replacement value

## Value

The constructor `new_dock_board()` returns a `board` object, as does the
coercion function `as_dock_board()`. Inheritance can be checked using
`is_dock_board()`, which returns a boolean. Getters `dock_layout()` and
`dock_extensions()` return `dock_layout` and `dock_extension` objects
while setters `dock_layout<-()` and `dock_extensions<-()` return the
updated board object (invisibly). A character vector of IDs is returned
by `dock_ext_ids()` and `dock_board_options()` returns a `board_options`
object.

## Examples

``` r
brd <- new_dock_board(c(a = blockr.core::new_dataset_block()))
str(dock_layout(brd), max.level = 2)
#> List of 3
#>  $ grid       :List of 2
#>   ..$ root       :List of 2
#>   ..$ orientation: chr "HORIZONTAL"
#>  $ panels     :List of 1
#>   ..$ block_panel-a:List of 5
#>  $ activeGroup: chr "1"
#>  - attr(*, "class")= chr "dock_layout"
```
