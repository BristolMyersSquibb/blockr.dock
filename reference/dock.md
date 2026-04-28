# Dock board

Using the docking layout manager provided by dockViewR, a `dock_board`
extends
[`blockr.core::new_board()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_board.html).
In addition to the attributes contained in a core board, this also
includes dock extensions (as `extensions`) and the panel arrangement (as
`layout`). The `layout` parameter accepts either a grid specification
(as before), a
[`dock_layouts()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
object for multi-view boards, or a plain named list of layout specs
which is auto-detected as multi-view.

## Usage

``` r
new_dock_board(
  blocks = list(),
  links = list(),
  stacks = list(),
  ...,
  extensions = new_dock_extensions(),
  layout = dock_layouts(Page = default_view_grid(blocks, extensions)),
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

  Either a grid specification (list), a `dock_layout`, a
  [`dock_layouts()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
  object, or a named [`list()`](https://rdrr.io/r/base/list.html) of
  layout specs

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
updated board object (invisibly). When `layout` is a `dock_layouts`
object,
[`board_views()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
returns it and `dock_layout()` returns the active view's layout. A
character vector of IDs is returned by `dock_ext_ids()` and
`dock_board_options()` returns a `board_options` object.

## Examples

``` r
brd <- new_dock_board(c(a = blockr.core::new_dataset_block()))
str(dock_layout(brd), max.level = 2)
#> List of 3
#>  $ grid       :List of 2
#>   ..$ root       :List of 3
#>   ..$ orientation: chr "HORIZONTAL"
#>  $ panels     :List of 1
#>   ..$ block_panel-a:List of 5
#>  $ activeGroup: chr "1"
#>  - attr(*, "class")= chr "dock_layout"
#>  - attr(*, "active")= logi TRUE
```
