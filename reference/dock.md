# Dock board

Using the docking layout manager provided by dockViewR, a `dock_board`
extends
[`blockr.core::new_board()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_board.html).
In addition to the attributes contained in a core board, this also
includes dock extensions (as `extensions`) and the panel arrangement (as
`layouts`). The `layouts` field is always stored internally as a
`dock_layouts` collection (multi-view); single-page boards are a
degenerate case with one auto-named "Page" view.

## Usage

``` r
new_dock_board(
  blocks = list(),
  links = list(),
  stacks = list(),
  ...,
  extensions = new_dock_extensions(),
  layouts = default_layout(blocks, extensions),
  options = dock_board_options(),
  ctor = NULL,
  pkg = NULL,
  class = character()
)

is_dock_board(x)

as_dock_board(x, ...)

active_layout(x)

active_layout(x) <- value

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

- layouts:

  A named list of per-view arrangements (multi-view), a `dock_layout` /
  raw list (single-page), or an existing `dock_layouts` collection. All
  forms are normalised to `dock_layouts`.

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
`is_dock_board()`, which returns a boolean.
[`board_layouts()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
returns the board's `dock_layouts`; `active_layout()` returns the active
view's `dock_layout` and `active_layout<-()` writes into the active
view. The `dock_extensions()` and `dock_extensions<-()` accessors return
/ set the board's `dock_extension` objects. A character vector of IDs is
returned by `dock_ext_ids()` and `dock_board_options()` returns a
`board_options` object.

## Details

For multi-view boards, pass a named list to `layouts =` — each name
becomes a view, each value is the panel arrangement (a
[`dock_layout()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout.md)
or a raw list of block / extension IDs). For a single-page board, pass a
`dock_layout` or raw list directly. Either way the input is normalised
to a `dock_layouts`, with leaf IDs resolved against the board's blocks
and extensions.

## Examples

``` r
brd <- new_dock_board(c(a = blockr.core::new_dataset_block()))
str(active_layout(brd), max.level = 2)
#> List of 2
#>  $ grid       :List of 2
#>   ..$ root       :List of 3
#>   ..$ orientation: chr "HORIZONTAL"
#>  $ activeGroup: chr "1"
#>  - attr(*, "class")= chr "dock_layout"
#>  - attr(*, "active")= logi TRUE
```
