# Dock board

Using the docking layout manager provided by dockViewR, a `dock_board`
extends
[`blockr.core::new_board()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_board.html).
In addition to the attributes contained in a core board, this also
includes dock extensions (as `extensions`) and the per-view layout,
stored as two independent slots – view structure
([`board_views()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md))
and grid geometry
([`board_grids()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)).
Single-page boards are a degenerate case with one auto-named "Page"
view.

## Usage

``` r
new_dock_board(
  blocks = list(),
  links = list(),
  stacks = list(),
  ...,
  extensions = new_dock_extensions(),
  views = NULL,
  grids = NULL,
  active = NULL,
  options = dock_board_options(),
  ctor = NULL,
  pkg = NULL,
  class = character()
)

is_dock_board(x)

as_dock_board(x, ...)

dock_extensions(x)

dock_extensions(x) <- value

dock_ext_ids(x)

extension_ids(x, class = NULL)

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

  Dock extensions. Each is keyed by its list name, or by its class
  stripped of the `_extension` suffix when unnamed (so
  `new_dag_extension()` becomes `dag`); that key names the extension's
  panel and is what
  [`ext()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/panel-ref.md)
  resolves against.

- views:

  Per-view membership: a named list keyed by view id (minted when
  absent), each value a
  [`dock_view()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md),
  or member panels named with
  [`blk()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/panel-ref.md)
  /
  [`ext()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/panel-ref.md)
  references or bare ids (as a vector or list). `NULL` yields a single
  default view over the board's blocks and extensions.

- grids:

  Per-view geometry: a named list keyed by view id whose values are
  [`dock_grid()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout.md)s,
  or a `dock_grids`. Optional – a view with no grid entry falls back to
  a default grid over its members.

- active:

  Id of the initially active view (a key of `views`). Defaults to the
  first view. Which view is active is a property of the collection, not
  of an individual view.

- options:

  Board-level user settings

- ctor, pkg:

  Constructor information (used for serialization)

- class:

  Optional extension subclass; when supplied, only ids of extensions
  inheriting from it are returned.

- x:

  Board object

- value:

  Replacement value

## Value

The constructor `new_dock_board()` returns a `board` object, as does the
coercion function `as_dock_board()`. Inheritance can be checked using
`is_dock_board()`, which returns a boolean. The `dock_extensions()` and
`dock_extensions<-()` accessors return / set the board's
`dock_extension` objects. A character vector of IDs is returned by
`dock_ext_ids()` and `dock_board_options()` returns a `board_options`
object.

## Details

Multi-view boards pass `views` (and optionally `grids`); see
[dock_view()](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
and [dock_grid()](https://rdrr.io/r/graphics/layout.html) for the input
forms and
[board_views()](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
for how they combine. Bare block / extension ids are resolved against
the board's blocks and extensions. With `views = NULL` the board gets a
single default view.

## Examples

``` r
brd <- new_dock_board(c(a = blockr.core::new_dataset_block()))
view_members(board_views(brd)[[1L]])
#> [1] "block_panel-a"
```
