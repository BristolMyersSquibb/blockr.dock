# Dock grid authoring

A
[dock_grid](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock-grid.md)
is a view's geometry: the arrangement of its panels into nested splits
and tab groups, with sizes. `dock_grid()` is the authoring DSL that
builds one; view *membership* (which panels belong) and the display
*name* live on the view (see
[dock_view()](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)),
not the grid.

## Usage

``` r
dock_grid(..., orientation = c("horizontal", "vertical"), sizes = NULL)

panels(..., active = NULL)

group(..., sizes = NULL)

default_layout(blocks, extensions)
```

## Arguments

- ...:

  For `dock_grid()` and `group()`, grid children
  ([`blk()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/panel-ref.md)
  /
  [`ext()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/panel-ref.md)
  references, bare ids, character vectors, lists, `panels()`, or
  `group()`). For `panels()`, panel references or ids. A reference used
  here cannot carry a placement hint. Otherwise reserved for generic
  consistency.

- orientation:

  Top-level split direction; one of `"horizontal"` (default) or
  `"vertical"`.

- sizes:

  Numeric vector parallel to `...`, giving each child's share of the
  parent (positive; need not sum to 1).

- active:

  For `panels()`, the id of the tab to open by default.

- blocks, extensions:

  Dock board components to arrange (for `default_layout()`).

## Value

`dock_grid()` returns a
[dock_grid](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock-grid.md)
object. `panels()` returns a `dock_panels` node and `group()` returns a
`dock_group` node – both are grid sub-trees usable inside `dock_grid()`
/ `group()`. `default_layout()` returns a `list` with `views` (a
`dock_views`) and `grids` (a `dock_grids`).

## Details

Construct a grid with:

- `dock_grid(...)`: the page-level container. Its `...` are the children
  of the root branch. Bare strings become single-panel leaves, character
  vectors become tabbed leaves, lists become nested branches. Use
  `panels()` for a tabbed leaf with an explicit open tab, and `group()`
  for a branch with explicit sizes.

- `panels(..., active = NULL)`: a tabbed leaf whose tab strip holds the
  given panel ids. `active` selects the initially-open tab; the first id
  wins by default. A single-panel `panels()` is permitted but redundant
  (a bare string is equivalent).

- `group(..., sizes = NULL)`: a branch container. `sizes` is a numeric
  vector parallel to `...` that overrides the even split.

- `default_layout(blocks, extensions)` produces the default board
  arrangement (extensions on top, blocks below) as a
  `list(views, grids)` the constructor consumes.

`dock_grid()` accepts `orientation = "horizontal" | "vertical"` for the
top-level split direction and `sizes` for the root-branch ratios. The
dockView-native `{grid, panels, activeGroup}` payload dockViewR consumes
is a
[dock_layout](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock-layout.md),
built from a grid against the board's blocks and extensions.

## Examples

``` r
blks <- c(
  a = blockr.core::new_dataset_block(),
  b = blockr.core::new_head_block()
)

# Panels named with typed references; bare id strings work too
panels(blk("a"), blk("b"), active = blk("b"))
#> $views
#> $views[[1]]
#> [1] "block_panel-a"
#> 
#> $views[[2]]
#> [1] "block_panel-b"
#> 
#> 
#> $active
#> [1] "block_panel-b"
#> 
#> attr(,"class")
#> [1] "dock_panels" "dock_node"  

# Branch with explicit child ratios
group(blk("a"), blk("b"), sizes = c(0.3, 0.7))
#> $children
#> $children[[1]]
#> <panel_ref> block_panel-a 
#> 
#> $children[[2]]
#> <panel_ref> block_panel-b 
#> 
#> 
#> $sizes
#> [1] 0.3 0.7
#> 
#> attr(,"class")
#> [1] "dock_group" "dock_node" 

# An extension panel beside a tabbed block leaf
dock_grid(
  ext("dag"),
  panels(blk("a"), blk("b"), active = blk("b")),
  sizes = c(0.3, 0.7)
)
#> <dock_grid> horizontal
#> ├─ dag (30%)
#> └─ tabs (70%)
#>    ├─ a
#>    └─ b (active)

# Vertical top-level split
dock_grid(blk("a"), blk("b"), orientation = "vertical")
#> <dock_grid> vertical
#> ├─ a (50%)
#> └─ b (50%)
```
