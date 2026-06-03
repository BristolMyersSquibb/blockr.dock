# Dock layout

A `dock_layout` is the panel arrangement for a single view: a tree of
block / extension IDs, with at most one leaf marked as initially active.
A board holds a `dock_layouts` collection (one layout per view); panel
content is derived on demand from the board's blocks and extensions, so
only the arrangement is stored in a `dock_layout`. See
[`is_dock_layouts()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
for the collection-level helpers.

## Usage

``` r
dock_layout(
  ...,
  orientation = c("horizontal", "vertical"),
  active = FALSE,
  sizes = NULL,
  name = NULL
)

panels(..., active = NULL)

group(..., sizes = NULL)

is_dock_layout(x)

as_dock_layout(x, ...)

# S3 method for class 'dock_layout'
format(x, ..., bare = TRUE)

# S3 method for class 'dock_layout'
print(x, ...)

default_layout(blocks, extensions)

validate_dock_layout(x, blocks = character())
```

## Arguments

- ...:

  For `dock_layout()` and `group()`, layout children (bare IDs,
  character vectors, lists, `panels()`, or `group()`). For `panels()`,
  panel IDs. Otherwise reserved for generic consistency.

- orientation:

  Top-level split direction; one of `"horizontal"` (default) or
  `"vertical"`.

- active:

  For `dock_layout()`, logical: mark this layout as the initially-active
  view. For `panels()`, the ID of the tab to open by default.

- sizes:

  Numeric vector parallel to `...`, giving each child's share of the
  parent (positive; need not sum to 1).

- name:

  For `dock_layout()`, an optional display label for the view
  (free-form). When omitted, a label is derived from the view's id. The
  view's id is the list name in `new_dock_board(layouts = list(...))`,
  minted when absent and unique across the views of a `dock_layouts`.

- x:

  Object

- bare:

  For [`format()`](https://rdrr.io/r/base/format.html) /
  [`print()`](https://rdrr.io/r/base/print.html), drop the
  `block_panel-` / `ext_panel-` prefixes from panel IDs (see
  [`panel_obj_ids()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout-json.md)).

- blocks, extensions:

  Dock board components. For `default_layout()` the components to
  arrange; for `as_dock_layout()`, optional, used to resolve bare IDs
  and validate the result.

## Value

`dock_layout()` and `default_layout()` return a `dock_layout` object.
`panels()` returns a `dock_panels` node and `group()` returns a
`dock_group` node — both are layout sub-trees usable inside
`dock_layout()` / `group()`. `as_dock_layout()` returns a `dock_layout`
(from a board or a spec list);
[`as.list()`](https://rdrr.io/r/base/list.html) of a `dock_layout`
returns the spec list. `is_dock_layout()` returns a boolean.
`validate_dock_layout()` returns its input and throws on error.

## Details

Construct a layout with:

- `dock_layout(...)`: the page-level container. Its `...` are the
  children of the root branch. Bare strings become single-panel leaves,
  character vectors become tabbed leaves, lists become nested branches.
  Use `panels()` for a tabbed leaf with an explicit open tab, and
  `group()` for a branch with explicit sizes.

- `panels(..., active = NULL)`: a tabbed leaf whose tab strip holds the
  given panel IDs. `active` selects the initially-open tab; the first ID
  wins by default. A single-panel `panels()` is permitted but redundant
  (a bare string is equivalent).

- `group(..., sizes = NULL)`: a branch container. `sizes` is a numeric
  vector parallel to `...` that overrides the even split.

- `default_layout(blocks, extensions)` produces the default two-row
  arrangement (extensions on top, blocks below) for a board.

`dock_layout()` accepts `orientation = "horizontal" | "vertical"` for
the top-level split direction, `sizes` for the root-branch ratios,
`active = TRUE` to mark this layout as the initially-active view in a
`dock_layouts` collection, and `name` for the view's display label. In
`new_dock_board(layouts = list(...))` the list name is the view's stable
*id* (the container's key, like a block id), minted when absent; `name`
sets the free-form display label on the view itself. When no name is
given, one is derived from the id for display.

A *view* is the conceptual page-level container; a *layout* is the panel
arrangement inside a view. The dockview-shape `grid + panels` payload
that dockViewR consumes is an internal projection of a `dock_layout`
against the board's blocks and extensions; it is not a public type.

`as_dock_layout()` coerces to a `dock_layout`: a `dock_layout`
(identity), a `board` (its active layout), or a spec list
([`as.list()`](https://rdrr.io/r/base/list.html) of a layout, or a
parsed
[`layout_to_json()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout-json.md)
string). Pass `blocks` / `extensions` to resolve bare IDs to canonical
panel IDs and validate. [`as.list()`](https://rdrr.io/r/base/list.html)
of a `dock_layout` returns that spec list. The JSON-string boundary is
[`layout_to_json()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout-json.md)
/
[`layout_from_json()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout-json.md).

## Examples

``` r
blks <- c(
  a = blockr.core::new_dataset_block(),
  b = blockr.core::new_head_block()
)

exts <- list(
  edit = new_edit_board_extension()
)

# The default arrangement for a given set of blocks and extensions
default_layout(blks, exts)
#> <dock_layout> horizontal
#> ├─ edit_board_extension
#> └─ tabs
#>    ├─ a (active)
#>    └─ b

# Mark a layout as the initially-active view in a `dock_layouts`
# collection
dock_layout("a", "b", active = TRUE)
#> <dock_layout> horizontal
#> ├─ a
#> └─ b

# Tabbed leaf with an explicit open tab
panels("a", "b", "edit_board_extension", active = "edit_board_extension")
#> $views
#> $views[[1]]
#> [1] "a"
#> 
#> $views[[2]]
#> [1] "b"
#> 
#> $views[[3]]
#> [1] "edit_board_extension"
#> 
#> 
#> $active
#> [1] "edit_board_extension"
#> 
#> attr(,"class")
#> [1] "dock_panels" "dock_node"  

# Branch with explicit child ratios
group("a", "b", sizes = c(0.3, 0.7))
#> $children
#> $children[[1]]
#> [1] "a"
#> 
#> $children[[2]]
#> [1] "b"
#> 
#> 
#> $sizes
#> [1] 0.3 0.7
#> 
#> attr(,"class")
#> [1] "dock_group" "dock_node" 

# Composing them inside a layout
dock_layout(
  "a",
  panels("b", "edit_board_extension", active = "edit_board_extension"),
  sizes = c(0.3, 0.7)
)
#> <dock_layout> horizontal
#> ├─ a (30%)
#> └─ tabs (70%)
#>    ├─ b
#>    └─ edit_board_extension (active)

# Vertical top-level split
dock_layout("a", "b", orientation = "vertical")
#> <dock_layout> vertical
#> ├─ a
#> └─ b
```
