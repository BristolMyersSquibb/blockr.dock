# Dock views: structure and grid

A `dock_board` stores its views as two independent slots. **Structure**
– which panels belong to each view, plus the view names, ids and the
active view – is a `dock_views` collection of `dock_view` objects, read
with `board_views()`. **Grid** – the geometry of each view (nesting, tab
groups, sizes) – is a separate, `NULL`-valid `dock_grids` slot, read
with `board_grids()`. Single-page boards are a degenerate case: one
auto-named "Page" view. Blocks and extensions are shared across views
via the board's DAG; view membership is a layout concern only.

## Usage

``` r
is_dock_grids(x)

validate_dock_grids(x, views = NULL)

board_grids(x)

board_grids(x) <- value

as_dock_view(x, ...)

dock_view(members = character(), name = NULL)

is_dock_view(x)

validate_dock_view(x)

view_members(x)

is_dock_views(x)

validate_dock_views(x)

view_name(x)

view_name(x) <- value

view_names(x)

active_view(x)

active_view(x) <- value

board_views(x)

board_views(x) <- value
```

## Arguments

- x:

  An object appropriate to the function: a `dock_view` (for
  `view_name()`, `view_members()`), a `dock_views` collection (for
  `view_names()`, `active_view()`), a `dock_grids`, or a `dock_board`
  (for the board accessors).

- views:

  A `dock_views` collection, used to check that grids key known views.

- value:

  Replacement value

- ...:

  Forwarded to methods.

- members:

  Ordered character vector of panel ids.

- name:

  Optional display name for the view.

## Value

`board_views()` returns a `dock_views`, `board_grids()` a `dock_grids`
or `NULL`, and their setters the modified board invisibly. `dock_view()`
returns a `dock_view` and `view_members()` a character vector.
`is_dock_view()` / `is_dock_views()` / `is_dock_grids()` return a
boolean; `validate_dock_view()`, `validate_dock_views()` and
`validate_dock_grids()` return their (validated) input and throw on
error. `active_view()` returns the active view's id, or `NULL` when no
view is active, and `active_view<-()` the modified collection (or
`dock_board`) invisibly. `view_name()` returns a view's explicit display
name (or `NULL`), `view_name<-()` the modified view, and `view_names()`
a character vector of display labels keyed by view id (derived from the
id where a view has no explicit name). `as_dock_view()` returns a
`dock_view`: identity on a `dock_view`, or a view whose members are a
[dock_layout](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock-layout.md)'s
panel ids.

## Details

Each view carries a stable, immutable **id** (its key in the collection)
distinct from its editable display **name**. This mirrors the id / name
separation used for blocks (an immutable id keys the collection;
[`blockr.core::block_name()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_name.html)
is an editable label). The id is minted once when the view is created
and never changes – rename only rewrites the name attribute, never the
key. Use `dock_view()` to construct a view, `view_name()` /
`view_name<-()` to read and write its display name, `view_names()` for
all names in a collection, and `view_members()` for a `dock_view`'s
ordered panel-id set.

Multi-view boards are defined by passing `views` (and optionally
`grids`) to
[`new_dock_board()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock.md):
`views` is a named list keyed by view id (minted when absent), each
value a `dock_view()`, a bare character vector of member panel ids, or a
list of panel ids. `grids` is a named list keyed by view id whose values
are
[`dock_grid()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout.md)s;
it is optional – a view with no grid entry falls back to a default grid
over its members wherever placement geometry is needed. Bare block /
extension ids in either slot are resolved to canonical panel ids against
the board's blocks and extensions. The initially-active view is chosen
by `new_dock_board(active = )` (a view id), defaulting to the first; it
is a property of the collection, never of an individual view. View CRUD
is enabled unless the dock is locked (see `is_dock_locked()`).

Structure and grid are related by total semantics, not containment: a
member with no grid entry is an un-landed intent, a grid entry with no
membership an inert ghost. Both are legal on a committed board and
reconciled only where placement is read (`view_grid()` prunes ghosts and
shows un-landed members via a default) – the board is valid with no grid
at all. Referential integrity still holds: every member must reference a
block or extension on the board.

## Examples

``` r
brd <- new_dock_board(
  blocks = c(
    dataset_1 = blockr.core::new_dataset_block(),
    head_1 = blockr.core::new_head_block()
  ),
  views = list(
    analysis = dock_view(c("dataset_1", "head_1"), name = "Analysis"),
    overview = "dataset_1"
  ),
  active = "overview"
)
view_names(board_views(brd))
#>   analysis   overview 
#> "Analysis" "Overview" 
view_members(board_views(brd)[["analysis"]])
#> [1] "block_panel-dataset_1" "block_panel-head_1"   
```
