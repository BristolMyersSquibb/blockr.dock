# Dock views: structure and grid

A `dock_board` stores its views as two independent slots. **Structure**
— which panels belong to each view, plus the view names, ids and the
active view — is a `dock_views` collection of `dock_view` objects, read
with `board_views()`. **Grid** — the geometry of each view (nesting, tab
groups, sizes) — is a separate, `NULL`-valid `dock_grids` slot, read
with `board_grids()`. `board_layouts()` composes the two into the
grid-bearing `dock_layouts` handle the update lifecycle still reads.
Single-page boards are a degenerate case: one auto-named "Page" view.
Blocks and extensions are shared across views via the board's DAG; view
membership is a layout concern only.

## Usage

``` r
new_dock_layouts(...)

validate_dock_layouts(x)

is_dock_layouts(x)

view_name(x)

view_name(x) <- value

view_names(x)

active_view(x)

active_view(x) <- value

board_layouts(x) <- value

board_layouts(x)

as_dock_layouts(x, ...)

new_dock_view(members = character(), name = NULL)

is_dock_view(x)

view_members(x)

is_dock_views(x)

validate_dock_views(x)

board_views(x)

board_views(x) <- value

is_dock_grids(x)

validate_dock_grids(x, views = NULL)

board_grids(x)

board_grids(x) <- value
```

## Arguments

- ...:

  Generic consistency

- x:

  An object appropriate to the function: a `dock_view` / `dock_layout`
  (for `view_name()`, `view_members()`), a `dock_views` / `dock_layouts`
  collection (for `view_names()`, `active_view()`), a `dock_grids`, or a
  `dock_board` (for the board accessors).

- value:

  Replacement value

- members:

  Ordered character vector of panel IDs.

- name:

  Optional display name for the view.

- views:

  A `dock_views` collection, used to check membership when validating
  grids.

## Value

`board_views()` returns a `dock_views`, `board_grids()` a `dock_grids`
or `NULL`, and their setters the modified board invisibly.
`view_members()` returns a character vector. `is_dock_view()` /
`is_dock_views()` / `is_dock_grids()` / `is_dock_layouts()` return a
boolean; `validate_dock_views()`, `validate_dock_grids()` and
`validate_dock_layouts()` return their (validated) input and throw on
error. `active_view()` returns the active view's id, or `NULL` when no
view is active, and `active_view<-()` the modified collection (or
`dock_board`) invisibly. `view_name()` returns a view's explicit display
name (or `NULL`), `view_name<-()` the modified object, and
`view_names()` a character vector of display labels keyed by view id
(derived from the id where a view has no explicit name).

## Details

Each view carries a stable, immutable **id** (its key in the collection)
distinct from its editable display **name**. This mirrors the id / name
separation used for blocks (an immutable id keys the collection;
[`blockr.core::block_name()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_name.html)
is an editable label). The id is minted once when the view is created
and never changes — rename only rewrites the name attribute, never the
key. Use `view_name()` / `view_name<-()` to read and write a view's
display name, `view_names()` for all names in a collection, and
`view_members()` for a `dock_view`'s ordered panel-id set.

Multi-view boards are defined by passing a named list to
[`new_dock_board()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock.md)'s
`layouts` argument: each **name is the view's id** (the container's key,
like a block id — minted when absent), each value is the panel
arrangement inside that view (a
[`dock_layout()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout.md),
or a raw list of block / extension IDs), split into the structure and
grid slots at construction. The display name is set on the layout via
`dock_layout(name = )`; with none set, a label is derived from the id.
The initially-active view is chosen by `new_dock_board(active = )` (a
view id), defaulting to the first; it is a property of the collection,
never of an individual layout. View CRUD is enabled unless the dock is
locked (see `is_dock_locked()`).

A stored grid must reference only panels in its view's membership
(`grid ⊆ membership`); the board is valid with no grid at all. Users do
not normally construct these objects directly; they pass a plain named
list to `new_dock_board(layouts = ...)`, which resolves, validates and
splits it.

## Examples

``` r
brd <- new_dock_board(
  blocks = c(
    dataset_1 = blockr.core::new_dataset_block(),
    head_1 = blockr.core::new_head_block()
  ),
  layouts = list(
    analysis = dock_layout("dataset_1", "head_1", name = "Analysis"),
    overview = dock_layout("dataset_1", name = "Overview")
  ),
  active = "overview"
)
view_names(board_views(brd))
#>   analysis   overview 
#> "Analysis" "Overview" 
view_members(board_views(brd)[["analysis"]])
#> [1] "block_panel-dataset_1" "block_panel-head_1"   
```
