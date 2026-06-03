# Dock views (layouts)

A `dock_board` always holds a `dock_layouts` object (multi-view tabs).
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
```

## Arguments

- ...:

  Generic consistency

- x:

  A `dock_layout` (for `view_name()` / `view_name<-()`) or a
  `dock_layouts` collection (for `view_names()`).

- value:

  Replacement value

## Value

`is_dock_layouts()` returns a boolean. `validate_dock_layouts()` returns
its input and throws on error. `active_view()` returns the active view's
id, or `NULL` when no view is active, and `active_view<-()` returns the
modified `dock_layouts` (or `dock_board`) object invisibly.
`view_name()` returns a view's explicit display name (or `NULL`),
`view_name<-()` the modified `dock_layout`, and `view_names()` a
character vector of display labels keyed by view id (derived from the id
where a view has no explicit name).

## Details

Each view carries a stable, immutable **id** (the key into the
`dock_layouts` collection) that is distinct from its editable display
**name**. This mirrors the id / name separation used for blocks (an
immutable id keys the collection;
[`blockr.core::block_name()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_name.html)
is an editable label). The id is minted once when the view is created
and never changes — rename only rewrites the name attribute, never the
key. Use `view_name()` / `view_name<-()` to read and write a view's
display name and `view_names()` for all names in a collection.

Multi-view boards are defined by passing a named list to
[`new_dock_board()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock.md)'s
`layouts` argument: each **name is the view's id** (the container's key,
like a block id — minted when absent), each value is the panel
arrangement inside that view (a
[`dock_layout()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout.md),
or a raw list of block / extension IDs). The display name is set on the
layout via `dock_layout(name = )`; with none set, a label is derived
from the id. A view can be marked as the initially-active one by passing
`active = TRUE` to
[`dock_layout()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout.md);
if none is marked, the first one is used. View CRUD is enabled unless
the dock is locked (see `is_dock_locked()`).

Users do not normally construct a `dock_layouts` directly; instead they
pass a plain named list to `new_dock_board(layouts = ...)`, which
resolves, validates and wraps it (minting any absent ids).

## Examples

``` r
brd <- new_dock_board(
  blocks = c(
    dataset_1 = blockr.core::new_dataset_block(),
    head_1 = blockr.core::new_head_block()
  ),
  layouts = list(
    analysis = dock_layout("dataset_1", "head_1", name = "Analysis"),
    overview = dock_layout("dataset_1", name = "Overview", active = TRUE)
  )
)
view_names(board_layouts(brd))
#>   analysis   overview 
#> "Analysis" "Overview" 
```
