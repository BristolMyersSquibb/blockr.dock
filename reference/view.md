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

  Object

- value:

  Replacement value

## Value

`is_dock_layouts()` returns a boolean. `validate_dock_layouts()` returns
its input and throws on error. `active_view()` returns a string and
`active_view<-()` returns the modified `dock_layouts` (or `dock_board`)
object invisibly.

## Details

Multi-view boards are defined by passing a named list to
[`new_dock_board()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock.md)'s
`layouts` argument: each name is a view, each value is the panel
arrangement inside that view (a
[`dock_layout()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout.md),
or a raw list of block / extension IDs). A view can be marked as the
initially-active one by passing `active = TRUE` to
[`dock_layout()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout.md);
if none is marked, the first one is used. View CRUD is enabled unless
the dock is locked (see `is_dock_locked()`).

Users do not normally construct a `dock_layouts` directly; instead they
pass a plain named list to `new_dock_board(layouts = ...)`, which
validates and wraps it.

## Examples

``` r
brd <- new_dock_board(
  blocks = c(
    dataset_1 = blockr.core::new_dataset_block(),
    head_1 = blockr.core::new_head_block()
  ),
  layouts = list(
    Analysis = list("dataset_1", "head_1"),
    Overview = dock_layout("dataset_1", active = TRUE)
  )
)
active_view(brd)
#> [1] "Overview"
```
