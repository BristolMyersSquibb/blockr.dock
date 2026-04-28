# Dock views (layouts)

A `dock_board` can contain multiple views (global tabs), each with its
own DockView layout. Blocks and extensions are shared across views via
the board's DAG; view membership is a layout concern only.

## Usage

``` r
new_dock_layouts(...)

validate_dock_layouts(x)

dock_layouts(...)

dock_view(..., active = FALSE)

is_dock_layouts(x)

view_ids(x)

active_view(x)

active_view(x) <- value

view_can_crud(x)

dock_layouts(x) <- value

board_views(x)

as_dock_layouts(x, ...)
```

## Arguments

- ...:

  Generic consistency

- x:

  Object

- active:

  Logical; mark this view as the initially active one. At most one view
  in a `dock_layouts()` may be active.

- value:

  Replacement value

## Value

`dock_layouts()` returns a `dock_layouts` object. `dock_view()` returns
a list (the view spec) with the `active` attribute set when requested.
`is_dock_layouts()` returns a boolean. `active_view()` returns a string
and `active_view<-()` returns the modified `dock_layouts` object
invisibly. `view_ids()` returns all IDs (block + extension) found in a
layout specification. The `view_can_crud()` helper returns `FALSE` when
the dock is locked.

## Details

Multiple views are defined via `dock_layouts()`, which accepts named
list elements – each a (possibly nested) list of block and extension IDs
(the same format accepted by `create_dock_layout(grid = ...)`). A plain
named [`list()`](https://rdrr.io/r/base/list.html) is also accepted and
auto-detected by
[`new_dock_board()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock.md).
A view can be marked as the initially active one by tagging its spec
with `attr(view, "active") <- TRUE`, conveniently produced by
`dock_view()`. If no view is tagged, the first one is used. View CRUD is
enabled unless the dock is locked (see `is_dock_locked()`).

## Examples

``` r
# Explicit constructor (first view is active by default)
ly <- dock_layouts(
  Analysis = list("dataset_1", "head_1"),
  Overview = list("dag_extension")
)
is_dock_layouts(ly)
#> [1] TRUE
active_view(ly)
#> [1] "Analysis"

# Mark a specific view as initially active
ly2 <- dock_layouts(
  Analysis = list("dataset_1"),
  Overview = dock_view("dag_extension", active = TRUE)
)
active_view(ly2)
#> [1] "Overview"
```
