# Dock layout: dockView's native representation

A `dock_layout` is dockView's own layout representation – the
`{grid, panels, activeGroup}` payload it renders – the wire form at the
client boundary. It is distinct from our
[dock_grid](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock-grid.md)
(a view's canonical geometry) and
[dock_view()](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
(a view's structure): a `dock_layout` is what the browser echoes and
what a restore pushes back, geometry fused with resolved panel content
in dockView's own shape. It is produced and consumed only at the
dockView seam and is never stored on the board nor passed through the
update lifecycle.

## Usage

``` r
new_dock_layout(x = list())

is_dock_layout(x)

validate_dock_layout(x)

as_dock_layout(x, ...)
```

## Arguments

- x:

  Object to wrap, validate, test, or cast.

- ...:

  Passed on to methods (e.g. `blocks` / `extensions` to resolve panel
  content when casting a `dock_grid`).

## Value

`new_dock_layout()`, `validate_dock_layout()` and `as_dock_layout()`
return a `dock_layout`; `is_dock_layout()` a boolean.

## Details

`new_dock_layout()` wraps a raw dockView state / payload list (a client
echo) as a `dock_layout`; `is_dock_layout()` is the class check;
`validate_dock_layout()` returns its input and errors on a malformed
shape. Cast across the seam with `as_dock_layout()` (build the dockView
payload from a
[dock_grid](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock-grid.md)
against the board's blocks and extensions),
[`as_dock_grid()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock-grid.md)
(canonical geometry from a layout) and
[as_dock_view()](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
(membership from a layout).
