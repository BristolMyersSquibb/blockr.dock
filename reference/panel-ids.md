# Panel IDs of a grid or layout

`layout_panel_ids()` returns the canonical panel IDs (`block_panel-...`
/ `ext_panel-...`) a
[dock_grid](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock-grid.md)
or
[dock_layout](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock-layout.md)
references; `panel_obj_ids()` strips those prefixes back to the bare
block / extension IDs.

## Usage

``` r
panel_obj_ids(ids)

layout_panel_ids(layout)
```

## Arguments

- ids:

  Character vector of panel IDs.

- layout:

  A
  [dock_grid](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock-grid.md)
  or
  [dock_layout](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock-layout.md).

## Value

Character vectors of IDs.
