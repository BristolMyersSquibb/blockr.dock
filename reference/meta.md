# Get block metadata

Returns various metadata for blocks or block categories, as well as
styling for block icons.

## Usage

``` r
blks_metadata(blocks)

blk_color(category)

blk_icon_data_uri(icon_svg, color, size = 48, mode = c("uri", "inline"))
```

## Arguments

- blocks:

  Blocks passed as `blocks` or `block` object

- category:

  Block category

- icon_svg:

  Character string containing the SVG icon markup

- color:

  Hex color code for the background

- size:

  Numeric size in pixels (default: 48)

- mode:

  Switch between URI and inline HTML mode

## Value

Metadata is returned from `blks_metadata()` as a `data.frame` with each
row corresponding to a block. Both `blk_color()` and
`blk_icon_data_uri()` return character vectors.

## Details

- `blks_metadata()`: Retrieves metadata given a `block` or `blocks`
  object from the block registry. Can also handle blocks which are not
  registered and provides default values in that case.

- `blk_color()`: Produces colors using the Okabe-Ito colorblind-friendly
  palette for a character vector of block categories.

- `blk_icon_data_uri()`: Processes block icons to add color and turn
  them into square-shaped icons.

## Examples

``` r
blk <- blockr.core::new_dataset_block()
meta <- blks_metadata(blk)

col <- blk_color(meta$category)
blk_icon_data_uri(meta$icon, col)
#> [1] "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciCiAgICAgICAgd2lkdGg9\nIjQ4IiBoZWlnaHQ9IjQ4IiB2aWV3Qm94PSIwIDAgNDggNDgiPgogICAgICA8cmVjdCB3aWR0\naD0iNDgiIGhlaWdodD0iNDgiIHJ4PSI3LjIiIHJ5PSI3LjIiIGZpbGw9InJnYmEoMCwgMTE0\nLCAxNzgsIDAuMykiLz4KICAgICAgPGcgdHJhbnNmb3JtPSJ0cmFuc2xhdGUoOS42LCA5LjYp\nIiBmaWxsPSIjMDA3MkIyIj4KICAgICAgICA8c3ZnIHdpZHRoPSIyOC44IiBoZWlnaHQ9IjI4\nLjgiIHZpZXdCb3g9IjAgMCAxNiAxNiI+PHBhdGggZD0iTTQuMzE4IDIuNjg3QzUuMjM0IDIu\nMjcxIDYuNTM2IDIgOCAyczIuNzY2LjI3IDMuNjgyLjY4N0MxMi42NDQgMy4xMjUgMTMgMy42\nMjcgMTMgNGMwIC4zNzQtLjM1Ni44NzUtMS4zMTggMS4zMTNDMTAuNzY2IDUuNzI5IDkuNDY0\nIDYgOCA2cy0yLjc2Ni0uMjctMy42ODItLjY4N0MzLjM1NiA0Ljg3NSAzIDQuMzczIDMgNGMw\nLS4zNzQuMzU2LS44NzUgMS4zMTgtMS4zMTNaTTEzIDUuNjk4VjdjMCAuMzc0LS4zNTYuODc1\nLTEuMzE4IDEuMzEzQzEwLjc2NiA4LjcyOSA5LjQ2NCA5IDggOXMtMi43NjYtLjI3LTMuNjgy\nLS42ODdDMy4zNTYgNy44NzUgMyA3LjM3MyAzIDdWNS42OThjLjI3MS4yMDIuNTguMzc4Ljkw\nNC41MjVDNC45NzggNi43MTEgNi40MjcgNyA4IDdzMy4wMjItLjI4OSA0LjA5Ni0uNzc3QTQu\nOTIgNC45MiAwIDAgMCAxMyA1LjY5OFpNMTQgNGMwLTEuMDA3LS44NzUtMS43NTUtMS45MDQt\nMi4yMjNDMTEuMDIyIDEuMjg5IDkuNTczIDEgOCAxcy0zLjAyMi4yODktNC4wOTYuNzc3QzIu\nODc1IDIuMjQ1IDIgMi45OTMgMiA0djljMCAxLjAwNy44NzUgMS43NTUgMS45MDQgMi4yMjND\nNC45NzggMTUuNzEgNi40MjcgMTYgOCAxNnMzLjAyMi0uMjg5IDQuMDk2LS43NzdDMTMuMTI1\nIDE0Ljc1NSAxNCAxNC4wMDcgMTQgMTNWNFptLTEgNC42OThWMTBjMCAuMzc0LS4zNTYuODc1\nLTEuMzE4IDEuMzEzQzEwLjc2NiAxMS43MjkgOS40NjQgMTIgOCAxMnMtMi43NjYtLjI3LTMu\nNjgyLS42ODdDMy4zNTYgMTAuODc1IDMgMTAuMzczIDMgMTBWOC42OThjLjI3MS4yMDIuNTgu\nMzc4LjkwNC41MjVDNC45NzggOS43MSA2LjQyNyAxMCA4IDEwczMuMDIyLS4yODkgNC4wOTYt\nLjc3N0E0LjkyIDQuOTIgMCAwIDAgMTMgOC42OThabTAgM1YxM2MwIC4zNzQtLjM1Ni44NzUt\nMS4zMTggMS4zMTNDMTAuNzY2IDE0LjcyOSA5LjQ2NCAxNSA4IDE1cy0yLjc2Ni0uMjctMy42\nODItLjY4N0MzLjM1NiAxMy44NzUgMyAxMy4zNzMgMyAxM3YtMS4zMDJjLjI3MS4yMDIuNTgu\nMzc4LjkwNC41MjVDNC45NzggMTIuNzEgNi40MjcgMTMgOCAxM3MzLjAyMi0uMjg5IDQuMDk2\nLS43NzdjLjMyNC0uMTQ3LjYzMy0uMzIzLjkwNC0uNTI1WiI+PC9wYXRoPjwvc3ZnPgogICAg\nICA8L2c+CiAgICA8L3N2Zz4="
```
