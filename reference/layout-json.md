# Layout serialization and inspection

Read and write the JSON form of a
[dock_layout](https://rdrr.io/r/graphics/layout.html), and inspect the
panel IDs it references. These are the canonical accessors for the
serialized layout format — downstream tooling should call them rather
than re-implement the format.

## Usage

``` r
panel_obj_ids(ids)

layout_panel_ids(layout)

layout_to_json(x, ...)

layout_from_json(x, blocks = NULL, extensions = NULL)
```

## Arguments

- ids:

  Character vector of panel IDs.

- layout:

  A `dock_layout` object.

- x:

  A `dock_layout` (for `layout_to_json()`), or a JSON string / parsed
  spec list (for `layout_from_json()`).

- ...:

  Forwarded to
  [`jsonlite::toJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html).

- blocks, extensions:

  Optional board components used to resolve and validate bare IDs in
  `layout_from_json()`.

## Value

`layout_to_json()` returns a JSON string; `layout_from_json()` a
`dock_layout`. `layout_panel_ids()` and `panel_obj_ids()` return
character vectors.

## Details

`layout_to_json()` renders a layout as a JSON string;
`layout_from_json()` is the inverse. The shape is a recursive tree: the
top object carries `orientation`, `children`, an optional `sizes`, and
an optional `focus` (the panel with current focus); a child is either a
bare string (single-panel leaf), an object with `panels` / optional
`active` (tabbed leaf), or an object with `children` / optional `sizes`
(nested branch). Sizes are ratios summing to 1, omitted when even.

`layout_from_json()` accepts a JSON string or an already-parsed spec
list and delegates to
[as_dock_layout()](https://rdrr.io/r/graphics/layout.html); when
`blocks` / `extensions` are supplied, bare IDs are resolved to canonical
panel IDs and the result is validated (an unknown panel or malformed
arrangement throws the usual classed error).

`layout_panel_ids()` returns the canonical panel IDs (`block_panel-…` /
`ext_panel-…`) referenced by a layout; `panel_obj_ids()` strips those
prefixes back to bare block / extension IDs.

## Examples

``` r
ly <- dock_layout("a", panels("b", "c", active = "c"), sizes = c(0.3, 0.7))

json <- layout_to_json(ly)
cat(json)
#> {"orientation":"horizontal","children":["a",{"panels":["b","c"],"active":"c"}],"sizes":[0.3,0.7]}

identical(layout_from_json(json), ly)
#> [1] TRUE
```
