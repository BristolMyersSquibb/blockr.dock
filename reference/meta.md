# Get block metadata

Returns various metadata for blocks or block categories, as well as
styling for block icons.

## Usage

``` r
blks_metadata(blocks)

blk_color(category)

blk_icon_data_uri(icon_svg, color, size = 48, mode = c("uri", "inline"))

block_status_badge(status, error_count = 0L)
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

- status:

  A block eval status: `stale`, `waiting`, `unset` and `failed` carry a
  badge; `ready` and `unevaluated` carry none; any other value yields no
  badge. The `size` field is the coloured dot's pixel diameter and
  `ring` the width of the ring around it, both shared by the dock card
  icon and the DAG node badge.

- error_count:

  Number of error conditions the block has raised. A positive count
  promotes the badge to `failed`, catching render-phase errors that
  leave the eval status `ready`. A `stale` block is exempt: its
  conditions were raised against inputs it no longer has.

## Value

Metadata is returned from `blks_metadata()` as a `data.frame` with each
row corresponding to a block. Both `blk_color()` and
`blk_icon_data_uri()` return character vectors. The badge
`block_status_badge()` draws is a list with its `label`; its fill as the
blockr.ui `token` and that token's light value `color`; `hollow` and
`outline`, for a badge drawn as an `outline`-wide ring in its fill; and
the dot's `size` and the width of the `ring` around it, in `ring_token`
with the light value `ring_color`. It is `NULL` for a status with no
badge.

## Details

- `blks_metadata()`: Retrieves metadata given a `block` or `blocks`
  object from the block registry. Can also handle blocks which are not
  registered and provides default values in that case. The `color`
  column is the category's colour, from
  [`blockr.ui::category_color()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/block_mark.html).

- `blk_color()`: Deprecated. The category colours are blockr.ui's, and
  [`blockr.ui::category_color()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/block_mark.html),
  which this now calls, looks them up.

- `blk_icon_data_uri()`: Deprecated. Processes block icons to add color
  and turn them into square-shaped icons. For the block's mark as an
  image, use
  [`blockr.ui::block_mark_svg()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/block_mark.html),
  which draws it from the block's glyph and category.

- `block_status_badge()`: Derives a block's status badge from its eval
  status and error count – the single derivation the dock card icon and
  the blockr.dag node badge share, so both show the same status. Returns
  a styling list (draw the badge) or `NULL` (no badge).

## Examples

``` r
blk <- blockr.core::new_dataset_block()
blks_metadata(blk)
#>                     id          name                     description
#> fieexomb dataset_block dataset block Choose a dataset from a package
#>                                                                                                                                                                                                                                                                                                                                                                                                        details
#> fieexomb This data block allows to select a dataset from a package, such as the datasets package available in most R installations as one of the packages with "recommended" priority. The source package can be chosen at time of block instantiation and can be set to any R package, for which then a set of candidate datasets is computed. This includes exported objects that inherit from `data.frame`.
#>                                                                                    link
#> fieexomb https://bristolmyerssquibb.github.io/blockr.core/reference/new_data_block.html
#>                                                                                                                                                                                  guidance
#> fieexomb Loads a data.frame that ships with an installed package. Set `package` to the source package (default "datasets"), then `dataset` to the name of a data.frame object it exports.
#>                                        keywords category
#> fieexomb dataset, data, import, package, source    input
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  icon
#> fieexomb <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-database " style="height:1em;width:1em;fill:currentColor;vertical-align:-0.125em;" aria-hidden="true" role="img" ><path d="M4.318 2.687C5.234 2.271 6.536 2 8 2s2.766.27 3.682.687C12.644 3.125 13 3.627 13 4c0 .374-.356.875-1.318 1.313C10.766 5.729 9.464 6 8 6s-2.766-.27-3.682-.687C3.356 4.875 3 4.373 3 4c0-.374.356-.875 1.318-1.313ZM13 5.698V7c0 .374-.356.875-1.318 1.313C10.766 8.729 9.464 9 8 9s-2.766-.27-3.682-.687C3.356 7.875 3 7.373 3 7V5.698c.271.202.58.378.904.525C4.978 6.711 6.427 7 8 7s3.022-.289 4.096-.777A4.92 4.92 0 0 0 13 5.698ZM14 4c0-1.007-.875-1.755-1.904-2.223C11.022 1.289 9.573 1 8 1s-3.022.289-4.096.777C2.875 2.245 2 2.993 2 4v9c0 1.007.875 1.755 1.904 2.223C4.978 15.71 6.427 16 8 16s3.022-.289 4.096-.777C13.125 14.755 14 14.007 14 13V4Zm-1 4.698V10c0 .374-.356.875-1.318 1.313C10.766 11.729 9.464 12 8 12s-2.766-.27-3.682-.687C3.356 10.875 3 10.373 3 10V8.698c.271.202.58.378.904.525C4.978 9.71 6.427 10 8 10s3.022-.289 4.096-.777A4.92 4.92 0 0 0 13 8.698Zm0 3V13c0 .374-.356.875-1.318 1.313C10.766 14.729 9.464 15 8 15s-2.766-.27-3.682-.687C3.356 13.875 3 13.373 3 13v-1.302c.271.202.58.378.904.525C4.978 12.71 6.427 13 8 13s3.022-.289 4.096-.777c.324-.147.633-.323.904-.525Z"></path></svg>
#>                                                                                                   arguments
#> fieexomb Selects the dataset to use., iris, string, R package to source the dataset from., datasets, string
#>                examples     package   color
#> fieexomb iris, datasets blockr.core #0072b2

block_status_badge("waiting")
#> $color
#> [1] "#d97706"
#> 
#> $token
#> [1] "--blockr-color-border-warning"
#> 
#> $label
#> [1] "Waiting for a data input"
#> 
#> $hollow
#> [1] TRUE
#> 
#> $outline
#> [1] 1.5
#> 
#> $size
#> [1] 8
#> 
#> $ring
#> [1] 2
#> 
#> $ring_color
#> [1] "#ffffff"
#> 
#> $ring_token
#> [1] "--blockr-color-bg-surface"
#> 
```
