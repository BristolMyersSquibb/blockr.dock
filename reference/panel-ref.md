# Typed panel references

`blk()` and `ext()` construct references to a block or extension panel
for the `views$mod` panel-op grammar (see
[board_update](https://bristolmyerssquibb.github.io/blockr.core/reference/board_update.html)).
They are the public currency: a caller names a block or extension by its
**id**, never the `block_panel-` / `ext_panel-` wire prefix – the ref is
the codec, and [`as.character()`](https://rdrr.io/r/base/character.html)
on one yields the canonical panel-id encoding.

## Usage

``` r
blk(id, near = NULL, side = NULL, size = NULL)

ext(id, near = NULL, side = NULL, size = NULL)

is_panel_ref(x)

# S3 method for class 'panel_ref'
as.character(x, ...)
```

## Arguments

- id:

  A block or extension id (not the wire-prefixed panel id).

- near:

  A ref or bare id to anchor placement against.

- side:

  Placement direction relative to `near`: one of `within`, `left`,
  `right`, `above`, `below`.

- size:

  Target size ratio in (0, 1) – consumed by `resize`, and recorded on
  `add` for when the `set_size` floor lands (#320).

- x:

  An object.

- ...:

  Ignored.

## Value

`blk()` / `ext()` return a `panel_ref`.
[`as.character()`](https://rdrr.io/r/base/character.html) on one returns
its canonical panel id, and `is_panel_ref()` returns a boolean.

## Details

Each ref optionally carries its own placement hint, so a verb's operands
are an unnamed list of self-describing refs:
`add = list(blk("a", near = "b", side = "right"), ext("dag"))`. Which
hint fields are meaningful depends on the verb – `add` consumes `near` /
`side` / `size`, `move` consumes `near` / `side` – and a hint on a ref
used where no placement happens (`rm`, `select`, a `near` anchor, or the
[`dock_grid()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout.md)
/
[`panels()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout.md)
authoring DSL) is a loud error. Because the hints are constructor
arguments, a misspelled one (`blk("a", sise = 0.4)`) fails at the call
site with R's own unused-argument error, before any payload exists.

Bare id strings are accepted as sugar wherever a ref is, resolved
block-first with a hard error only on a true cross-namespace clash (an
id that is both a block and an extension), which then demands a typed
ref.

## Examples

``` r
blk("my_block", near = "other_block", side = "right")
#> <panel_ref> block_panel-my_block 
ext("dag")
#> <panel_ref> ext_panel-dag 
as.character(blk("my_block"))
#> [1] "block_panel-my_block"
```
