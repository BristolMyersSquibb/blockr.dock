# Extension metadata

The `description` argument of
[`new_dock_extension()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/extension.md)
accepts either a bare string or a structured `ext_meta` object built
with `new_ext_meta()`. The structured form carries model-facing
documentation for a client driving the extension through
`modify_extension`: a free-text `description`, per-variable `arguments`
documentation keyed by externally controllable variable (see
[`external_ctrl_vars()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_name.html)),
worked `examples` and free-text `guidance` on how to drive it.
`is_ext_meta()` checks the class and `ext_meta()` reads the whole
metadata off an extension, coercing a bare-string description to an
`ext_meta` whose only populated slot is `description`; the individual
components are read with `ext_desc()`, `ext_args()`, `ext_examples()`
and `ext_guidance()`. `extension_description()` remains as a deprecated
alias of `ext_desc()`.

## Usage

``` r
new_ext_meta(
  description = NULL,
  arguments = NULL,
  examples = list(),
  guidance = NULL
)

is_ext_meta(x)

ext_meta(x)

ext_desc(x)

ext_args(x)

ext_examples(x)

ext_guidance(x)

extension_description(x)
```

## Arguments

- description:

  Free-text summary of what the extension is

- arguments:

  Per-variable documentation for the externally controllable variables,
  either a named character vector (variable to description) or a
  [`new_block_args()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block_arg.html)
  object; keyed by controllable variable

- examples:

  List of worked configurations, each a named list keyed by controllable
  variable (a `modify_extension`-shaped payload)

- guidance:

  Free-text steering on how to drive the extension, distinct from the
  human-facing `description`

- x:

  An `ext_meta` object for `is_ext_meta()`, a `dock_extension` for
  `ext_meta()` and the per-component accessors

## Value

`new_ext_meta()` returns an `ext_meta` object (a list with entries
`description`, `arguments`, `examples` and `guidance`), `is_ext_meta()`
a boolean and `ext_meta()` the normalized `ext_meta` an extension
carries. The per-component accessors return that component: `ext_desc()`
a string or `NULL`, `ext_args()` a `block_args`, `ext_examples()` a list
and `ext_guidance()` a string or `NULL`.

## Details

Per-variable `arguments` reuse blockr.core's block-argument
specification: pass a named character vector (variable to description)
for the common case, or a
[`new_block_args()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block_arg.html)
object to attach a machine-readable `type` (via the `arg_*()`
constructors) and a worked `example` to each variable.

## Examples

``` r
new_ext_meta(
  "Workflow diagram of the board's blocks and links.",
  arguments = c(positions = "JSON object mapping block id to {x, y} coords.")
)
#> $description
#> [1] "Workflow diagram of the board's blocks and links."
#> 
#> $arguments
#> $positions
#> $description
#> [1] "JSON object mapping block id to {x, y} coords."
#> 
#> $example
#> NULL
#> 
#> $type
#> NULL
#> 
#> attr(,"class")
#> [1] "block_arg"
#> 
#> attr(,"class")
#> [1] "block_args"
#> 
#> $examples
#> list()
#> 
#> $guidance
#> NULL
#> 
#> attr(,"class")
#> [1] "ext_meta"

ext_meta(new_edit_board_extension())
#> $description
#> NULL
#> 
#> $arguments
#> list()
#> attr(,"class")
#> [1] "block_args"
#> 
#> $examples
#> list()
#> 
#> $guidance
#> NULL
#> 
#> attr(,"class")
#> [1] "ext_meta"
```
