# ID utilities

Objects, such as blocks and dock extensions carry their own IDs. These
can be converted into other ID types, such as panel IDs or "handle" IDs.
Panel IDs are used to refer to dock panels, while handle IDs provide
"handles" for DOM manipulations. All such IDs inherit from `dock_id` and
panel IDs additionally inherit from `dock_panel_id`, while handle IDs
inherit from `dock_handle_id`. For panel IDs, depending on whether the
panel is showing a block or an extension, the inheritance structure
additionally contains `block_panel_id` or `ext_panel_id`, respectively.
Similarly, for handle IDs, we have `block_handle_id` and
`ext_handle_id`. All `dock_id` objects can be converted back to native
IDs, by calling `as_obj_id()`. The utility function `dock_id()` returns
a (possibly namespaced) ID of the `dock` instance that is used to manage
all visible panels.

## Usage

``` r
dock_id(ns = NULL)

as_dock_panel_id(x)

as_obj_id(x)

as_block_panel_id(x)

as_ext_panel_id(x)

as_dock_handle_id(x)

as_block_handle_id(x)

as_ext_handle_id(x)
```

## Arguments

- ns:

  Namespace prefix

- x:

  Object

## Value

Coercion functions `as_block_panel_id()`, `as_ext_panel_id()`,
`as_block_handle_id()` and `as_ext_handle_id()` return objects that
inherit from `block_panel_id`, `ext_panel_id`, `block_handle_id` and
`ext_handle_id` as classed character vectors. The less specific coercion
functions `as_dock_panel_id()` and `as_dock_handle_id()` return objects
that inherit from `dock_panel_id` and `dock_handle_id`, in addition to a
sub-class such as `block_panel_id` or `ext_panel_id` (in the case of
`as_dock_panel_id()`). If a mix of sub-classes is returned, this will be
represented by a list of classed character vectors. Finally,
`as_obj_id()` returns a character vector, as does `dock_id()`.

## Examples

``` r
blks <- c(
  a = blockr.core::new_dataset_block(),
  b = blockr.core::new_head_block()
)

ext <- new_edit_board_extension()

as_dock_panel_id(blks)
#> [1] "block_panel-a" "block_panel-b"
#> attr(,"class")
#> [1] "block_panel_id" "dock_panel_id"  "dock_id"       
as_dock_panel_id(ext)
#> [1] "ext_panel-edit_board_extension"
#> attr(,"class")
#> [1] "ext_panel_id"  "dock_panel_id" "dock_id"      

identical(names(blks), as_obj_id(as_block_panel_id(blks)))
#> [1] TRUE

as_dock_handle_id(blks)
#> [1] "block_handle-a" "block_handle-b"
#> attr(,"class")
#> [1] "block_handle_id" "dock_handle_id"  "dock_id"        
as_dock_handle_id(ext)
#> [1] "ext_handle-edit_board_extension"
#> attr(,"class")
#> [1] "ext_handle_id"  "dock_handle_id" "dock_id"       

identical(names(blks), as_obj_id(as_block_handle_id(blks)))
#> [1] TRUE
```
