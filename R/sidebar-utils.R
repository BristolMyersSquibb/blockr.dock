# Shared helpers for the block / link / stack menus.

# A fresh id: a non-empty string not already taken by `existing`. Shared
# by the stack-id and link-id validators.
is_new_id <- function(id, existing) {
  is_string(id) && nzchar(id) && !(id %in% existing)
}

# Normalise a menu-server argument that may arrive as a reactive, a bare
# value, or `NULL` into a zero-arg accessor. Passing a reactive opts the
# menu into live board sync; a bare value / `NULL` is read once and never
# changes.
as_arg_reactive <- function(x) {
  if (is.reactive(x)) {
    return(x)
  }
  function() x
}
