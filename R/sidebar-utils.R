# Shared helpers for the instance-backed menus (stack, link). These
# menus own validation of their committed payload and keep themselves in
# sync with the board, so the consumer (e.g. blockr.dock) shrinks to a
# thin adapter. We reuse blockr.core where it already has a helper
# (`is_string()`, `notify()`) and only add what it lacks.

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

# `board_*_ids()` guarded against a `NULL` board (the snapshot / no-sync
# path passes `board = NULL`).
safe_ids <- function(board, fun) {
  if (is.null(board)) character() else fun(board)
}
