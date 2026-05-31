# Structural + gating checks for the `extensions` slice of a board update.
# Only field-level `mod` is supported (no add / rm lifecycle); each delta
# key must name an externally controllable variable of its extension, the
# write face of the `state` the serializer already reads. Mirrors
# blockr.core's `validate_board_update_blocks()` gate for blocks.
validate_extensions_delta <- function(extensions, board) {

  if (!is.list(extensions)) {
    blockr_abort(
      "`extensions` must be a list with an optional `mod` component.",
      class = "dock_extensions_delta_invalid"
    )
  }

  unknown_keys <- setdiff(names(extensions), "mod")

  if (length(unknown_keys)) {
    blockr_abort(
      "Unknown `extensions` slice key{?s} {unknown_keys}; only `mod` is ",
      "supported.",
      class = "dock_extensions_delta_invalid"
    )
  }

  mod <- extensions[["mod"]]

  if (!length(mod)) {
    return(invisible(extensions))
  }

  if (length(names(mod)) != length(mod) || any(!nzchar(names(mod)))) {
    blockr_abort(
      "Expecting the `extensions$mod` component to be a named list keyed ",
      "by extension ID.",
      class = "dock_extensions_delta_unnamed"
    )
  }

  exts <- dock_extensions(board)

  unknown_id <- setdiff(names(mod), names(exts))

  if (length(unknown_id)) {
    blockr_abort(
      "Modified extension entries reference unknown ID{?s} {unknown_id}.",
      class = "dock_extensions_delta_unknown_id"
    )
  }

  ext_list <- as.list(exts)

  for (id in names(mod)) {

    delta <- mod[[id]]

    if (!is.list(delta) || length(names(delta)) != length(delta) ||
          any(!nzchar(names(delta)))) {
      blockr_abort(
        "Expecting each `extensions$mod` entry to be a named list of ",
        "constructor argument values.",
        class = "dock_extensions_delta_entry_invalid"
      )
    }

    if (!length(delta)) {
      blockr_abort(
        "`extensions$mod` entry `{id}` is empty; omit it or supply at ",
        "least one argument.",
        class = "dock_extensions_delta_entry_empty"
      )
    }

    extra <- setdiff(names(delta), extension_external_ctrl_vars(ext_list[[id]]))

    if (length(extra)) {
      blockr_abort(
        "Extension `{id}` mod delta contains argument{?s} {extra} which ",
        "{?is/are} not externally controllable.",
        class = "dock_extensions_delta_not_ctrl"
      )
    }
  }

  invisible(extensions)
}

# Write each `extensions$mod` delta into the live extension `state`
# reactiveVals built by `board_server_callback()` (reachable via
# `dock_mgr$ext_res`). Mirrors blockr.core's `apply_block_mod_delta()`:
# only entries that actually change are written, so unchanged values do
# not spuriously invalidate downstream reactives.
apply_extensions_mod <- function(mod, ext_res) {

  for (ext_id in names(mod)) {

    state_rvs <- ext_res[[ext_id]][["state"]]
    delta <- mod[[ext_id]]

    for (nm in names(delta)) {

      if (!identical(reval_if(state_rvs[[nm]]), delta[[nm]])) {
        state_rvs[[nm]](delta[[nm]])
      }
    }
  }

  invisible()
}
