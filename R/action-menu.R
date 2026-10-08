# The menus the link and stack actions open in place of a sidebar form
# (#544). The server describes a menu and sends it; action-menu.js opens it
# as a `Blockr.menu` where the action's trigger happened (`at`, see
# new_action()) and sends a pick back on the action's `pick` input:
#
#   list(value = <the row's value>)       a row was picked
#   list(values = <the ticked values>)    a multi menu closed with its ticks
#   list(field = <the typed text>)        a name field was committed
#
# each with a nonce, so the same pick twice is two events. A menu may carry
# `back`, the menu it was opened from: Escape reopens that one.

open_action_menu <- function(at = NULL, caption = NULL, head = NULL,
                             items = list(), multi = FALSE, field = NULL,
                             filter = NULL, back = NULL,
                             session = get_session()) {

  msg <- list(
    pick = session$ns("pick"),
    at = at,
    caption = caption,
    head = head,
    items = items,
    multi = isTRUE(multi),
    field = field,
    filter = filter %||% (length(items) > 8L),
    back = back
  )

  session$sendCustomMessage("blockr-action-menu", msg)

  invisible(msg)
}

# A row of an action menu. `value` comes back as the pick.
menu_row <- function(label, value, meta = NULL, mark = NULL, icon = NULL,
                     danger = FALSE, quiet = FALSE, checked = NULL,
                     keywords = NULL, ...) {
  Filter(
    Negate(is.null),
    list(
      label = label,
      value = value,
      meta = meta,
      mark = mark,
      icon = icon,
      danger = if (isTRUE(danger)) TRUE,
      quiet = if (isTRUE(quiet)) TRUE,
      checked = checked,
      keywords = keywords,
      ...
    )
  )
}

menu_divider <- function() list(divider = TRUE)

menu_title <- function(title) list(title = title)

# A name field in a menu of its own. `taken` names are refused in place with
# `taken_msg`; an empty name is refused with `empty_msg` unless `empty_ok`.
menu_field <- function(value, placeholder = NULL, taken = character(),
                       taken_msg = "That name is taken",
                       empty_ok = FALSE, empty_msg = "A name is needed") {
  list(
    value = value %||% "",
    placeholder = placeholder,
    taken = as.list(taken),
    taken_msg = taken_msg,
    empty_ok = isTRUE(empty_ok),
    empty_msg = empty_msg
  )
}

# A row per board block, drawn as the "+" menu draws a block type: its mark,
# its name, its type as meta text.
block_menu_rows <- function(board, ids, value = identity, checked = NULL) {

  if (!length(ids)) {
    return(list())
  }

  blks <- board_blocks(board)[ids]
  meta <- blks_metadata(blks)

  lapply(
    seq_along(ids),
    function(i) {
      menu_row(
        label = block_label(board, ids[[i]]),
        value = value(ids[[i]]),
        meta = meta$name[i],
        mark = list(icon = meta$icon[i], category = meta$category[i]),
        keywords = ids[[i]],
        checked = if (!is.null(checked)) ids[[i]] %in% checked
      )
    }
  )
}

# The pick a menu sent: the row's value, the ticked values or the field's
# text, by what was asked for.
pick_value <- function(pick) pick[["value"]]
pick_values <- function(pick) as.character(unlist(pick[["values"]]))
pick_field <- function(pick) pick[["field"]]

# "step:arg" values let one `pick` input carry every step of a menu.
pick_step <- function(value) sub(":.*$", "", value)
pick_arg <- function(value) {
  if (grepl(":", value, fixed = TRUE)) sub("^[^:]*:", "", value) else NULL
}

action_menu_dep <- function() {
  htmltools::htmlDependency(
    "blockr-action-menu",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "action-menu.js"
  )
}
