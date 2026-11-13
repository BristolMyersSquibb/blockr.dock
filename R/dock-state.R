# The dock's state, as an app script gives it in R, written once, as the
# script runs, in the shape of the schema of the state, as the section
# "Defining the state in R" of page 1.2.1 has it
# (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.1).
# The board holds it at /plugins/blockr.dock-dock_plugin, and core's
# `blockr_ser()` writes it by `js/dock.schema.json`.
# A panel there is `list(block = "a")` or
# `list(plugin = "blockr.dag-dag_plugin")`.

# The dock's state from `views`, `folders`, `active` and `visible`, as
# `new_dock_plugin()` takes them.
# The state is an empty list where the script gives nothing, so that a board
# holds no state for the dock.
dock_state <- function(views, folders, active, visible) {

  views <- check_nav_list(views, "view")
  folders <- check_nav_list(folders, "folder")
  visible <- check_visible(visible)

  if (not_null(active) && (!is_string(active) || is.na(active))) {
    blockr_abort(
      "The argument `active` is the id of a view, or `NULL` for the first ",
      "view of the views menu.",
      class = "dock_active_invalid"
    )
  }

  if (!length(views) && !length(folders) && !length(visible) &&
        is.null(active)) {
    return(list())
  }

  ids <- nav_ids(views, folders)

  view_holders <- set_names(chr_ply(views, holder_of), ids[["views"]])
  folder_holders <- set_names(chr_ply(folders, holder_of), ids[["folders"]])

  check_holders(view_holders, folder_holders)

  if (is.null(active)) {
    active <- first_nav_view(view_holders, folder_holders)
  } else if (!active %in% ids[["views"]]) {
    blockr_abort(
      "The argument `active` names the view {active}, which `views` lacks.",
      class = "dock_active_invalid"
    )
  }

  grids <- set_names(lapply(views, view_grid_state), ids[["views"]])
  check_placed_once(grids)
  check_plugin_ids(grids)

  index <- nav_index(view_holders, folder_holders)

  list(
    views = set_names(
      Map(
        function(view, id, holder, index, grid) {
          list(
            name = coal(view[["name"]], nav_name(id), fail_all = FALSE),
            folder = nav_folder(holder),
            index = index,
            grid = grid
          )
        },
        views,
        ids[["views"]],
        view_holders,
        index[["views"]],
        grids
      ),
      ids[["views"]]
    ),
    folders = set_names(
      Map(
        function(folder, holder, index) {
          list(
            name = folder[["name"]],
            folder = nav_folder(holder),
            index = index
          )
        },
        folders,
        folder_holders,
        index[["folders"]]
      ),
      ids[["folders"]]
    ),
    active = active,
    visible = visible
  )
}

# The views or the folders that a script gives, where `what` is "view" or
# "folder": a list of them, each written with `dock_view()` or `dock_folder()`.
check_nav_list <- function(x, what) {

  if (is.null(x)) {
    return(list())
  }

  ctor <- paste0("dock_", what)
  plain <- is.list(x) && !is.object(x)
  bad <- if (plain) as.character(which(!lgl_ply(x, inherits, ctor)))

  if (plain && !length(bad)) {
    return(x)
  }

  abort_each(
    paste0(
      "The argument `", what, "s` is a list of ", what, "s, each written ",
      "with `", ctor, "()`",
      if (length(bad)) {
        paste0(
          ", which its ", if (length(bad) > 1L) "entries " else "entry ",
          paste_enum(bad, quotes = ""), if (length(bad) > 1L) " are" else " is",
          " not"
        )
      },
      "."
    ),
    class = paste0("dock_", what, "s_invalid")
  )
}

# The sections that a script gives each block's card, by the block's id, as
# the state holds them: an entry that names both sections goes, since a card
# without an entry shows both.
check_visible <- function(visible) {

  if (is.null(visible)) {
    return(list())
  }

  if (!is.list(visible) || is.object(visible)) {
    blockr_abort(
      "The argument `visible` is a list of the sections that each block's ",
      "card shows, by the block's id.",
      class = "dock_visible_invalid"
    )
  }

  if (!length(visible)) {
    return(list())
  }

  ids <- names(visible)

  if (is.null(ids) || anyNA(ids) || !all(nzchar(ids))) {
    blockr_abort(
      "The argument `visible` gives the sections of each card by the id of ",
      "the card's block, so each of its entries has a name.",
      class = "dock_visible_invalid"
    )
  }

  twice <- unique(ids[duplicated(ids)])

  if (length(twice)) {
    blockr_abort(
      "The argument `visible` gives the sections of each card once, while it ",
      "gives those of the block{?s} {twice} more than once.",
      class = "dock_visible_invalid"
    )
  }

  sections <- c("inputs", "outputs")

  plain <- lgl_ply(
    visible,
    function(x) is.character(x) && !is.object(x) && !anyNA(x)
  )

  if (!all(plain)) {
    blockr_abort(
      "The argument `visible` gives the sections of each card as strings, ",
      "while it gives none for the block{?s} {ids[!plain]}.",
      class = "dock_visible_invalid"
    )
  }

  unknown <- setdiff(unlst(visible), sections)

  if (length(unknown)) {
    blockr_abort(
      "A card's sections are {sections}, while `visible` names {unknown}.",
      class = "dock_visible_invalid"
    )
  }

  shown <- lapply(visible, unique)

  shown[lengths(shown) < length(sections)]
}

# The ids of the views and folders, as two vectors: their names in `views` and
# `folders`, and an id that R draws for each that the script leaves unnamed,
# by the blockr option `rand_id`, as core draws the ids of blocks.
# No drawn id is an id that the script gives a view or folder.
nav_ids <- function(views, folders) {

  given <- function(x) {
    ids <- coal(names(x), rep("", length(x)), fail_all = FALSE)
    ids[is.na(ids)] <- ""
    ids
  }

  ids <- list(views = given(views), folders = given(folders))

  for (what in c("view", "folder")) {

    named <- ids[[paste0(what, "s")]]
    named <- named[nzchar(named)]
    twice <- unique(named[duplicated(named)])

    if (length(twice)) {
      blockr_abort(
        "Each {what} has an id of its own, its name in the list of {what}s, ",
        "while {twice} name{?s/} more than one.",
        class = "dock_nav_ids_invalid"
      )
    }
  }

  taken <- unlst(ids)
  taken <- taken[nzchar(taken)]

  for (what in names(ids)) {

    drawn <- !nzchar(ids[[what]])
    ids[[what]][drawn] <- rand_names(taken, sum(drawn))

    taken <- c(taken, ids[[what]][drawn])
  }

  ids
}

# The folder that a view or folder sits in, as its id, or "" for the top.
holder_of <- function(x) {
  coal(x[["folder"]], "", fail_all = FALSE)
}

# The folder of a view or folder as the state holds it: its id, or `NULL` for
# the top.
nav_folder <- function(holder) {
  if (nzchar(holder)) holder
}

# The name of a view that the script gives none, made from its id, as "Adverse
# events" for `adverse_events`, or the id itself where that name would hold
# nothing but spaces.
nav_name <- function(id) {

  name <- id_to_sentence_case(id)

  if (nzchar(trimws(name))) name else id
}

# Each view and folder sits in a folder that the script gives, or at the top,
# and no folder sits in itself.
check_holders <- function(view_holders, folder_holders) {

  folder_ids <- names(folder_holders)

  lost <- function(holders, what) {

    hit <- nzchar(holders) & !holders %in% folder_ids

    paste0(
      "the ", what, " ", names(holders)[hit], " names ", holders[hit],
      recycle0 = TRUE
    )
  }

  lost <- c(lost(view_holders, "view"), lost(folder_holders, "folder"))

  if (length(lost)) {
    blockr_abort(
      "A view or folder sits in a folder that `folders` gives, or at the top, ",
      "while {lost}.",
      class = "dock_folder_unknown"
    )
  }

  inside <- folders_in_themselves(folder_holders)

  if (length(inside)) {
    blockr_abort(
      "No folder sits in itself, while the folder{?s} {inside} {?does/do}, ",
      "through the folders that hold {?it/them}.",
      class = "dock_folder_in_itself"
    )
  }

  invisible()
}

# The grid of a view as the dock's state holds it: its own, or the default
# grid over the panels that it is given.
view_grid_state <- function(view) {

  resolve <- function(ref) state_panel(ref[["id"]], ref[["type"]])

  if (not_null(view[["grid"]])) {
    return(grid_state(view[["grid"]], resolve))
  }

  default_grid(lapply(view[["panels"]], resolve))
}

# A grid places each panel once, across its tab groups and its rails, so a
# grid of the dock's state that places one twice is refused, naming the
# view, and each such panel.
check_placed_once <- function(grids) {

  twice <- lapply(grids, panels_placed_twice)
  hit <- lengths(twice) > 0L

  abort_each(
    paste0(
      "The grid of the view ", names(grids)[hit], " places ",
      chr_ply(twice[hit], paste_enum, quotes = ""), " more than once.",
      recycle0 = TRUE
    ),
    class = "dock_grid_panel_duplicated"
  )
}

# A grid names a plugin by the id of its class, which has the form
# `{package}-{id}`, so a grid of the dock's state that names one by an id
# without a package is refused, naming the view, and each such id.
# An id of a plugin class goes unchecked, since `serve()` may add the class.
check_plugin_ids <- function(grids) {

  lost <- lapply(
    grids,
    function(grid) {
      ids <- unlst(lst_xtr(grid_panels(grid), "plugin"))
      unique(ids[!grepl("-", ids, fixed = TRUE)])
    }
  )

  lost <- lost[lengths(lost) > 0L]
  many <- lengths(lost) > 1L

  abort_each(
    paste0(
      "The grid of the view ", names(lost), " names the plugin",
      ifelse(many, "s ", " "), chr_ply(lost, paste_enum, quotes = ""),
      ifelse(
        many,
        ", which are no ids of plugin classes.",
        ", which is no id of a plugin class."
      ),
      recycle0 = TRUE
    ),
    class = "dock_plugin_id_invalid"
  )
}

# Refuses what `lines` describe, sentences that each name a fault, with one
# error that holds them all, where there are any.
# The sentences go into the message through glue's braces, so that a brace in
# an id that they name is not read as glue.
abort_each <- function(lines, class) {

  if (!length(lines)) {
    return(invisible())
  }

  blockr_abort("{paste(lines, collapse = ' ')}", class = class)
}
