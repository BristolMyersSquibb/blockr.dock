#' Views and folders
#'
#' The dock shows one view of the board at a time, a grid of panels, and its
#' views menu lists every view, grouped in folders (see [new_dock_board()]).
#' The function `dock_view()` writes a view, and `dock_folder()` a folder.
#'
#' A view takes its grid, as [dock_grid()][layout] writes it, which says which
#' panels the view shows, and where.
#' In place of a grid, it may take the panels that it shows, as bare ids,
#' panel references (see [blk()]), or a list of them.
#' R then gives the view the default grid over those panels: the plugins'
#' panels in one tab group, beside the blocks' panels in another.
#' A view without a name is named after its id, as "Adverse events" for
#' `adverse_events`.
#'
#' A folder groups views and other folders in the views menu, at any depth,
#' and does not change what a view shows.
#' A view or folder sits in the folder whose id it names in `folder`, which is
#' the folder's name in the `folders` of [new_dock_board()], and at the top
#' where it names none.
#'
#' The function `view_name()` reads the name of a view, such as one that
#' [board_views()] reads from a board.
#'
#' @param grid The view's grid, as [dock_grid()][layout] writes it, or the
#'   panels that its default grid places.
#' @param name The name of the view or folder, which the user sees.
#'   It is a string that holds more than spaces, and a view without one is
#'   named after its id.
#' @param folder The id of the folder that the view or folder sits in, or
#'   `NULL` for the top.
#' @param x A view, as `dock_view()` writes it.
#'
#' @return The function `dock_view()` returns a `dock_view`, `dock_folder()` a
#'   `dock_folder`, and `view_name()` the name of the view, a string, or
#'   `NULL` for a view without one.
#'
#' @examples
#' dock_view(dock_grid("a", "b"), name = "Overview")
#' dock_view(c("a", "b"), folder = "safety")
#' dock_folder("Safety")
#'
#' view_name(dock_view("a", name = "Overview"))
#'
#' @rdname view
#' @export
dock_view <- function(grid, name = NULL, folder = NULL) {

  check_nav_name(name, allow_null = TRUE)
  check_nav_folder(folder)

  if (is_dock_grid(grid)) {
    return(new_dock_view(grid = grid, name = name, folder = folder))
  }

  new_dock_view(panels = as_panel_refs(grid), name = name, folder = folder)
}

# A view holds its grid, or in its place the panels of its default grid, and
# its name and folder, each of which may be `NULL`.
new_dock_view <- function(grid = NULL, panels = NULL, name = NULL,
                          folder = NULL) {
  structure(
    list(grid = grid, panels = panels, name = name, folder = folder),
    class = "dock_view"
  )
}

#' @rdname view
#' @export
dock_folder <- function(name, folder = NULL) {

  check_nav_name(name)
  check_nav_folder(folder)

  structure(list(name = name, folder = folder), class = "dock_folder")
}

#' @rdname view
#' @export
view_name <- function(x) {

  if (!is_dock_view(x)) {
    blockr_abort(
      "Expecting a view, as `dock_view()` writes it.",
      class = "dock_view_invalid"
    )
  }

  x[["name"]]
}

is_dock_view <- function(x) {
  inherits(x, "dock_view")
}

is_dock_folder <- function(x) {
  inherits(x, "dock_folder")
}

# The name of a view or folder is a string that holds more than spaces, as
# the commands that add or rename one have it.
check_nav_name <- function(name, allow_null = FALSE) {

  if (allow_null && is.null(name)) {
    return(invisible(name))
  }

  if (!is_string(name) || is.na(name) || !nzchar(trimws(name))) {
    blockr_abort(
      "The name of a view or folder is a string that holds more than spaces.",
      class = "dock_view_name_invalid"
    )
  }

  invisible(name)
}

# A view or folder names the folder that it sits in by the folder's id, or
# names none at the top.
check_nav_folder <- function(folder) {

  if (is.null(folder)) {
    return(invisible(folder))
  }

  if (!is_string(folder) || is.na(folder) || !nzchar(folder)) {
    blockr_abort(
      "A view or folder names the folder that it sits in by the folder's id, ",
      "a string that is not empty, or names none at the top.",
      class = "dock_folder_invalid"
    )
  }

  invisible(folder)
}

#' @export
format.dock_view <- function(x, ...) {

  header <- paste0(
    "<dock_view>",
    if (not_null(x[["name"]])) paste0(" ", x[["name"]]),
    if (not_null(x[["folder"]])) paste0(" (in ", x[["folder"]], ")")
  )

  if (not_null(x[["grid"]])) {
    return(c(header, format(x[["grid"]])))
  }

  ids <- chr_xtr(x[["panels"]], "id")

  c(header, paste0("panels: ", if (length(ids)) paste(ids, collapse = ", ")))
}

#' @export
print.dock_view <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
format.dock_folder <- function(x, ...) {
  paste0(
    "<dock_folder> ",
    x[["name"]],
    if (not_null(x[["folder"]])) paste0(" (in ", x[["folder"]], ")")
  )
}

#' @export
print.dock_folder <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

# The keys that fractional-indexing's `generateNKeysBetween(null, null, n)`
# gives for `n` items in a row: "a0" to "a9", "aA" to "aZ" and "aa" to "az",
# then "b00" to "bzz", then "c000" and on.
# Each key is a whole number in base 62, after a letter that gives its number
# of digits, "a" for one, "b" for two.
# The dock orders the views and folders of a folder by these keys, which it
# compares as strings.
index_keys <- function(n) {

  digits <- c(0:9, LETTERS, letters)

  key <- function(i) {

    width <- 1L

    while (i >= 62^width) {
      i <- i - 62^width
      width <- width + 1L
    }

    places <- (i %/% 62^seq(width - 1L, 0L)) %% 62

    paste0(letters[width], paste(digits[places + 1L], collapse = ""))
  }

  chr_ply(seq_len(n) - 1, key)
}

# The `index` of each view and of each folder, which orders it among the views
# and folders of the folder that holds it, as a list of the two.
# In each folder, and at the top, the views take the first keys, in the order
# of the script, and the folders take the keys after them.
# The holder of each view and folder is the id of a folder, or "" for the top.
nav_index <- function(view_holders, folder_holders) {

  holders <- c(view_holders, folder_holders)
  index <- character(length(holders))

  split(index, holders) <- lapply(
    split(index, holders),
    function(keys) index_keys(length(keys))
  )

  list(
    views = index[seq_along(view_holders)],
    folders = index[length(view_holders) + seq_along(folder_holders)]
  )
}

# The id of the first view of the navigation, which lists the views of a
# folder where the folder sits, or `NULL` where there is no view.
# Since the views of each folder come before its folders, that view is the
# first view at the top, in the order of the script, or else the first view
# in the first folder that holds one.
first_nav_view <- function(view_holders, folder_holders) {

  first <- function(at) {

    views <- names(view_holders)[view_holders == at]

    if (length(views)) {
      return(views[[1L]])
    }

    for (folder in names(folder_holders)[folder_holders == at]) {

      view <- first(folder)

      if (not_null(view)) {
        return(view)
      }
    }

    NULL
  }

  first("")
}

# The folders that sit in themselves, through the folders that hold them.
# Each folder's holder is the id of a folder that the state holds, or "" for
# the top.
folders_in_themselves <- function(folder_holders) {

  in_itself <- function(id) {

    passed <- character()
    at <- folder_holders[[id]]

    while (nzchar(at) && !at %in% passed) {

      if (identical(at, id)) {
        return(TRUE)
      }

      passed <- c(passed, at)
      at <- folder_holders[[at]]
    }

    FALSE
  }

  ids <- names(folder_holders)

  ids[lgl_ply(ids, in_itself)]
}
