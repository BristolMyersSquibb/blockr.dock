#' Compact block headers
#'
#' A board option that switches every block header to its compact form: an
#' eyebrow line over the block's content, with the block's mark as a small
#' tinted square and the name in small capitals. Off by default, which keeps
#' the regular header (a 32px mark and a 16px title that may wrap to two
#' lines). It sits with the light/dark switch under "Theme options".
#'
#' A board that holds no value for it takes its default as it opens:
#' the blockr option `compact`, as `options(blockr.compact = TRUE)` or the
#' environment variable `BLOCKR_COMPACT` sets it, or off where neither does
#' (see [blockr.core::blockr_option()]).
#' The client defines the board option, with its label, category, schema, UI
#' and default, in the module `js/compact-option.js`, which the package lists
#' in its `._BLOCKR_MODULES_` (see [new_dock_plugin()]).
#'
#' @param value Logical, whether headers start compact.
#'
#' @return A `board_option` object.
#'
#' @examples
#' new_compact_option(TRUE)
#'
#' @export
new_compact_option <- function(value) {
  new_board_option("compact", value)
}

#' Views as tabs
#'
#' A board option that shows the board's views as a line of tabs under the
#' navbar, one tab per view, with the view menu reduced to its chevron at the
#' end of the line. Off by default, it sits with the light/dark switch and
#' "Compact" under "Theme options", and "Show views as tabs" at the foot of the
#' view menu sets it as well. A board saved with the option on shows the tabs
#' from its first paint.
#'
#' A board that holds no value for it takes its default as it opens:
#' the blockr option `view_tabs`, as `options(blockr.view_tabs = TRUE)` or the
#' environment variable `BLOCKR_VIEW_TABS` sets it, or off where neither does
#' (see [blockr.core::blockr_option()]).
#' The client defines the board option in the module `js/view-tabs-option.js`.
#'
#' @param value Logical, whether the views show as tabs.
#'
#' @return A `board_option` object.
#'
#' @examples
#' new_view_tabs_option(TRUE)
#'
#' @export
new_view_tabs_option <- function(value) {
  new_board_option("view_tabs", value)
}

#' Rails in step across views
#'
#' A board option that opens and closes the rails of every view together.
#' Collapsing or expanding the rail on one edge of a view does the same to the
#' rail on that edge of every other view, and a view visited for the first
#' time opens with its rails the way the views already visited show theirs.
#' Only a rail holding panels takes part: an empty one is not shown. On by
#' default; switched off, each view keeps its rails as they were left there.
#'
#' A board that holds no value for it takes its default as it opens:
#' the blockr option `sync_rails`, as `options(blockr.sync_rails = FALSE)` or
#' the environment variable `BLOCKR_SYNC_RAILS` sets it, or on where neither
#' does (see [blockr.core::blockr_option()]).
#' The client defines the board option in the module `js/sync-rails-option.js`.
#'
#' @param value Logical, whether the rails are kept in step.
#'
#' @return A `board_option` object.
#'
#' @examples
#' new_sync_rails_option(FALSE)
#'
#' @export
new_sync_rails_option <- function(value) {
  new_board_option("sync_rails", value)
}
