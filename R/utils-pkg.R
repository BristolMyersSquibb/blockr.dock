#' @importFrom glue glue
#' @import blockr.core shiny bslib
NULL

pkg_file <- function(..., pkg = parent.frame()) {
  system.file(..., package = pkg_name(pkg))
}
