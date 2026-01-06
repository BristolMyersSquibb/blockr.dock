#' Navbar provider
#'
#' A navbar provider allows packages to contribute content to the navbar
#' without creating a dock panel. Unlike dock extensions which provide UI
#' in a dock panel, navbar providers only contribute left/right slot content.
#'
#' @param id Unique identifier for the provider
#' @param navbar_left_ui Function to generate left slot UI (takes namespace ID)
#' @param navbar_left_server Function for left slot server logic
#' @param navbar_right_ui Function to generate right slot UI (takes namespace ID)
#' @param navbar_right_server Function for right slot server logic
#' @param class Optional subclass
#' @param x Object to test or validate
#'
#' @return A `navbar_provider` object
#'
#' @examples
#' \dontrun{
#' provider <- new_navbar_provider(
#'   id = "my_provider",
#'   navbar_left_ui = function(id) {
#'     shiny::actionButton(shiny::NS(id, "btn"), "Click")
#'   }
#' )
#' }
#'
#' @name navbar_provider
#' @export
new_navbar_provider <- function(id,
                                navbar_left_ui = NULL,
                                navbar_left_server = NULL,
                                navbar_right_ui = NULL,
                                navbar_right_server = NULL,
                                class = character()) {

  stopifnot(
    is.character(id), length(id) == 1L,
    is.null(navbar_left_ui) || is.function(navbar_left_ui),
    is.null(navbar_left_server) || is.function(navbar_left_server),
    is.null(navbar_right_ui) || is.function(navbar_right_ui),
    is.null(navbar_right_server) || is.function(navbar_right_server)
  )

  structure(
    list(),
    id = id,
    navbar_left_ui = navbar_left_ui,
    navbar_left_server = navbar_left_server,
    navbar_right_ui = navbar_right_ui,
    navbar_right_server = navbar_right_server,
    class = c(class, "navbar_provider")
  )
}

#' @rdname navbar_provider
#' @export
is_navbar_provider <- function(x) {
  inherits(x, "navbar_provider")
}

#' @rdname navbar_provider
#' @export
navbar_provider_id <- function(x) {
  stopifnot(is_navbar_provider(x))
  attr(x, "id")
}

navbar_provider_left_ui <- function(x) {
  stopifnot(is_navbar_provider(x))
  attr(x, "navbar_left_ui")
}

navbar_provider_left_server <- function(x) {
  stopifnot(is_navbar_provider(x))
  attr(x, "navbar_left_server")
}

navbar_provider_right_ui <- function(x) {
  stopifnot(is_navbar_provider(x))
  attr(x, "navbar_right_ui")
}

navbar_provider_right_server <- function(x) {
  stopifnot(is_navbar_provider(x))
  attr(x, "navbar_right_server")
}
