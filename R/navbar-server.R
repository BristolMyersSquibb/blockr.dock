#' Navbar server
#'
#' Initializes server logic for navbar, including all navbar_provider servers.
#'
#' @param id Namespace ID
#' @param board Reactive values object containing board state
#' @param dock Dock object (optional, for serialization)
#' @param session Shiny session
#'
#' @keywords internal
navbar_server <- function(id, board, dock = NULL,
                          session = shiny::getDefaultReactiveDomain()) {
  moduleServer(id, function(input, output, session) {
    # Initialize navbar provider servers
    init_navbar_servers(board, dock, session)

    # Handle code button - trigger the generate_code plugin's button
    observeEvent(input$code_btn, {
      # Get the board's namespace (parent of navbar)
      board_ns <- sub("-navbar-?$", "", session$ns(""))
      # Trigger the generate_code plugin's button
      code_btn_id <- paste0(board_ns, "-generate_code-code_mod")
      session$sendCustomMessage(
        "blockr-trigger-click",
        list(id = code_btn_id)
      )
    })
  })
}

#' Initialize navbar provider servers
#'
#' Calls server functions from all navbar_provider objects.
#'
#' @param board Reactive values object containing board state
#' @param dock Dock object (optional)
#' @param session Shiny session
#'
#' @keywords internal
init_navbar_servers <- function(board, dock = NULL,
                                session = shiny::getDefaultReactiveDomain()) {
  providers <- dock_navbar_providers(isolate(board$board))

  for (p in providers) {
    provider_id <- navbar_provider_id(p)

    # Left server
    left_server <- navbar_provider_left_server(p)
    if (!is.null(left_server)) {
      left_server(provider_id, board, dock = dock, session = session)
    }

    # Right server
    right_server <- navbar_provider_right_server(p)
    if (!is.null(right_server)) {
      right_server(provider_id, board, dock = dock, session = session)
    }
  }
}
