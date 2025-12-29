#' Mobile breakpoint for switching between dock and mobile layouts
#' @noRd
MOBILE_BREAKPOINT <- 900

#' @export
board_ui.dock_board <- function(id, x, plugins = board_plugins(x),
                                options = board_options(x), ...) {

  opt_ui_or_null <- function(plg, plgs, x) {
    if (plg %in% names(plgs)) board_ui(id, plgs[[plg]], x)
  }

  stopifnot(is_string(id))

  ns <- NS(id)

  tagList(
    show_hide_block_dep(),
    blockr_dock_dep(),
    # Early viewport detection - sends width before Shiny fully loads
    viewport_detection_script(ns),
    off_canvas(
      id = NS(id, "blocks_offcanvas"),
      title = "Offcanvas blocks",
      block_ui(id, x, plugins[["edit_block"]])
    ),
    options_ui(
      id,
      options,
      div(
        id = "preserve_board",
        class = "mb-1",
        opt_ui_or_null("preserve_board", plugins, x)
      ),
      div(
        id = "generate_code",
        opt_ui_or_null("generate_code", plugins, x)
      )
    ),
    # Desktop: Dockview layout (hidden on mobile via CSS)
    tags$div(
      id = ns("board_view"),
      class = "blockr-board-view blockr-desktop-only",
      style = "position: relative;",
      dockViewR::dock_view_output(
        NS(id, dock_id()),
        width = "100%",
        height = "calc(100vh - 48px)"
      )
    ),
    # Mobile: Simple tabbed layout (hidden on desktop via CSS)
    tags$div(
      id = ns("mobile_view"),
      class = "blockr-mobile-view blockr-mobile-only",
      mobile_board_ui(ns)
    ),
    # Slide-out block panel (shared between desktop and mobile)
    block_panel_ui(ns),
    # Workflow overview page (hidden by default using position/visibility)
    tags$div(
      id = ns("workflows_view"),
      class = "blockr-workflows-view blockr-view-hidden",
      workflow_overview_ui(ns)
    ),
    off_canvas(
      id = NS(id, "exts_offcanvas"),
      position = "bottom",
      title = "Offcanvas extensions",
      lapply(
        dock_extensions(x),
        extension_ui,
        id = id,
        board = x
      )
    ),
    # JavaScript for view switching (handles both desktop and mobile views)
    tags$script(HTML(sprintf("
      Shiny.addCustomMessageHandler('blockr-switch-view', function(view) {
        var boardView = document.getElementById('%s');
        var mobileView = document.getElementById('%s');
        var workflowsView = document.getElementById('%s');
        if (view === 'workflows') {
          if (boardView) boardView.classList.add('blockr-view-hidden');
          if (mobileView) mobileView.classList.add('blockr-view-hidden');
          if (workflowsView) workflowsView.classList.remove('blockr-view-hidden');
          // Trigger resize for reactable
          setTimeout(function() {
            window.dispatchEvent(new Event('resize'));
          }, 50);
        } else {
          if (boardView) boardView.classList.remove('blockr-view-hidden');
          if (mobileView) mobileView.classList.remove('blockr-view-hidden');
          if (workflowsView) workflowsView.classList.add('blockr-view-hidden');
        }
      });
    ", ns("board_view"), ns("mobile_view"), ns("workflows_view"))))
  )
}

#' Viewport detection script
#'
#' Sends viewport width to Shiny as early as possible
#' @noRd
viewport_detection_script <- function(ns) {
  tags$script(HTML(sprintf("
    (function() {
      function sendViewport() {
        if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
          Shiny.setInputValue('%s', window.innerWidth, {priority: 'event'});
        }
      }
      // Send immediately when Shiny connects
      $(document).on('shiny:connected', sendViewport);
      // Also send on resize
      var resizeTimer;
      window.addEventListener('resize', function() {
        clearTimeout(resizeTimer);
        resizeTimer = setTimeout(sendViewport, 150);
      });
    })();
  ", ns("viewport_width"))))
}

#' Mobile board UI
#'
#' Card stack layout for mobile devices - each panel becomes a card
#' @noRd
mobile_board_ui <- function(ns) {
  tagList(
    # Scrollable card stack container
    tags$div(
      id = ns("mobile_card_stack"),
      class = "mobile-card-stack"
      # Cards will be rendered dynamically from blocks and extensions
    ),
    # Add block button (floating)
    tags$button(
      id = ns("mobile_add_block"),
      class = "mobile-add-btn",
      type = "button",
      tags$span(class = "mobile-add-icon", "+")
    )
  )
}

options_ui <- function(id, x, ...) {

  stopifnot(is_board_options(x))

  opts <- split(x, chr_ply(x, attr, "category"))

  offcanvas_id <- NS(id, "options_offcanvas")

  tagList(
    # Edge tab button removed - navbar gear icon triggers this offcanvas instead
    off_canvas(
      id = offcanvas_id,
      position = "end",
      title = "Board options",
      ...,
      hr(),
      do.call(
        accordion,
        c(
          list(
            id = NS(id, "board_options"),
            multiple = TRUE,
            open = FALSE,
            class = "accordion-flush"
          ),
          map(
            do.call,
            rep(list(accordion_panel), length(opts)),
            map(
              list,
              title = names(opts),
              lapply(opts, lapply, board_option_ui, id)
            )
          )
        )
      )
    )
  )
}

blockr_dock_dep <- function() {
  htmltools::htmlDependency(
    "blockr-dock",
    pkg_version(),
    src = pkg_file("assets"),
    stylesheet = "css/blockr-dock.css",
    script = "js/blockr-components.js"
  )
}
