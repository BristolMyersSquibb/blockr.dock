# =============================================================================
# Sidebar API
# =============================================================================
#
#' Sidebar API
#'
#' @description
#' A slide-out sidebar system for blockr.dock. The API mirrors Shiny's
#' modal API:
#'
#' | Shiny Modal | Sidebar |
#' |-------------|---------|
#' | `showModal(modalDialog(...))` | `show_sidebar(new_sidebar(...))` |
#' | `removeModal()` | `hide_sidebar()` |
#'
#' @section Functions:
#'
#' - [init_sidebar()] - Initialize the sidebar system (call once in server)
#' - [show_sidebar()] - Show a sidebar (like [shiny::showModal()])
#' - [hide_sidebar()] - Hide the current sidebar (like [shiny::removeModal()])
#' - [new_sidebar()] - Create a sidebar object (like [shiny::modalDialog()])
#' - [get_sidebar()] - Get current sidebar state
#'
#' @section Usage:
#' ```r
#' # In server (once):
#' init_sidebar(session)
#'
#' # Show a sidebar:
#' show_sidebar(new_sidebar("add_block", list(mode = "add")))
#'
#' # Or with custom ID and context:
#' show_sidebar("my_sidebar", context = list(data = myData))
#'
#' # Hide:
#' hide_sidebar()
#'
#' # Get state (for conditional rendering):
#' sidebar <- get_sidebar()
#' if (identical(sidebar$sidebar_id, "my_sidebar")) {
#'   # render content...
#' }
#' ```
#'
#' @section How It Works:
#' 1. `init_sidebar()` creates reactive state in `session$userData`
#' 2. `show_sidebar()` sets the `sidebar_id` to show
#' 3. An observer toggles CSS classes via shinyjs to show/hide
#' 4. UI containers are always in the DOM, just hidden via CSS transform
#'
#' @param session Shiny session (default: current reactive domain)
#'
#' @name sidebar
#' @aliases sidebar-api
NULL


#' @rdname sidebar
#' @export
init_sidebar <- function(session = shiny::getDefaultReactiveDomain()) {

  ns <- session$ns

  # Create sidebar state
  sidebar <- reactiveValues(
    sidebar_id = NULL,
    context = list(),
    prev_sidebar_id = NULL
  )

  # Store in session for access by show_sidebar/hide_sidebar
  session$userData$blockr_sidebar <- sidebar

  # Add keyboard shortcuts for sidebar
  # nolint start: line_length_linter.

  # Escape to close sidebar
  shinyjs::runjs(sprintf(
    "document.addEventListener('keydown', function(e) {
      if (e.key === 'Escape') {
        var openSidebar = document.querySelector('.blockr-sidebar:not(.blockr-sidebar-hidden)');
        if (openSidebar) {
          Shiny.setInputValue('%s', Date.now(), {priority: 'event'});
        }
      }
    });",
    ns("close_sidebar")
  ))

  # Arrow key navigation and Enter to select in search box
  shinyjs::runjs(
    "document.addEventListener('keydown', function(e) {
      if (!e.target.classList.contains('blockr-sidebar-search-input')) return;

      var sidebar = e.target.closest('.blockr-sidebar');
      if (!sidebar) return;

      var cards = Array.from(sidebar.querySelectorAll('.blockr-block-card:not([style*=\"display: none\"])'));
      if (cards.length === 0) return;

      var selected = sidebar.querySelector('.blockr-block-card.blockr-card-selected');
      var selectedIndex = selected ? cards.indexOf(selected) : -1;

      if (e.key === 'ArrowDown') {
        e.preventDefault();
        if (selected) selected.classList.remove('blockr-card-selected');
        var nextIndex = selectedIndex < cards.length - 1 ? selectedIndex + 1 : 0;
        cards[nextIndex].classList.add('blockr-card-selected');
        cards[nextIndex].scrollIntoView({ block: 'nearest' });
      } else if (e.key === 'ArrowUp') {
        e.preventDefault();
        if (selected) selected.classList.remove('blockr-card-selected');
        var prevIndex = selectedIndex > 0 ? selectedIndex - 1 : cards.length - 1;
        cards[prevIndex].classList.add('blockr-card-selected');
        cards[prevIndex].scrollIntoView({ block: 'nearest' });
      } else if (e.key === 'Enter') {
        e.preventDefault();
        var card = selected || cards[0];
        if (card) {
          // Stack cards have onclick on card itself, block cards have it on .blockr-block-card-main
          var main = card.querySelector('.blockr-block-card-main');
          if (main) {
            main.click();
          } else {
            card.click();
          }
        }
      }
    });

    // Track keyboard-selected block ID to preserve across re-renders
    var selectedBlockId = null;

    // Watch for content changes and restore/auto-select card
    var observer = new MutationObserver(function(mutations) {
      mutations.forEach(function(mutation) {
        var sidebar = mutation.target.closest('.blockr-sidebar');
        if (sidebar && !sidebar.classList.contains('blockr-sidebar-hidden')) {
          // Try to restore previous selection by block ID
          if (selectedBlockId) {
            var card = sidebar.querySelector(
              '.blockr-block-card[data-block-id=\"' + selectedBlockId + '\"]'
            );
            if (card && !card.style.display.includes('none')) {
              card.classList.add('blockr-card-selected');
              return;
            }
          }

          // Only auto-select first if no card is selected
          var existing = sidebar.querySelector('.blockr-card-selected');
          if (!existing) {
            var firstCard = sidebar.querySelector(
              '.blockr-block-card:not([style*=\"display: none\"])'
            );
            if (firstCard) {
              firstCard.classList.add('blockr-card-selected');
              selectedBlockId = firstCard.dataset.blockId || null;
            }
          }
        }
      });
    });

    // Update selectedBlockId when arrow keys change selection
    document.addEventListener('keydown', function(e) {
      if (e.key === 'ArrowDown' || e.key === 'ArrowUp') {
        setTimeout(function() {
          var sel = document.querySelector('.blockr-card-selected');
          selectedBlockId = sel ? (sel.dataset.blockId || null) : null;
        }, 0);
      }
    });

    // Clear selection tracking when sidebar closes
    document.addEventListener('keydown', function(e) {
      if (e.key === 'Escape') selectedBlockId = null;
    });

    // Observe all sidebar content areas
    document.querySelectorAll('.blockr-sidebar-content').forEach(function(content) {
      observer.observe(content, { childList: true, subtree: true });
    });

    // Also observe for new sidebars being added
    new MutationObserver(function(mutations) {
      mutations.forEach(function(mutation) {
        mutation.addedNodes.forEach(function(node) {
          if (node.nodeType === 1) {
            var contents = node.querySelectorAll ? node.querySelectorAll('.blockr-sidebar-content') : [];
            contents.forEach(function(content) {
              observer.observe(content, { childList: true, subtree: true });
            });
          }
        });
      });
    }).observe(document.body, { childList: true, subtree: true });"
  )

  # Observer: react to sidebar_id changes and update UI via shinyjs
  observeEvent(sidebar$sidebar_id, {
    prev_id <- isolate(sidebar$prev_sidebar_id)
    new_id <- sidebar$sidebar_id

    # Normalize empty values
    if (length(prev_id) == 0 || identical(prev_id, "")) prev_id <- NULL
    if (length(new_id) == 0 || identical(new_id, "")) new_id <- NULL

    # Hide previous sidebar
    if (!is.null(prev_id) && nzchar(prev_id)) {
      prev_el_id <- ns(paste0(prev_id, "_sidebar"))
      shinyjs::runjs(sprintf(
        "var el = document.getElementById('%s'); if(el) el.classList.add('blockr-sidebar-hidden');",
        prev_el_id
      ))
      shinyjs::runjs(sprintf(
        "var el = document.getElementById('%s'); if(el) el.value = '';",
        ns(paste0(prev_id, "_search"))
      ))
      shiny::updateTextInput(session, paste0(prev_id, "_search"), value = "")
      shinyjs::runjs(sprintf(
        "document.querySelectorAll('#%s .blockr-block-card.is-open').forEach(function(c) { c.classList.remove('is-open'); });",
        prev_el_id
      ))
    }

    # Show new sidebar
    if (!is.null(new_id) && nzchar(new_id)) {
      sidebar_el_id <- ns(paste0(new_id, "_sidebar"))
      shinyjs::runjs(sprintf(
        "var el = document.getElementById('%s'); if(el) el.classList.remove('blockr-sidebar-hidden');",
        sidebar_el_id
      ))
      shinyjs::runjs(sprintf(
        "setTimeout(function() { var el = document.getElementById('%s'); if(el) el.focus(); }, 100);",
        ns(paste0(new_id, "_search"))
      ))
    }

    sidebar$prev_sidebar_id <- new_id
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  # nolint end.

  # Close sidebar handler (from close button)
  observeEvent(session$input$close_sidebar, {
    sidebar$sidebar_id <- NULL
    sidebar$context <- list()
  })

  invisible(sidebar)
}


#' @param id Either a string sidebar ID or a sidebar object from [new_sidebar()]
#' @param context List of context data (ignored if `id` is a sidebar object)
#' @rdname sidebar
#' @export
show_sidebar <- function(id, context = list(),
                         session = shiny::getDefaultReactiveDomain()) {

  sidebar <- session$userData$blockr_sidebar

  # Return silently if sidebar system not initialized
  # (allows actions to work in non-sidebar contexts or tests)
  if (is.null(sidebar)) {
    return(invisible())
  }

  # Handle sidebar objects (like modalDialog objects)
  if (inherits(id, "sidebar")) {
    context <- id$context
    id <- id$id
  }

  sidebar$context <- context
  sidebar$sidebar_id <- id

  invisible()
}


#' @rdname sidebar
#' @export
hide_sidebar <- function(session = shiny::getDefaultReactiveDomain()) {
  sidebar <- session$userData$blockr_sidebar
  if (is.null(sidebar)) return(invisible())

  sidebar$sidebar_id <- NULL
  sidebar$context <- list()

  invisible()
}


#' @rdname sidebar
#' @export
get_sidebar <- function(session = shiny::getDefaultReactiveDomain()) {
  session$userData$blockr_sidebar
}


#' @rdname sidebar
#' @export
new_sidebar <- function(id, context = list()) {
  structure(
    list(id = id, context = context),
    class = "sidebar"
  )
}
