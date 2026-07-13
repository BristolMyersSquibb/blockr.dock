sidebar_ui <- function(id, ui = NULL, title = NULL,
                       side = c("right", "left"), width = "420px",
                       mode = c("overlay", "push")) {
  stopifnot(
    is.character(id), length(id) == 1L, nzchar(id),
    is.character(width), length(width) == 1L, nzchar(width),
    is.null(title) || (is.character(title) && length(title) == 1L)
  )
  side <- match.arg(side)
  mode <- match.arg(mode)

  # Body slot: empty by default ("modal-like" use; populate later via
  # show_sidebar()) or pre-rendered from `ui` ("offcanvas-like" use; the
  # panel is ready to open with no server round-trip via the data-attribute
  # trigger or `show_sidebar(id)` with no `ui`).
  body_children <- list()
  body_deps <- list()
  if (!is.null(ui)) {
    rendered <- htmltools::renderTags(ui)
    body_children <- list(htmltools::HTML(rendered$html))
    body_deps <- rendered$dependencies
  }

  panel <- tags$div(
    id = id,
    class = "blockr-sidebar",
    `data-side` = side,
    `data-mode` = mode,
    `aria-hidden` = "true",
    tabindex = "-1",
    style = paste0("--blockr-sidebar-panel-width: ", width, ";"),
    tags$header(
      class = "blockr-sidebar-header",
      tags$h2(class = "blockr-sidebar-title", title),
      tags$div(
        class = "blockr-sidebar-actions",
        tags$button(
          type = "button",
          class = "blockr-sidebar-btn blockr-sidebar-pin",
          `aria-label` = "Pin sidebar",
          `aria-pressed` = "false",
          title = "Pin",
          pin_icon()
        ),
        tags$button(
          type = "button",
          class = "blockr-sidebar-btn blockr-sidebar-close",
          `aria-label` = "Close sidebar",
          title = "Close",
          "\u00d7"
        )
      )
    ),
    do.call(
      tags$div,
      c(
        list(
          class = "blockr-sidebar-body",
          role = "region",
          `aria-live` = "polite"
        ),
        body_children
      )
    )
  )

  htmltools::attachDependencies(panel, c(list(sidebar_dep()), body_deps))
}

sidebar_dep <- function() {
  htmltools::htmlDependency(
    name = "sidebar-server",
    version = utils::packageVersion("blockr.dock"),
    package = "blockr.dock",
    src = "assets",
    stylesheet = "css/sidebar-server.css",
    script = "js/sidebar-server.js",
    all_files = FALSE
  )
}

show_sidebar <- function(id, ui = NULL, title = NULL,
                         session = getDefaultReactiveDomain()) {
  stopifnot(
    is.character(id), length(id) == 1L, nzchar(id),
    is.null(title) || (is.character(title) && length(title) == 1L)
  )
  root <- root_session(session)

  # Two modes:
  #   * `ui` non-NULL: full content-swap (modal-like). Pre-render via
  #     renderTags and ship html + deps.
  #   * `ui = NULL`: open-only. The body was either pre-rendered at
  #     `sidebar_ui(ui = ...)` time or left in place by an earlier show.
  #     Skip the html / deps fields so the JS skips the swap sequence.
  payload <- list(action = "show")
  if (!is.null(ui)) {
    rendered <- htmltools::renderTags(ui)
    payload$html <- rendered$html
    payload$dependencies <- lapply(
      htmltools::resolveDependencies(rendered$dependencies),
      createWebDependency
    )
  }
  if (!is.null(title)) {
    payload$title <- title
  }
  root$sendInputMessage(id, payload)
  invisible(NULL)
}

hide_sidebar <- function(id, session = getDefaultReactiveDomain()) {
  stopifnot(is.character(id), length(id) == 1L, nzchar(id))
  root <- root_session(session)

  root$sendInputMessage(id, list(action = "hide"))
  invisible(NULL)
}

sidebar_state <- function(id, session = getDefaultReactiveDomain()) {
  stopifnot(is.character(id), length(id) == 1L, nzchar(id))
  root <- root_session(session)

  # Snapshot read: callers (typically `observeEvent` confirm handlers)
  # want the current value without creating a reactive dependency on it.
  state <- isolate(root$input[[id]])
  if (is.null(state)) {
    list(open = FALSE, pinned = FALSE)
  } else {
    state
  }
}

keep_or_hide_sidebar <- function(id, ui, title = NULL,
                                 session = getDefaultReactiveDomain()) {
  if (isTRUE(sidebar_state(id, session = session)$pinned)) {
    show_sidebar(id, ui = ui, title = title, session = session)
  } else {
    hide_sidebar(id, session = session)
  }
}

# Inline SVG pushpin (Bootstrap-icons "pin"). Swapped in for an earlier
# `▣` ("WHITE SQUARE WITH ROUNDED CORNERS") that no font rendered as
# a recognisable pin. SVG keeps the icon legible across platforms with
# no extra dep, and `fill="currentColor"` lets the existing pinned-state
# colour rule (`.blockr-sidebar-pinned .blockr-sidebar-pin`) drive it.
pin_icon <- function() {
  tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    viewBox = "0 0 16 16",
    fill = "currentColor",
    `aria-hidden` = "true",
    tags$path(
      d = paste0(
        "M9.828.722a.5.5 0 0 1 .354.146l4.95 4.95a.5.5 0 0 1 0 .707",
        "c-.48.48-1.072.588-1.503.588-.177 0-.335-.018-.46-.039",
        "l-3.134 3.134a5.927 5.927 0 0 1 .16 1.013c.046.702-.032 ",
        "1.687-.72 2.375a.5.5 0 0 1-.707 0l-2.829-2.828-3.182 ",
        "3.182c-.195.195-1.219.902-1.414.707-.195-.195.512-1.22",
        ".707-1.414l3.182-3.182-2.828-2.829a.5.5 0 0 1 0-.707",
        "c.688-.688 1.673-.767 2.375-.72a5.922 5.922 0 0 1 1.013",
        ".16l3.134-3.133a2.772 2.772 0 0 1-.04-.461c0-.43.108-",
        "1.022.589-1.503a.5.5 0 0 1 .353-.146z"
      )
    )
  )
}

# Walk to the root session so `sendInputMessage(id, ...)` targets the
# absolute DOM id, not the calling module's namespaced id. The panel
# id passed to `sidebar_ui()` is a free-form DOM id chosen by the
# caller and is what `show_sidebar()` / `hide_sidebar()` target.
root_session <- function(session) {
  if (is.null(session)) {
    blockr_abort(
      paste(
        "`show_sidebar()` / `hide_sidebar()` must be called from",
        "within a Shiny session."
      ),
      class = "blockr_dock_no_session"
    )
  }
  session$rootScope()
}

# A usable candidate id: a non-empty string not already in `existing`.
id_available <- function(id, existing) {
  is_string(id) && nzchar(id) && !(id %in% existing)
}

# Uniform reader for a menu-server argument: a reactive is returned as-is,
# a plain value / `NULL` is wrapped so it reads the same way via `x()`.
as_accessor <- function(x) {
  if (is.reactive(x)) {
    return(x)
  }
  function() x
}
