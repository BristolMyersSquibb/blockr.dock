# get_download can transiently fail after a session reload: the download
# link's href is filled only once outputs bind, and the server download
# endpoint may briefly not answer ("Unable request data from server").
# Retry a few times with a short pause; a persistent failure re-raises.
retry_download <- function(app, output, .attempts = 6L) {
  for (i in seq_len(.attempts)) {
    res <- tryCatch(app$get_download(output), error = function(e) e)
    if (!inherits(res, "error")) {
      return(res)
    }
    Sys.sleep(0.5)
  }
  stop(res)
}

# Every e2e AppDriver goes through here so the chromote command timeout is
# hardened in one place. A loaded CI runner (Windows especially) can take longer
# than chromote's 10s default to bind the app's port, so AppDriver's first
# `Page.navigate` times out at init (rstudio/shinytest2#448). The chromote
# session inherits its command timeout from the default chromote object, so
# raise that before the session is spawned -- via `default_chromote_object()`,
# the same object AppDriver draws its session from.
new_app_driver <- function(...) {
  chrome <- chromote::default_chromote_object()
  chrome$default_timeout <- 60
  shinytest2::AppDriver$new(...)
}

# The dockview `_state` a grid serialises from carries a transient `focus`
# marker (which group holds UI focus) the client sets after render, independent
# of the authored geometry -- so it is present or absent depending on when the
# export samples the client, not on the layout the round-trip must preserve.
# Strip it (anywhere in the nested grid) before a byte-for-byte grid compare.
drop_focus <- function(x) {
  if (!is.list(x)) {
    return(x)
  }

  if (!is.null(names(x))) {
    x <- x[names(x) != "focus"]
  }

  lapply(x, drop_focus)
}

board_args <- function(...) {
  generate_plugin_args(
    new_dock_board(...),
    mode = "read"
  )[["board"]]
}

# Stand-in for the `visibility` channel blockr.core hands the board callback:
# three environments of per-block reactiveVals (`required`, `visible`,
# `frozen`), one slot per block, mirroring core's add_vis_slots at construction
# (which seeds every board block before the callback runs). The dock writes
# values into these slots; core owns their lifecycle in the real thing. Pass the
# block ids to seed, or a board handle to seed from its blocks.
fake_visibility <- function(x = character()) {
  ids <- if (is.character(x)) x else board_block_ids(shiny::isolate(x$board))

  vis <- list(
    required = new.env(parent = emptyenv()),
    visible = new.env(parent = emptyenv()),
    frozen = new.env(parent = emptyenv())
  )
  for (id in ids) {
    vis$required[[id]] <- shiny::reactiveVal(NA)
    vis$visible[[id]] <- shiny::reactiveVal(NA_character_)
    vis$frozen[[id]] <- shiny::reactiveVal(FALSE)
  }
  vis
}

# Resolve a view's stable id from its display label. Views are keyed by
# id internally; tests that know a view by its label use this to reach
# the id (labels are unique within the fixtures).
vid <- function(x, name) {
  if (is_dock_board(x)) {
    x <- board_views(x)
  }
  nms <- view_names(x)
  id <- names(nms)[match(name, nms)]
  if (is.na(id)) {
    stop("no view named ", name)
  }
  id
}

# Display label of the active view (derived from the id when unnamed),
# or NULL when none is active.
active_name <- function(x) {
  if (is_dock_board(x)) {
    x <- board_views(x)
  }
  av <- active_view(x)
  if (is.null(av)) {
    return(NULL)
  }
  unname(view_names(x)[av])
}

# Minimal externally controllable extension: its sole constructor input
# `content` is exposed as a `reactiveVal` in `state`, mirroring the author
# contract a real extension (e.g. blockr.md) implements. `external_ctrl` is
# set internally so it does not leak into the constructor formals (which
# define the controllable variables).
new_ctrl_extension <- function(content = "") {
  new_dock_extension(
    server = function(id, ...) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          list(state = list(content = shiny::reactiveVal(content)))
        }
      )
    },
    ui = function(id) shiny::tagList(),
    name = "Document",
    class = "doc_extension",
    external_ctrl = TRUE
  )
}

# Parse the live view-nav dropdown of a running app into one row per view
# (id, display label, active flag), so a browser test can state the nav
# contract directly: one entry per view, unique ids, no blank labels. Reads
# the rendered DOM (post static UI + any client-side `add`/`remove`), which
# is where the two layers compose -- the seam a unit test can't observe.
#
# `blockr-view-item` is a prefix of the child `-name` / `-actions` classes,
# so match the class token exactly (space-padded) rather than by substring.
read_view_nav <- function(app, board_id = "my_board") {
  by_class <- function(axis, token) {
    sprintf(
      "%s*[contains(concat(' ', normalize-space(@class), ' '), ' %s ')]",
      axis, token
    )
  }

  nav <- xml2::read_html(app$get_html(paste0("#", board_id, "-view_nav")))
  items <- xml2::xml_find_all(nav, by_class("//", "blockr-view-item"))
  name_nodes <- xml2::xml_find_first(
    items, by_class(".//", "blockr-view-item-name")
  )

  data.frame(
    id = xml2::xml_attr(items, "data-view-id"),
    label = trimws(xml2::xml_text(name_nodes)),
    active = grepl("(^| )active( |$)", xml2::xml_attr(items, "class")),
    stringsAsFactors = FALSE
  )
}

# Parse the live view-dock containers into one row per view (id, active flag),
# so a browser test can assert the rendered docks track a view-lifecycle
# change alongside the nav: switching moves the `blockr-view-dock-active`
# class, adding inserts a container, removing drops one. Each view's dock sits
# in `#<board>-view_handle-<id>`; the active one carries the extra class
# (toggled by the `switch-view` client handler). Reads the composed DOM -- the
# seam a unit test can't observe.
read_view_docks <- function(app, board_id = "my_board") {
  by_class <- function(token) {
    sprintf(
      "//*[contains(concat(' ', normalize-space(@class), ' '), ' %s ')]",
      token
    )
  }

  cont <- xml2::read_html(
    app$get_html(paste0("#", board_id, "-view_container"))
  )
  docks <- xml2::xml_find_all(cont, by_class("blockr-view-dock"))
  prefix <- paste0("^", board_id, "-view_handle-")

  data.frame(
    id = sub(prefix, "", xml2::xml_attr(docks, "id")),
    active = grepl(
      "(^| )blockr-view-dock-active( |$)", xml2::xml_attr(docks, "class")
    ),
    stringsAsFactors = FALSE
  )
}

# Wrap `wait_for_js` so a timeout dumps the page state before failing. The
# queue-only e2e flakes never reproduce locally, so a residual timeout has to
# carry enough context to be actionable rather than an opaque "JS did not
# evaluate to true" (mirrors setup-idle-debug.R). `diagnose` is a closure that
# returns a one-shot status string.
wait_js <- function(app, cond, diagnose, timeout = 30 * 1000) {
  tryCatch(
    app$wait_for_js(cond, timeout = timeout),
    error = function(e) {
      message("\n--- [e2e-wait] timed out waiting for: ", cond)
      message(diagnose())
      message("----------------------------------------------------")
      stop(e)
    }
  )
}

# A snapshot of the dock shell for the wait diagnostics: which markers are
# present, the server-rendered active view (nav), and the client-toggled dock
# handles -- the latter can lag or stall on a loaded runner, which is exactly
# why the waits below gate on the nav marker and not on these.
dock_shell_diag <- function(app, board_id) {
  js <- r"(JSON.stringify((function () {
    var nav = document.querySelector('#__BID__-view_nav');
    var activeSel = '#__BID__-view_nav .blockr-view-item.active';
    var active = document.querySelector(activeSel);
    var dockActive = document.querySelector('.blockr-view-dock-active');
    return {
      nav: nav !== null,
      blocks: document.querySelectorAll('[id^="__BID__-block_handle-"]').length,
      navActive: active ? active.getAttribute('data-view-id') : null,
      dockHandles: Array.from(document.querySelectorAll('.blockr-view-dock'))
        .map(function (e) { return e.id; }),
      dockActive: dockActive ? dockActive.id : null
    };
  })()))"

  app$get_js(gsub("__BID__", board_id, js, fixed = TRUE))
}

# Wait until the server-rendered dock shell is in place: the view nav, every
# block card and the active view. These render independently of the dockview
# client, so the serialization e2e can read them without racing the
# (client-side) panel layout. The active view is gated on the server-rendered
# nav marker (`.blockr-view-item.active`), not the client-toggled
# `.blockr-view-dock-active` class -- the latter is applied by the `switch-view`
# handler whose retry budget can lapse on a loaded runner, leaving it
# permanently off and ejecting a green PR.
wait_dock_loaded <- function(app, n_blocks, board_id = "my_board") {

  diagnose <- function() dock_shell_diag(app, board_id)

  wait_js(
    app,
    sprintf("document.querySelector('#%s-view_nav') !== null", board_id),
    diagnose
  )
  wait_js(
    app,
    sprintf(
      "document.querySelectorAll('[id^=\"%s-block_handle-\"]').length === %d",
      board_id, n_blocks
    ),
    diagnose
  )
  wait_js(
    app,
    sprintf(
      paste0(
        "document.querySelector(",
        "'#%s-view_nav .blockr-view-item.active') !== null"
      ),
      board_id
    ),
    diagnose
  )
}

# The dock-owned board state observable in server-rendered DOM: the view nav
# (one row per view, including which is active), and every block's card id. The
# serialization e2e captures this before and after a save / restore reload to
# assert the round-trip rebuilds it identically. The active view is read off the
# server-rendered nav marker rather than the client-toggled
# `.blockr-view-dock-active` handle, which can stall on a loaded runner (see
# wait_dock_loaded). Block cards live in a pool the dockview client teleports
# into panels, so query their ids globally rather than from a fixed container.
read_dock_state <- function(app, board_id = "my_board") {

  nav <- read_view_nav(app, board_id)

  handles <- unlist(
    app$get_js(
      sprintf(
        paste0(
          "Array.from(document.querySelectorAll(",
          "'[id^=\"%s-block_handle-\"]')).map(e => e.id)"
        ),
        board_id
      )
    )
  )
  block_ids <- sort(sub(paste0("^", board_id, "-block_handle-"), "", handles))

  list(
    nav = nav,
    active_view = nav$id[nav$active],
    blocks = block_ids
  )
}

# A block panel's open tab outlives the panel content, which dockview detaches
# while inactive -- so the tab strip, not the (detached) card, is where panel
# presence is observable. Each tab carries its panel id as
# `...-dock-tab-block_panel-<id>`; read them sorted straight off the live DOM.
block_panel_tabs <- function(app, board_id = "my_board") {
  html <- xml2::read_html(
    app$get_html(paste0("#", board_id, "-view_container"))
  )
  nodes <- xml2::xml_find_all(html, "//*[contains(@id, '-tab-block_panel-')]")
  sort(sub(".*-tab-(block_panel-.+)$", "\\1", xml2::xml_attr(nodes, "id")))
}

# The block panels whose tab is the *front* (active) tab of its dockview group,
# scoped to one `view`'s dock (every view's dock is in the DOM, so an unscoped
# read would mix in another view's front tabs). The client-rendered geometry a
# server-side read cannot see: dockview marks the active tab by toggling
# `dv-active-tab` on the `.dv-tab` wrapper of the custom tab element (id
# `...-tab-block_panel-<id>`), so the front tab is that wrapper's descendant tab
# id. A restored group shows exactly one front tab; a collapse to separate
# leaves surfaces every panel as its own. The view's dock sits in the stable,
# server-rendered `#<board>-view_handle-<view>` (not the client-toggled
# `.blockr-view-dock-active`, which can lag).
active_block_panel_tabs <- function(app, view, board_id = "my_board") {
  html <- xml2::read_html(
    app$get_html(paste0("#", board_id, "-view_handle-", view))
  )
  xpath <- paste0(
    "//*[contains(@id, '-tab-block_panel-')]",
    "[ancestor-or-self::*[contains(concat(' ', normalize-space(@class), ' '), ",
    "' dv-active-tab ')]]"
  )
  nodes <- xml2::xml_find_all(html, xpath)
  ids <- sub(".*-tab-(block_panel-.+)$", "\\1", xml2::xml_attr(nodes, "id"))
  sort(unique(ids))
}

# Wait until `view`'s dock has settled to exactly `expected` active (front)
# block-panel tabs. dockviewR restores a tab group asynchronously and can
# transiently surface its members as separate leaves (each its own front tab)
# before collapsing the back tabs; under a heavier startup that transient
# outlasts wait_dock_loaded (which gates on server DOM). Reading the tabs -- or
# the mirrored grid the export serialises from the client layout -- before it
# settles catches the separate-leaves state. `expected` is the settled
# `block_panel-<id>` front tabs (matched exactly, like active_block_panel_tabs).
wait_active_block_tabs <- function(app, view, expected, board_id = "my_board",
                                   timeout = 30 * 1000) {
  want <- paste(sort(expected), collapse = ",")

  js <- sprintf(
    paste0(
      "(function(){",
      "var d=document.querySelector('#%s-view_handle-%s');if(!d)return false;",
      "var a=Array.from(d.querySelectorAll('[id*=\"-tab-block_panel-\"]'))",
      ".filter(function(e){return e.closest('.dv-active-tab')!==null;})",
      ".map(function(e){return e.id",
      ".replace(/.*-tab-(block_panel-.+)$/,'$1');});",
      "return Array.from(new Set(a)).sort().join(',')==='%s';})()"
    ),
    board_id, view, want
  )

  diagnose <- function() {
    sprintf(
      "[active-block-tabs] view=%s want=[%s] got=[%s]", view, want,
      paste(active_block_panel_tabs(app, view, board_id), collapse = ",")
    )
  }

  wait_js(app, js, diagnose, timeout)
}

# Shared helpers for the edit-board extension e2e tests (links, stacks). The
# extension namespaces its inputs under `my_board-ext_edit_board-`. The
# extension panel stays active in those tests (pre-seeded fixtures, no block
# adds), so plain set_inputs/click drive it reliably.
nsid <- function(x) paste0("my_board-ext_edit_board-", x)

# Staging inputs do not drive an output, so do not wait for one.
set_in <- function(app, id, value) {
  do.call(
    app$set_inputs,
    c(set_names(list(value), nsid(id)), list(wait_ = FALSE))
  )
}

click <- function(app, id) app$click(nsid(id))

# A DataTables redraw (e.g. after adding a row) re-renders the cell inputs,
# which the table's `drawCallback` re-binds via `Shiny.bindAll` once the async
# redraw lands -- which can be after the server reports idle. The old approach
# polled for the `.shiny-bound-input` marker, but `Shiny.unbindAll` strips that
# class on *every* redraw, so a poll can sample an unbound window (or a stalled
# rebind can leave it off for the whole budget) and eject a green PR. Instead
# latch a sticky flag the first time a `shiny:bound` event reports the target
# bound: set once, it survives the later unbind/rebind churn. `draw.dt` is
# counted only to make a residual timeout diagnosable. The triggering click has
# already awaited idle, so wait_bound adds no idle wait of its own (one would
# only re-expose the separate idle-check flake).
wait_bound <- function(app, id, timeout = 30 * 1000) {

  target <- nsid(id)

  js <- r"((function () {
    var id = '__ID__';
    var bound = function () {
      var el = document.getElementById(id);
      return !!(el && el.classList.contains('shiny-bound-input'));
    };
    var st = {done: bound(), draws: 0, existed: !!document.getElementById(id)};
    window.__blockrWaitBound = st;
    if (st.done) return;
    $(document).on('draw.dt.blockrWaitBound', function () { st.draws += 1; });
    $(document).on('shiny:bound.blockrWaitBound', function () {
      if (bound()) { st.done = true; $(document).off('.blockrWaitBound'); }
    });
  })())"

  app$run_js(gsub("__ID__", target, js, fixed = TRUE))

  diagnose <- function() {
    sprintf(
      "[wait_bound] id=%s state=%s tables=%s",
      target,
      app$get_js("JSON.stringify(window.__blockrWaitBound)"),
      app$get_js("document.querySelectorAll('table.dataTable').length")
    )
  }

  wait_js(
    app,
    "window.__blockrWaitBound && window.__blockrWaitBound.done === true",
    diagnose,
    timeout
  )
}

set_color <- function(app, id, hex) {
  app$run_js(
    paste0(
      "var e=document.getElementById('", nsid(id), "'); e.value='", hex,
      "'; e.dispatchEvent(new Event('change'));"
    )
  )
}

field <- function(app, id) {
  app$get_js(
    paste0(
      "(function(){var e=document.getElementById('", nsid(id),
      "'); return e ? e.value : null;})()"
    )
  )
}
