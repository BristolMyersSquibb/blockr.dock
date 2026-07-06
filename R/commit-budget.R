# The commit ledger records the board commits the dock issues within one
# reactive flush, each attributed to the writer that produced it -- read off the
# update payload's slots (the grid mirror writes `views$grid`, the
# membership lifecycle `views$mod` / `blocks`, view CRUD the remaining slots).
# `dock_commit_budget()` is a dev-mode bound on commits per flush: an unbounded
# server-side loop spins inside a single `flushReact()` that never returns, so a
# flush that commits past the bound aborts with the attributed writer chain --
# the hang surfaces as a stack trace naming its two writers. Off by default
# (`Inf`); dev / CI set `options(blockr.dock_commit_budget = N)` or
# `BLOCKR_DOCK_COMMIT_BUDGET=N`.
#
# The runtime tooth is deliberately intra-flush only. A cross-flush echo loop
# advances one commit per flush (the scheduler runs between iterations), so it
# never trips the bound -- but that loop class is dead by design (no board ->
# dock push in normal operation), and the e2e commit-count sentinel is its
# guard: it never reaches quiescence, so the sentinel's zero-after-quiescence
# assertion fails instead. The monotonic `count` is the per-session tally that
# sentinel reads to assert one commit per settled gesture and none after
# quiescence.

dock_commit_budget <- function() {
  as.numeric(blockr_option("dock_commit_budget", Inf))
}

commit_probe_enabled <- function() {
  is.finite(dock_commit_budget())
}

new_commit_ledger <- function() {
  ledger <- new.env(parent = emptyenv())
  ledger$count <- reactiveVal(0L)
  ledger$flush <- character()
  ledger
}

# Which writer a dock update payload commits through. A single update touches
# one slot, so this is usually one label; the flush chain is the ordered list of
# labels across a flush's commits, and two entries for the same view name the
# racing writers a loop would show.
attribute_commit <- function(payload) {

  views <- payload[["views"]]

  writer <- character()

  if (not_null(views)) {
    if (not_null(views[["grid"]])) writer <- c(writer, "grid mirror")
    if (not_null(views[["mod"]])) writer <- c(writer, "membership lifecycle")
    if (not_null(views[["add"]])) writer <- c(writer, "view add")
    if (not_null(views[["rm"]])) writer <- c(writer, "view remove")
    if (not_null(views[["rename"]])) writer <- c(writer, "view rename")
    if (not_null(views[["active"]])) writer <- c(writer, "view switch")
  }

  if (not_null(payload[["blocks"]])) writer <- c(writer, "block lifecycle")
  if (not_null(payload[["extensions"]])) writer <- c(writer, "extension")

  if (!length(writer)) {
    return("unknown")
  }

  paste(writer, collapse = "+")
}

record_commit <- function(ledger, writer, budget = dock_commit_budget()) {

  ledger$count(isolate(ledger$count()) + 1L)
  ledger$flush <- c(ledger$flush, writer)

  if (length(ledger$flush) > budget) {
    blockr_abort(
      paste0(
        "Dock commit budget of ", budget, " per flush exceeded: a single ",
        "reactive flush committed the board ", length(ledger$flush),
        " times -- the signature of a layout loop. Writer chain: ",
        paste(ledger$flush, collapse = " -> "), "."
      ),
      class = "dock_commit_budget_exceeded"
    )
  }

  invisible(ledger)
}

reset_flush_ledger <- function(ledger) {
  ledger$flush <- character()
  invisible(ledger)
}

# Wrap the board `update` handle so every dock-issued commit is counted and
# budget-checked, while a bare `update()` read forwards unchanged. The flush
# chain resets after each flush settles, so the bound is per flush; the
# monotonic count never resets.
instrument_commits <- function(update, ledger, session = get_session()) {

  # The caller reassigns `update <- instrument_commits(update, ...)`, so force
  # the promise now: left lazy, the closure below would resolve `update` to the
  # reassigned wrapper and recurse forever.
  force(update)

  session$onFlushed(function() reset_flush_ledger(ledger), once = FALSE)

  function(payload) {

    if (missing(payload)) {
      return(update())
    }

    record_commit(ledger, attribute_commit(payload))

    update(payload)
  }
}

# Round-trip stability: the authored grid a view is restored with must be the
# fixed point the client echoes back, so the debounced echo the restore push
# provokes is absorbed by the mirror's `all.equal(tolerance = grid_size_tol())`
# guard rather than committing a second time. It holds when, for every view, the
# stored grid is that-tolerance-equal to the projection of the live layout --
# the very comparison the mirror commits on. Read post-load, pre-gesture: once a
# gesture lands the mirror has stored `project_grid()` of the live layout and
# equality is trivial; the authored-vs-live comparison is the one that can fail.
grids_stable <- function(stored, live) {

  view_stable <- function(v) {
    isTRUE(
      all.equal(
        stored[[v]], project_grid(live[[v]]), tolerance = grid_size_tol()
      )
    )
  }

  all(lgl_ply(names(live), view_stable))
}

# Two hidden dev-mode readouts, rendered only when a budget is set: the
# monotonic commit count (the sentinel asserts a settled gesture commits once
# and quiescence adds nothing) and the round-trip stability flag (asserted
# post-load).
commit_probe_ui <- function(id) {

  if (!commit_probe_enabled()) {
    return(NULL)
  }

  div(
    id = NS(id, "commit_probe_wrap"),
    class = "blockr-commit-probe",
    style = "display: none;",
    textOutput(NS(id, "commit_probe"), inline = TRUE),
    textOutput(NS(id, "roundtrip_stable"), inline = TRUE)
  )
}

register_commit_probe <- function(ledger, session = get_session()) {

  if (!commit_probe_enabled()) {
    return(invisible())
  }

  session$output$commit_probe <- renderText(as.character(ledger$count()))

  # The probe is hidden, so Shiny would suspend it and the DOM would never
  # reflect a commit; keep it live so the e2e sentinel can read the count.
  outputOptions(session$output, "commit_probe", suspendWhenHidden = FALSE)

  invisible()
}

# The round-trip stability readout: `TRUE` once every view's stored grid
# matches the projection of its live layout. `NULL` until every view has
# reported a layout (view_data is all-or-nothing), so the sentinel waits for it.
register_roundtrip_probe <- function(board, view_data,
                                     session = get_session()) {

  if (!commit_probe_enabled()) {
    return(invisible())
  }

  session$output$roundtrip_stable <- renderText({
    live <- req(view_data())
    as.character(grids_stable(board_grids(board$board), live))
  })

  outputOptions(session$output, "roundtrip_stable", suspendWhenHidden = FALSE)

  invisible()
}
