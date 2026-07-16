# Model-based interleaving tests for the split-record flow. The pure reducers --
# `apply_board_update` for membership (`views$mod`) and the grid mirror
# (`views$grid`), plus `view_grid()` -- are driven against an abstract client
# that tracks a rendered panel set and a `dock_grid` it echoes. The client
# places panels arbitrarily; every property here is placement-agnostic, so no
# dockview logic is duplicated and arbitrary placement is a stronger test than a
# faithful one.
#
# The event alphabet is {server op, client gesture, echo delivery (with flush
# lag), restore}. A settled echo is a snapshot the generator delivers some steps
# after it is taken, so membership can shift underneath it -- the flush lag that
# makes an in-flight echo carry a since-removed panel (a ghost) or miss a
# just-added one. Placement is member-driven now: a view shows exactly its
# members (a ghost is dropped, a member the grid omits is given a default spot),
# so the grid is pure geometry and membership is authoritative. The single view
# is "V"; multi-view membership is covered in test-views-delta.R.

model_pool <- function() {
  set_names(
    lapply(seq_len(6L), function(i) new_dataset_block()),
    letters[seq_len(6L)]
  )
}

model_panels <- function() as.character(as_block_panel_id(letters[seq_len(6L)]))

# The client's echoed geometry: a flat or lightly-nested `dock_grid` over its
# rendered set, with arbitrary sizes so the mirror's size tolerance is hit. A
# single panel (or none) has no geometry to echo.
client_grid <- function(rendered, sizes = NULL, nested = FALSE) {

  rendered <- as.character(rendered)

  if (!length(rendered)) {
    return(new_dock_grid())
  }

  if (length(rendered) == 1L) {
    return(dock_grid(rendered))
  }

  if (isTRUE(nested) && length(rendered) >= 3L) {
    head <- rendered[[1L]]
    tail <- rendered[-1L]
    return(
      dock_grid(head, do.call(group, as.list(tail)), sizes = c(0.4, 0.6))
    )
  }

  args <- as.list(rendered)

  if (not_null(sizes)) {
    args <- c(args, list(sizes = sizes))
  }

  do.call(dock_grid, args)
}

model_new <- function() {
  brd <- new_dock_board(
    blocks = model_pool(),
    grids = list(V = dock_grid("a", "b", "c"))
  )
  members <- view_members(board_views(brd)[["V"]])
  list(
    board = brd,
    client = list(rendered = members, arr = client_grid(members),
                  pending = NULL)
  )
}

model_members <- function(st) view_members(board_views(st$board)[["V"]])

model_stored <- function(st) board_grids(st$board)[["V"]]

# The placed panel set: what the view actually shows. Member-driven, so it is
# always exactly the members (ghosts dropped, omitted members defaulted).
model_shown <- function(st) {
  grid_panel_ids(view_grid(board_views(st$board)[["V"]], model_stored(st)))
}

# Membership write through the pure reducer, reporting whether the set changed
# (one board commit) or not (a no-op). Expressed in the panel-op grammar: the
# target set becomes an `add` of the gained panels and an `rm` of the lost ones.
model_set_members <- function(st, members) {

  before <- model_members(st)

  gained <- setdiff(members, before)
  lost <- setdiff(before, members)

  mod <- list()

  if (length(gained)) {
    mod[["add"]] <- set_names(rep_len(list(list()), length(gained)), gained)
  }

  if (length(lost)) {
    mod[["rm"]] <- as.character(lost)
  }

  st$board <- apply_board_update(
    st$board,
    list(views = list(mod = set_names(list(mod), "V")))
  )

  list(st = st, commit = as.integer(!setequal(before, model_members(st))))
}

# The grid mirror, reproduced from observe_grid_echo(): a server-sourced echo --
# an intermediate frame dockviewR streams while applying a restore -- is dropped
# by the `_state-source` skip and never commits; a client echo casts to a
# `dock_grid` and commits only when it falls outside the size tolerance of what
# is stored (the all.equal guard), else no-ops. Returns the commit count so the
# "one gesture, at most one commit" bound is checkable.
model_deliver_echo <- function(st, source = "client") {

  echo <- st$client$pending

  if (is.null(echo)) {
    return(list(st = st, commit = 0L))
  }

  if (identical(source, "server")) {
    st$client$pending <- NULL
    return(list(st = st, commit = 0L))
  }

  grid <- as_dock_grid(echo)
  stored <- model_stored(st)

  if (isTRUE(all.equal(stored, grid, tolerance = grid_size_tol()))) {
    st$client$pending <- NULL
    return(list(st = st, commit = 0L))
  }

  st$board <- apply_board_update(
    st$board,
    list(views = list(grid = set_names(list(grid), "V")))
  )
  st$client$pending <- NULL

  list(st = st, commit = 1L)
}

# Restore-as-rebuild: reconstruct the board from its views + grids, which the
# constructor restricts to the authoritative membership (dropping ghosts and
# unknowns), then rebuild the client from the restored board.
model_restore <- function(st) {

  st$board <- new_dock_board(
    blocks = board_blocks(st$board),
    views = board_views(st$board),
    grids = board_grids(st$board)
  )

  members <- model_members(st)
  st$client$rendered <- members
  st$client$arr <- view_grid(board_views(st$board)[["V"]], model_stored(st))
  st$client$pending <- NULL

  st
}

# No ghost rendering and membership authority in one check: the view shows
# exactly its members. A ghost (grid panel no longer a member) is dropped, a
# member the grid omits is defaulted -- neither leaves the shown set unequal to
# membership.
model_placement_total <- function(st) {
  setequal(model_shown(st), model_members(st))
}

# Every per-step invariant in one place, tagged for replay by seed / step.
check_invariants <- function(st, commit, info) {

  testthat::expect_no_error(validate_board(st$board))

  shown <- model_shown(st)
  members <- model_members(st)

  # No ghost rendering: a panel absent from membership never reaches placement.
  testthat::expect_true(all(shown %in% members),
                        info = paste(info, "| ghost render"))

  # Membership authoritative: the view shows exactly its members.
  testthat::expect_true(model_placement_total(st),
                        info = paste(info, "| placement"))

  # One event, at most one board commit.
  testthat::expect_lte(commit, 1L)
  testthat::expect_gte(commit, 0L)
}

model_gesture <- function(st) {

  rendered <- st$client$rendered

  if (length(rendered) >= 2L) {
    ord <- sample(rendered)
    sizes <- runif(length(ord), 0.1, 1)
    nested <- length(ord) >= 3L && runif(1L) < 0.3
    st$client$arr <- client_grid(ord, sizes = sizes, nested = nested)
  } else {
    st$client$arr <- client_grid(rendered)
  }

  st$client$pending <- st$client$arr

  list(st = st, commit = 0L)
}

model_add <- function(st) {

  cand <- setdiff(model_panels(), model_members(st))

  if (!length(cand)) {
    return(list(st = st, commit = 0L))
  }

  p <- if (length(cand) == 1L) cand else sample(cand, 1L)
  res <- model_set_members(st, c(model_members(st), p))

  st <- res$st
  st$client$rendered <- union(st$client$rendered, p)
  st$client$arr <- client_grid(st$client$rendered)

  list(st = st, commit = res$commit)
}

model_remove <- function(st) {

  members <- model_members(st)

  if (!length(members)) {
    return(list(st = st, commit = 0L))
  }

  p <- if (length(members) == 1L) members else sample(members, 1L)
  res <- model_set_members(st, setdiff(members, p))

  st <- res$st
  st$client$rendered <- setdiff(st$client$rendered, p)
  st$client$arr <- client_grid(st$client$rendered)
  # pending is left as-is: an in-flight echo may still carry p, which lands as a
  # ghost until superseded -- the interleaving the flush lag is here to reach.

  list(st = st, commit = res$commit)
}

test_that("a stale echo's ghost is stored verbatim but never placed", {

  st <- model_new()
  cpan <- as.character(as_block_panel_id("c"))

  # The client settles a drag over {a, b, c}; the snapshot is in flight.
  st$client$arr <- client_grid(model_members(st), sizes = c(0.2, 0.3, 0.5))
  st$client$pending <- st$client$arr

  # Membership drops c before the echo lands.
  st <- model_set_members(st, setdiff(model_members(st), cpan))$st

  del <- model_deliver_echo(st)
  st <- del$st

  expect_identical(del$commit, 1L)
  expect_true(cpan %in% layout_panel_ids(model_stored(st)))
  expect_false(cpan %in% model_members(st))
  expect_false(cpan %in% model_shown(st))
  expect_no_error(validate_board(st$board))
})

test_that("an added member is placed at once; its echo refines it", {

  st <- model_new()
  dpan <- as.character(as_block_panel_id("d"))

  st <- model_set_members(st, c(model_members(st), dpan))$st
  st$client$rendered <- c(st$client$rendered, dpan)

  # Membership is authoritative, so d is shown the instant it is a member --
  # view_grid appends it a default spot even before any echo places it.
  expect_true(dpan %in% model_members(st))
  expect_true(dpan %in% model_shown(st))

  # The client echoes a grid that places d; the stored grid now carries it.
  st$client$pending <- client_grid(
    st$client$rendered, sizes = c(0.25, 0.25, 0.25, 0.25)
  )
  st <- model_deliver_echo(st)$st

  expect_true(dpan %in% layout_panel_ids(model_stored(st)))
  expect_true(dpan %in% model_shown(st))
})

test_that("a settled gesture commits once; a re-echo commits nothing", {

  st <- model_new()

  st$client$arr <- client_grid(model_members(st), sizes = c(0.5, 0.3, 0.2))
  st$client$pending <- st$client$arr

  first <- model_deliver_echo(st)
  st <- first$st
  expect_identical(first$commit, 1L)

  st$client$pending <- st$client$arr
  second <- model_deliver_echo(st)
  expect_identical(second$commit, 0L)

  # Sub-tolerance jitter sits within the mirror's all.equal noise floor -> no
  # commit, even though the stored sizes are kept verbatim (not quantised).
  st$client$pending <- client_grid(
    model_members(st), sizes = c(0.501, 0.299, 0.2)
  )
  expect_identical(model_deliver_echo(st)$commit, 0L)
})

test_that("restore drops a stored ghost, never resurrecting it", {

  st <- model_new()
  cpan <- as.character(as_block_panel_id("c"))

  st$client$arr <- client_grid(model_members(st), sizes = c(0.2, 0.3, 0.5))
  st$client$pending <- st$client$arr
  st <- model_set_members(st, setdiff(model_members(st), cpan))$st
  st <- model_deliver_echo(st)$st

  expect_true(cpan %in% layout_panel_ids(model_stored(st)))

  st <- model_restore(st)

  # Construction restricts each grid to its members, so restore heals the ghost
  # out of the stored slot, not just out of the placement.
  expect_false(cpan %in% model_shown(st))
  expect_false(cpan %in% model_members(st))
  stored <- model_stored(st)
  if (not_null(stored)) {
    expect_false(cpan %in% layout_panel_ids(stored))
  }
  expect_no_error(validate_board(st$board))
})

test_that("restore rebuilds a view whose membership emptied under a ghost", {

  # A committed board can legally hold empty membership beside a grid that is
  # now all ghosts (every member removed after the echo settled). Restoring it
  # restricts the grid down to nothing, which must rebuild rather than abort.
  st <- model_new()

  st$client$arr <- client_grid(model_members(st), sizes = c(0.2, 0.3, 0.5))
  st$client$pending <- st$client$arr
  st <- model_deliver_echo(st)$st

  for (p in model_members(st)) {
    st <- model_set_members(st, setdiff(model_members(st), p))$st
  }

  expect_length(model_members(st), 0L)
  expect_no_error(validate_board(st$board))

  st <- model_restore(st)

  expect_length(model_shown(st), 0L)
  expect_length(model_members(st), 0L)
  expect_no_error(validate_board(st$board))
})

test_that("a restore's staged frames never overwrite the authored grid", {

  # dockviewR applies a restored grid incrementally, streaming intermediate
  # `_state` frames -- a tab group transiently split into separate leaves before
  # the back tabs collapse. Each is server-sourced (the mirror's own restore
  # push provoked it), so the `_state-source` skip drops it and the committed
  # grid holds at the authored value: an export mid-load persists the pushed
  # arrangement, not a frame that was never anyone's intent. The invariant --
  # between a boundary push and its client ack the committed grid never diverges
  # from authored absent a membership change -- is why the skip is a cause
  # filter where the old debounce was only a (defeatable) time filter.
  st <- model_new()
  members <- model_members(st)

  # Authored: the members grouped into one tab (a leaf beside a group). The
  # restore push settles it as the stored, authoritative grid.
  st$client$pending <- client_grid(members, nested = TRUE)
  st <- model_deliver_echo(st)$st
  st <- model_restore(st)
  authored <- model_stored(st)
  expect_false(is.null(authored))

  # The restore application streams the group as separate leaves, in shifting
  # orders and sizes -- flat frames, structurally unlike the grouped authored,
  # so only the `_state-source` skip (not the all.equal tolerance) holds them.
  staged <- list(
    client_grid(members),
    client_grid(rev(members)),
    client_grid(members, sizes = c(0.7, 0.2, 0.1))
  )

  for (i in seq_along(staged)) {

    st$client$pending <- staged[[i]]

    expect_false(
      isTRUE(all.equal(authored, as_dock_grid(staged[[i]]),
                       tolerance = grid_size_tol())),
      info = sprintf("frame %d differs from authored", i)
    )

    res <- model_deliver_echo(st, source = "server")
    st <- res$st

    expect_identical(res$commit, 0L, info = sprintf("frame %d no commit", i))
    expect_true(
      isTRUE(all.equal(
        model_stored(st), authored, tolerance = grid_size_tol()
      )),
      info = sprintf("frame %d holds authored", i)
    )
  }

  # The user's own reorder is the ack: a client-sourced echo commits, exactly as
  # a gesture always has.
  st$client$pending <- client_grid(rev(members))

  expect_identical(model_deliver_echo(st, source = "client")$commit, 1L)
})

test_that("seeded interleavings hold the invariants and converge", {

  events <- c("gesture", "echo", "add", "remove", "restore")
  probs <- c(0.30, 0.30, 0.16, 0.16, 0.08)

  for (seed in seq_len(40L)) {

    set.seed(seed)
    st <- model_new()

    for (i in seq_len(30L)) {

      ev <- sample(events, 1L, prob = probs)

      res <- switch(
        ev,
        gesture = model_gesture(st),
        echo = model_deliver_echo(st),
        add = model_add(st),
        remove = model_remove(st),
        restore = list(st = model_restore(st), commit = 0L)
      )

      st <- res$st
      check_invariants(
        st, res$commit, sprintf("seed=%d step=%d ev=%s", seed, i, ev)
      )
    }

    # Quiescence: the client catches up to membership, echoes once, and the
    # board must converge to what the client shows and hold as a fixed point.
    members <- model_members(st)
    n <- length(members)
    st$client$rendered <- members
    st$client$arr <- client_grid(members, sizes = if (n >= 2L) seq_len(n))
    st$client$pending <- st$client$arr
    st <- model_deliver_echo(st)$st

    info <- sprintf("seed=%d quiescence", seed)

    expect_true(
      setequal(model_shown(st), members), info = paste(info, "content")
    )
    expect_true(
      setequal(model_shown(st), st$client$rendered),
      info = paste(info, "client agree")
    )
    # Geometry converged: the stored grid is tolerance-equal to the client echo.
    expect_true(
      isTRUE(all.equal(
        model_stored(st), as_dock_grid(st$client$arr),
        tolerance = grid_size_tol()
      )),
      info = paste(info, "geometry")
    )

    st$client$pending <- st$client$arr
    expect_identical(
      model_deliver_echo(st)$commit, 0L,
      info = paste(info, "fixed point")
    )
  }
})
