test_that("dock_get_layout degrades to NULL layout when view layout() is NULL", {
  # Right after a programmatic view rebuild the active view's layout()
  # reactive is NULL until the browser re-renders. dock_get_layout()
  # must return a list (not error in as_dock_layout(NULL)).
  state <- dock_layouts(
    Overview = dock_view("blk_a", active = TRUE),
    Detail   = dock_view("blk_b")
  )

  fake_handle <- list(
    vs = list(state = state),
    dock_mgr = list(
      docks = list(
        Overview = list(layout = function() NULL),
        Detail   = list(layout = function() NULL)
      )
    )
  )

  fake_session <- list(
    userData = list(blockr_dock_handle = fake_handle)
  )

  res <- dock_get_layout(session = fake_session)

  expect_type(res, "list")
  expect_identical(res$active_view, "Overview")
  expect_identical(res$views, c("Overview", "Detail"))
  expect_identical(res$panels, character(0L))
  expect_null(res$layout)
})

test_that("dock_get_layout returns NULL when no dock handle is attached", {
  expect_null(dock_get_layout(session = list(userData = list())))
})
