test_that("panel layout", {
  expect_snapshot(draw_panel_tree(NULL))
  expect_snapshot(draw_panel_tree(c("a", "b", "c")))
  expect_snapshot(draw_panel_tree(list("a", list("b", "c"))))
})

test_that("lock_panels flips tabComponent both ways (#124)", {

  # Build a tiny layout via the public create_dock_layout in unlocked mode:
  # panels should come out as closable (tabComponent = "manual").
  unlocked_layout <- withr::with_options(
    list(blockr.dock_is_locked = NULL),
    create_dock_layout(c(a = new_dataset_block()))
  )

  saved_tabs <- vapply(
    unlocked_layout[["panels"]], `[[`, character(1L), "tabComponent"
  )
  expect_true(all(saved_tabs == "manual"))

  # Restoring in a locked app must flip every panel to non-closable.
  locked_view <- lock_panels(unlocked_layout, locked = TRUE)
  restored_tabs <- vapply(
    locked_view[["panels"]], `[[`, character(1L), "tabComponent"
  )
  expect_true(all(restored_tabs == "custom"))
  expect_true(
    all(vapply(
      locked_view[["panels"]],
      function(p) is.null(p[["params"]][["removeCallback"]]),
      logical(1L)
    ))
  )

  # And the inverse: a layout that came in locked should become closable
  # again when restored in an unlocked app. Both `tabComponent` AND
  # `removeCallback` must be restored to their canonical unlocked shape,
  # otherwise clicking the close X would throw a TypeError client-side
  # because `DefaultTab` calls `config.params.removeCallback(config)`
  # unconditionally.
  unlocked_view <- lock_panels(locked_view, locked = FALSE)
  expect_true(all(
    vapply(unlocked_view[["panels"]], `[[`, character(1L), "tabComponent") ==
      "manual"
  ))
  expect_true(all(vapply(
    unlocked_view[["panels"]],
    function(p) {
      rc <- p[["params"]][["removeCallback"]]
      isTRUE(rc[["__IS_FUNCTION__"]]) &&
        grepl("panel-to-remove", rc[["source"]], fixed = TRUE)
    },
    logical(1L)
  )))
})

test_that("lock_panels restores callback on save-locked -> restore-unlocked", {

  # The reverse-direction round-trip the original test didn't cover: build
  # a layout in locked mode (so `removeCallback` is NULL at save time),
  # then restore unlocked. The restored panels must carry a callable
  # `removeCallback` so the close X works (#TBD).
  locked_layout <- withr::with_options(
    list(blockr.dock_is_locked = TRUE),
    create_dock_layout(c(a = new_dataset_block()))
  )

  saved_callbacks <- lapply(
    locked_layout[["panels"]],
    function(p) p[["params"]][["removeCallback"]]
  )
  expect_true(all(vapply(saved_callbacks, is.null, logical(1L))))

  unlocked_view <- lock_panels(locked_layout, locked = FALSE)
  for (p in unlocked_view[["panels"]]) {
    expect_identical(p[["tabComponent"]], "manual")
    rc <- p[["params"]][["removeCallback"]]
    expect_true(isTRUE(rc[["__IS_FUNCTION__"]]))
    expect_match(rc[["source"]], "panel-to-remove", fixed = TRUE)
  }
})
