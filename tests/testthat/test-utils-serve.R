test_that("serve utils", {

  board <- new_dock_board()

  expect_s3_class(
    blockr_app_options(board),
    "board_options"
  )

  expect_s3_class(
    blockr_app_ui(
      "test",
      board,
      blockr_app_plugins(board),
      blockr_app_options(board)
    ),
    "shiny.tag.list"
  )

  testServer(
    blockr_app_server,
    session$flushReact(),
    args = list(
      board,
      blockr_app_plugins(board),
      blockr_app_options(board)
    )
  )
})

test_that("dock app", {

  skip_on_cran()

  app <- shinytest2::AppDriver$new(
    system.file("examples", "empty", "app.R", package = "blockr.dock"),
    name = "dock",
    seed = 42,
    load_timeout = 30 * 1000
  )

  app$set_inputs(
    `my_board-edit_board_extension-registry_select` = "dataset_block",
    `my_board-edit_board_extension-block_id` = "a"
  )

  app$click("my_board-edit_board_extension-confirm_add")

  app$wait_for_idle()
  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$set_inputs(
    `my_board-edit_board_extension-registry_select` = "head_block",
    `my_board-edit_board_extension-block_id` = "b"
  )

  app$click("my_board-edit_board_extension-confirm_add")

  app$wait_for_idle()
  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$set_inputs(`my_board-edit_board_extension-new_link_id` = "ab")

  app$click("my_board-edit_board_extension-add_link")
  app$wait_for_idle()

  app$set_inputs(`my_board-edit_board_extension-ab_from` = "a")
  app$wait_for_idle()

  app$set_inputs(`my_board-edit_board_extension-ab_to` = "b")
  app$wait_for_idle()

  app$set_inputs(`my_board-edit_board_extension-ab_input` = "data")
  app$wait_for_idle()

  app$click("my_board-edit_board_extension-apply_changes")

  app$wait_for_idle()
  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$set_inputs(`my_board-edit_board_extension-new_stack_id` = "s1")

  app$click("my_board-edit_board_extension-add_stack")
  app$wait_for_idle()

  app$set_inputs(`my_board-edit_board_extension-s1_name` = "My Stack")
  app$wait_for_idle()

  app$set_inputs(`my_board-edit_board_extension-s1_blocks` = "a")
  app$wait_for_idle()

  app$click("my_board-edit_board_extension-apply_changes")

  app$wait_for_idle()
  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$stop()
})

test_that("edit board extension stacks (e2e)", {

  skip_on_cran()

  app <- shinytest2::AppDriver$new(
    test_path("apps", "edit-stacks"),
    name = "edit-stacks",
    seed = 42,
    load_timeout = 30 * 1000
  )

  nsid <- function(x) paste0("my_board-edit_board_extension-", x)

  set_in <- function(id, value) {
    do.call(app$set_inputs, set_names(list(value), nsid(id)))
  }

  click <- function(id) app$click(nsid(id))

  set_color <- function(id, hex) {
    app$run_js(
      paste0(
        "var e=document.getElementById('", nsid(id), "'); e.value='", hex,
        "'; e.dispatchEvent(new Event('change'));"
      )
    )
  }

  field <- function(id) {
    app$get_js(
      paste0(
        "(function(){var e=document.getElementById('", nsid(id),
        "'); return e ? e.value : null;})()"
      )
    )
  }

  set_in("new_stack_id", "grp")
  click("add_stack")
  app$wait_for_idle()

  set_in("grp_name", "Group A")
  set_in("grp_blocks", "data")
  set_color("grp_color", "#aabbcc")
  app$wait_for_idle()

  click("apply_changes")
  app$wait_for_idle()

  expect_identical(field("grp_name"), "Group A")
  expect_identical(tolower(field("grp_color")), "#aabbcc")

  set_color("grp_color", "#112233")
  app$wait_for_idle()
  click("apply_changes")
  app$wait_for_idle()

  expect_identical(tolower(field("grp_color")), "#112233")

  app$run_js(
    paste0(
      "Shiny.setInputValue('", nsid("stacks_dt_rows_selected"),
      "', [1], {priority: 'event'});"
    )
  )
  app$wait_for_idle()
  click("rm_stack")
  app$wait_for_idle()
  click("apply_changes")
  app$wait_for_idle()

  expect_null(field("grp_name"))

  app$stop()
})
