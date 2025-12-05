test_that("dock app", {

  skip_on_cran()

  app <- shinytest2::AppDriver$new(
    system.file("examples", "dock", "app.R", package = "blockr.dock"),
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

  app$click("my_board-edit_board_extension-modify_links")

  app$wait_for_idle()
  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$stop()
})
