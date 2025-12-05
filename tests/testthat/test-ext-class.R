test_that("dock extension validation", {

  expect_error(
    validate_extension(structure(1L, class = "dock_extension")),
    class = "dock_extension_inheritance_invalid"
  )

  expect_error(
    validate_extension(
      structure(
        1L,
        class = c("test", "dock_extension")
      )
    ),
    class = "dock_extension_structure_invalid"
  )

  expect_error(
    validate_extension(
      structure(
        list(ui = 1L, server = 1L),
        class = c("test", "dock_extension")
      )
    ),
    class = "dock_extension_id_invalid"
  )

  expect_error(
    validate_extension(
      structure(
        list(ui = 1L, server = 1L),
        name = 1L,
        class = c("test_extension", "dock_extension")
      )
    ),
    class = "dock_extension_name_invalid"
  )

  expect_error(
    validate_extension(
      structure(
        list(ui = 1L, server = 1L),
        name = "test",
        class = c("test_extension", "dock_extension")
      )
    ),
    class = "dock_extension_ui_invalid"
  )

  expect_error(
    validate_extension(
      structure(
        list(ui = function() {}, server = 1L),
        name = "test",
        class = c("test_extension", "dock_extension")
      )
    ),
    class = "dock_extension_server_invalid"
  )
})

test_that("dock extensions validation", {

  expect_error(
    validate_extensions(structure(1L, class = "dock_extensions")),
    class = "dock_extensions_structure_invalid"
  )

  expect_error(
    validate_extensions(
      structure(
        list(
          structure(1L, name = "a", class = "dock_extension"),
          structure(2L, name = "a", class = "dock_extension")
        ),
        class = "dock_extensions"
      )
    ),
    class = "dock_extensions_names_invalid"
  )
})
