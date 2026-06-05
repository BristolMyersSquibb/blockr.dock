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
        description = 1L,
        class = c("test_extension", "dock_extension")
      )
    ),
    class = "dock_extension_description_invalid"
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

ctrl_ext <- function(external_ctrl = TRUE, ctor = function(content = "") NULL) {
  new_dock_extension(
    server = function(id, ...) NULL,
    ui = function(id) tagList(),
    name = "Document",
    class = "doc_extension",
    ctor = ctor,
    external_ctrl = external_ctrl
  )
}

test_that("new_dock_extension validates external_ctrl", {

  expect_error(ctrl_ext(external_ctrl = 1L))

  expect_identical(attr(new_edit_board_extension(), "external_ctrl"), FALSE)
  expect_identical(attr(ctrl_ext(), "external_ctrl"), TRUE)
})

test_that("extension description defaults to NULL and is accessible", {

  expect_null(extension_description(ctrl_ext()))

  desc <- "References block results via blockr://<block_id>."

  expect_identical(
    extension_description(new_edit_board_extension(description = desc)),
    desc
  )
})

test_that("ext_ctor_inputs drops dots", {

  ext <- ctrl_ext(ctor = function(content = "", select = "", ...) NULL)
  expect_setequal(ext_ctor_inputs(ext), c("content", "select"))
})

test_that("external_ctrl_vars.dock_extension resolves against ctor inputs", {

  ext <- ctrl_ext(ctor = function(content = "", select = "") NULL)

  expect_setequal(external_ctrl_vars(ext), c("content", "select"))

  attr(ext, "external_ctrl") <- FALSE
  expect_identical(external_ctrl_vars(ext), character())

  attr(ext, "external_ctrl") <- "content"
  expect_identical(external_ctrl_vars(ext), "content")

  attr(ext, "external_ctrl") <- "nonexistent"
  expect_error(external_ctrl_vars(ext))
})

test_that("validate_ext_srv_result enforces controllable reactiveVals", {

  ext <- ctrl_ext()

  expect_silent(
    validate_ext_srv_result(list(state = list(content = reactiveVal("x"))), ext)
  )

  expect_error(
    validate_ext_srv_result(list(state = list(content = reactive("x"))), ext),
    class = "extension_ctrl_var_not_rv"
  )

  expect_error(
    validate_ext_srv_result(list(state = list(other = reactiveVal(1))), ext),
    class = "extension_ctrl_var_absent"
  )

  off <- ctrl_ext(external_ctrl = FALSE)

  expect_silent(
    validate_ext_srv_result(list(state = list(content = reactive("x"))), off)
  )
})

test_that("str_value.dock_extension lists ctor inputs, marking controllable", {

  ext <- ctrl_ext(
    external_ctrl = "content",
    ctor = function(content = "", select = "") NULL
  )

  expect_identical(str_value(ext), "<doc_extension> content*, select")

  expect_output(
    str(ext),
    "<doc_extension> content*, select",
    fixed = TRUE
  )
})

test_that("str_value.dock_extension renders a stateless extension bare", {

  expect_identical(
    str_value(new_edit_board_extension()),
    "<edit_board_extension>"
  )
})

test_that("str_value.dock_extensions renders one line per extension", {

  exts <- as_dock_extensions(
    list(
      ctrl_ext(
        external_ctrl = "content",
        ctor = function(content = "", select = "") NULL
      ),
      new_edit_board_extension()
    )
  )

  expect_identical(
    str_value(exts),
    paste(
      "<dock_extensions[2]>",
      "  doc_extension: <doc_extension> content*, select",
      "  edit_board_extension: <edit_board_extension>",
      sep = "\n"
    )
  )

  expect_output(
    str(exts),
    "doc_extension: <doc_extension> content*, select",
    fixed = TRUE
  )
})

test_that("str_value.dock_extensions keys lines by the container alias", {

  exts <- as_dock_extensions(
    list(my_doc = ctrl_ext(ctor = function() NULL))
  )

  expect_identical(names(exts), "doc_extension")

  expect_identical(
    str_value(exts),
    paste(
      "<dock_extensions[1]>",
      "  my_doc: <doc_extension>",
      sep = "\n"
    )
  )
})

test_that("str_value.dock_extensions handles an empty container", {

  expect_identical(str_value(new_dock_extensions()), "<dock_extensions[0]>")
})
