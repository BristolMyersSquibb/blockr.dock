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
          dup = structure(1L, name = "a", class = "dock_extension"),
          dup = structure(2L, name = "a", class = "dock_extension")
        ),
        class = "dock_extensions"
      )
    ),
    class = "dock_extensions_names_invalid"
  )
})

ctrl_ext <- function(external_ctrl = TRUE, ctor = function(content = "") NULL,
                     description = NULL) {
  new_dock_extension(
    server = function(id, ...) NULL,
    ui = function(id) tagList(),
    name = "Document",
    class = "doc_extension",
    ctor = ctor,
    description = description,
    external_ctrl = external_ctrl
  )
}

test_that("new_dock_extension validates external_ctrl", {

  expect_error(ctrl_ext(external_ctrl = 1L))

  expect_identical(attr(new_edit_board_extension(), "external_ctrl"), FALSE)
  expect_identical(attr(ctrl_ext(), "external_ctrl"), TRUE)
})

test_that("extension description defaults to NULL and is accessible", {

  expect_null(ext_desc(ctrl_ext()))

  desc <- "References block results via blockr://<block_id>."

  expect_identical(
    ext_desc(new_edit_board_extension(description = desc)),
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
      "  doc: <doc_extension> content*, select",
      "  edit_board: <edit_board_extension>",
      sep = "\n"
    )
  )

  expect_output(
    str(exts),
    "doc: <doc_extension> content*, select",
    fixed = TRUE
  )
})

test_that("str_value.dock_extensions keys lines by the container alias", {

  exts <- as_dock_extensions(
    list(my_doc = ctrl_ext(ctor = function() NULL))
  )

  expect_identical(names(exts), "my_doc")

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

test_that("extension keys are container-owned: derived and overridable", {

  expect_identical(
    names(as_dock_extensions(new_edit_board_extension())),
    "edit_board"
  )

  expect_identical(
    names(as_dock_extensions(list(analysis = new_edit_board_extension()))),
    "analysis"
  )
})

test_that("extension keys survive serialization; stale ids degrade", {

  exts <- as_dock_extensions(list(analysis = new_edit_board_extension()))
  ser <- blockr_ser(exts, list())

  expect_identical(names(ser[["payload"]]), "analysis")
  expect_identical(names(blockr_deser(ser)), "analysis")

  # A board saved before container-owned ids has an unnamed payload; deser
  # re-derives the default key rather than erroring (the stale panel member
  # no longer matches and is pruned at restore).
  old <- list(
    object = ser[["object"]],
    payload = set_names(ser[["payload"]], NULL)
  )
  expect_identical(names(blockr_deser(old)), "edit_board")
})

test_that("new_ext_meta assembles and validates its fields", {

  meta <- new_ext_meta(
    "Workflow diagram.",
    arguments = c(positions = "Block coordinates."),
    examples = list(list(positions = list(a = 1))),
    guidance = "Drive via modify_extension."
  )

  expect_true(is_ext_meta(meta))
  expect_identical(meta[["description"]], "Workflow diagram.")
  expect_identical(meta[["guidance"]], "Drive via modify_extension.")
  expect_true(is_arg_specs(meta[["arguments"]]))
  expect_identical(names(meta[["arguments"]]), "positions")

  expect_error(
    new_ext_meta(description = 1L),
    class = "ext_meta_description_invalid"
  )
  expect_error(
    new_ext_meta(guidance = 1L),
    class = "ext_meta_guidance_invalid"
  )
  expect_error(
    new_ext_meta(examples = "x"),
    class = "ext_meta_examples_invalid"
  )
})

test_that("new_ext_meta normalizes the arguments field", {

  expect_identical(names(new_ext_meta()[["arguments"]]), NULL)
  expect_length(new_ext_meta()[["arguments"]], 0L)

  spec <- new_arg_specs(
    positions = new_arg_spec("Coords.", type = arg_string())
  )
  expect_identical(new_ext_meta(arguments = spec)[["arguments"]], spec)

  expect_error(
    new_ext_meta(arguments = c("unnamed")),
    class = "ext_meta_arguments_invalid"
  )
})

test_that("ext_meta reads a bare string or a structured description", {

  expect_identical(ext_meta(ctrl_ext())[["description"]], NULL)

  string_ext <- ctrl_ext(description = "Plain summary.")
  expect_true(is_ext_meta(ext_meta(string_ext)))
  expect_identical(ext_desc(string_ext), "Plain summary.")

  meta <- new_ext_meta("Rich summary.", arguments = c(content = "The text."))
  rich_ext <- ctrl_ext(description = meta)
  expect_identical(ext_meta(rich_ext), meta)
  expect_identical(ext_desc(rich_ext), "Rich summary.")
})

test_that("per-component accessors read the extension metadata", {

  bare <- ctrl_ext()
  expect_null(ext_guidance(bare))
  expect_true(is_arg_specs(ext_args(bare)))
  expect_length(ext_args(bare), 0L)
  expect_identical(ext_examples(bare), list())

  ext <- ctrl_ext(
    description = new_ext_meta(
      "Doc.",
      arguments = c(content = "The text."),
      examples = list(list(content = "hello")),
      guidance = "Drive via modify_extension."
    )
  )

  expect_identical(names(ext_args(ext)), "content")
  expect_identical(ext_examples(ext), list(list(content = "hello")))
  expect_identical(ext_guidance(ext), "Drive via modify_extension.")
})

test_that("extension_description is a deprecated alias of ext_desc", {

  withr::local_options(rlib_warning_verbosity = "verbose")

  ext <- ctrl_ext(description = "Summary.")

  expect_warning(
    result <- extension_description(ext),
    class = "deprecated_extension_description"
  )

  expect_identical(result, "Summary.")
})

test_that("validate_extension gates ext_meta docs against controllable vars", {

  expect_silent(
    ctrl_ext(
      external_ctrl = "content",
      description = new_ext_meta(arguments = c(content = "The text."))
    )
  )

  expect_error(
    ctrl_ext(
      external_ctrl = "content",
      ctor = function(content = "", select = "") NULL,
      description = new_ext_meta(arguments = c(select = "Not controllable."))
    ),
    class = "ext_meta_arguments_not_ctrl"
  )

  expect_error(
    ctrl_ext(
      external_ctrl = "content",
      description = new_ext_meta(examples = list(list(other = 1)))
    ),
    class = "ext_meta_example_not_ctrl"
  )

  expect_error(
    ctrl_ext(
      external_ctrl = "content",
      description = new_ext_meta(examples = list(list(1)))
    ),
    class = "ext_meta_example_invalid"
  )
})
