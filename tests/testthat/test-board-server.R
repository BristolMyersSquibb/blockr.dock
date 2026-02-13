test_that("board server", {

  board_rv_1 <- board_args(
    blocks = c(a = new_dataset_block())
  )

  with_mock_session(
    {
      res <- board_server_callback(board_rv_1, update = reactiveVal())

      expect_type(res, "list")
      expect_length(res, 2L)
      expect_named(res, c("dock", "actions"))

      dock <- res[["dock"]]

      expect_type(dock, "list")
      expect_length(dock, 3L)
      expect_named(dock, c("layout", "proxy", "prev_active_group"))

      expect_s3_class(dock[["layout"]], "reactive")
      expect_s3_class(dock[["proxy"]], "dock_view_proxy")
      expect_s3_class(dock[["prev_active_group"]], "reactive")
    }
  )

  board_rv_2 <- board_args(
    blocks = c(a = new_dataset_block()),
    extensions = new_edit_board_extension()
  )

  with_mock_session(
    {
      res <- board_server_callback(board_rv_2, update = reactiveVal())

      expect_type(res, "list")
      expect_length(res, 3L)
      expect_named(res, c("dock", "actions", "edit_board_extension"))

      dock <- res[["dock"]]

      expect_type(dock, "list")
      expect_length(dock, 3L)
      expect_named(dock, c("layout", "proxy", "prev_active_group"))

      expect_s3_class(dock[["layout"]], "reactive")
      expect_s3_class(dock[["proxy"]], "dock_view_proxy")
      expect_s3_class(dock[["prev_active_group"]], "reactive")

      ext <- res[["edit_board_extension"]]

      expect_type(ext, "list")
      expect_length(ext, 1L)
      expect_named(ext, "state")

      expect_identical(ext[["state"]], list())
    }
  )

  withr::defer(suppressMessages(untrace(manage_dock)))

  suppressMessages(
    trace(
      manage_dock,
      exit = quote(
        {
          env <- environment()
          log_info(
            "recording `manage_dock()` execution env: {format(env)}"
          )
          assign(
            "manage_dock",
            env,
            envir = get("trace_env", envir = asNamespace("blockr.dock"))
          )
        }
      ),
      print = FALSE
    )
  )

  with_mock_session(
    {
      manage_dock(
        board_rv_2,
        update = reactiveVal(),
        session = session
      )

      session$flushReact()

      n_panels <- get("n_panels", envir = trace_env$manage_dock)

      do.call(
        session$setInputs,
        set_names(list(TRUE), dock_input("initialized"))
      )

      expect_identical(n_panels(), 2L)

      do.call(
        session$setInputs,
        set_names(
          list(as_block_panel_id("a")),
          dock_input("panel-to-remove")
        )
      )

      expect_identical(n_panels(), 1L)

      do.call(
        session$setInputs,
        set_names(
          list(as_ext_panel_id("edit_board_extension")),
          dock_input("panel-to-remove")
        )
      )

      expect_identical(n_panels(), 0L)

      do.call(
        session$setInputs,
        set_names(
          list(1L, c("blk-a", "ext-edit_board_extension")),
          c("confirm_add", "add_dock_panel")
        )
      )

      expect_identical(n_panels(), 2L)
    }
  )

  with_mock_session(
    {
      manage_dock(
        board_rv_2,
        update = reactiveVal(),
        session = session
      )

      session$flushReact()

      prevs <- get("prev_active_group", envir = trace_env$manage_dock)
      trail <- get("active_group_trail", envir = trace_env$manage_dock)

      expect_null(prevs())
      expect_null(trail())

      do.call(
        session$setInputs,
        set_names(list("1"), dock_input("active-group"))
      )

      expect_null(prevs())
      expect_identical(trail(), "1")

      do.call(
        session$setInputs,
        set_names(list("2"), dock_input("active-group"))
      )

      expect_identical(prevs(), "1")
      expect_identical(trail(), "2")

      do.call(
        session$setInputs,
        set_names(list("2"), dock_input("active-group"))
      )

      expect_identical(prevs(), "1")
      expect_identical(trail(), "2")

      do.call(
        session$setInputs,
        set_names(list("1"), dock_input("active-group"))
      )

      expect_identical(prevs(), "2")
      expect_identical(trail(), "1")
    }
  )

  with_mock_session(
    {
      upd <- reactiveVal()

      manage_dock(
        board_rv_2,
        update = upd,
        session = session
      )

      session$flushReact()

      upd(
        list(
          blocks = list(
            mod = blocks(a = new_dataset_block(block_name = "Test block"))
          )
        )
      )

      with_mocked_bindings(
        session$flushReact(),
        get_dock_panel = function(...) list(title = "Old title")
      )
    }
  )
})
