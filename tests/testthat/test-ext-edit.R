test_that("edit extension server (blocks)", {

  testServer(
    blk_ext_srv,
    {
      expect_null(update())

      session$setInputs(
        registry_select = "dataset_block",
        block_id = "a",
        confirm_add = 1
      )

      res <- update()$blocks

      expect_s3_class(res$add, "blocks")
      expect_length(res$add, 1L)
      expect_null(res$rm)

      expect_null(session$returned)
    },
    args = list(
      board = board_args(),
      update = reactiveVal()
    )
  )

  testServer(
    blk_ext_srv,
    {
      expect_null(update())

      session$setInputs(block_select = "a", confirm_rm = 1)

      session$flushReact()

      res <- update()$blocks

      expect_type(res$rm, "character")
      expect_length(res$rm, 1L)
      expect_null(res$add)

      expect_null(session$returned)
    },
    args = list(
      board = board_args(
        blocks = c(
          a = new_dataset_block("iris"),
          b = new_subset_block()
        ),
        links = links(from = "a", to = "b")
      ),
      update = reactiveVal()
    )
  )

  testServer(
    blk_ext_srv,
    {
      expect_null(update())

      session$setInputs(confirm_add = 1)
      session$setInputs(block_id = "", confirm_add = 2)
      session$setInputs(block_id = "a", confirm_add = 3)
      session$setInputs(block_id = "c", registry_select = "abc",
                        confirm_add = 4)
      session$setInputs(cancel_add = 1)

      session$setInputs(confirm_rm = 1)
      session$setInputs(block_select = "x", confirm_rm = 2)
      session$setInputs(cancel_rm = 1)

      res <- update()$blocks

      expect_null(res$add)
      expect_null(res$rm)

      expect_null(session$returned)
    },
    args = list(
      board = board_args(
        blocks = c(
          a = new_dataset_block("iris"),
          b = new_subset_block()
        ),
        links = links(from = "a", to = "b")
      ),
      update = reactiveVal()
    )
  )
})

test_that("edit extension server (links)", {

  testServer(
    blk_ext_srv,
    {
      expect_null(session$returned)
      expect_null(update())

      expect_identical(upd$add, links())
      expect_identical(upd$rm, character())
      expect_null(upd$edit)

      expect_null(update())

      session$setInputs(add_link = 1)

      expect_s3_class(upd$add, "links")
      expect_length(upd$add, 1L)

      lnk <- names(upd$add)
      expect_identical(upd$add, as_links(set_names(list(new_link()), lnk)))

      session$setInputs(add_link = 2)

      expect_identical(upd$add, as_links(set_names(list(new_link()), lnk)))

      upd$edit <- list(row = names(upd$add), col = "from", val = "a")
      session$flushReact()
      expect_identical(upd$add, as_links(set_names(list(new_link("a")), lnk)))

      upd$edit <- list(row = names(upd$add), col = "to", val = "b")
      session$flushReact()
      expect_identical(
        upd$add,
        as_links(set_names(list(new_link("a", "b")), lnk))
      )

      upd$edit <- list(row = names(upd$add), col = "input", val = "data")
      session$flushReact()
      expect_identical(
        upd$add,
        as_links(set_names(list(new_link("a", "b", "data")), lnk))
      )

      session$setInputs(modify_links = 1)

      expect_identical(
        update()$links,
        list(
          add = as_links(set_names(list(new_link("a", "b", "data")), lnk)),
          rm = NULL
        )
      )

      expect_identical(upd$add, links())
      expect_identical(upd$rm, character())

      expect_null(session$returned)
    },
    args = list(
      board = board_args(
        blocks = c(
          a = new_dataset_block("iris"),
          b = new_subset_block()
        )
      ),
      update = reactiveVal()
    )
  )

  testServer(
    blk_ext_srv,
    {
      expect_null(upd$edit)
      expect_length(upd$add, 0L)

      session$flushReact()

      expect_length(upd$obs, 1L)
      expect_named(upd$obs, "ac")
      expect_type(upd$obs, "list")

      expect_length(upd$obs[["ac"]], 3L)
      expect_named(upd$obs[["ac"]], c("from", "to", "input"))
      expect_type(upd$obs[["ac"]], "list")

      for (i in c("from", "to", "input")) {
        expect_type(upd$obs[["ac"]][[i]], "list")
        for (x in upd$obs[["ac"]][[i]]) {
          expect_s3_class(x, "Observer")
        }
      }

      session$setInputs(ac_input = "data")
      expect_null(upd$edit)
      expect_length(upd$add, 0L)

      session$setInputs(ac_to = "d")
      expect_identical(upd$edit, list(row = "ac", col = "to", val = "d"))
      expect_length(upd$add, 1L)

      session$setInputs(ac_from = "b")
      expect_identical(upd$edit, list(row = "ac", col = "from", val = "b"))
      expect_length(upd$add, 1L)
    },
    args = list(
      board = board_args(
        blocks = c(
          a = new_dataset_block("iris"),
          b = new_dataset_block("mtcars"),
          c = new_subset_block(),
          d = new_subset_block()
        ),
        links = links(ac = new_link(from = "a", to = "c"))
      ),
      update = reactiveVal()
    )
  )

  testServer(
    blk_ext_srv,
    {
      expect_null(session$returned)
      expect_null(update())

      expect_s3_class(upd$curr, "links")
      expect_length(upd$curr, 1L)
      expect_named(upd$curr, "aa")

      expect_identical(upd$rm, character())

      session$setInputs(links_dt_rows_selected = 1, rm_link = 1)

      expect_identical(upd$rm, "aa")

      session$setInputs(modify_links = 1)

      expect_identical(
        update()$links,
        list(add = NULL, rm = "aa")
      )

      expect_identical(upd$add, links())
      expect_identical(upd$rm, character())
      expect_identical(upd$curr, links())

      expect_null(session$returned)
    },
    args = list(
      board = board_args(
        blocks = c(
          a = new_dataset_block("iris"),
          b = new_subset_block()
        ),
        links = links(aa = new_link(from = "a", to = "b"))
      ),
      update = reactiveVal()
    )
  )
})

test_that("edit extension server (stacks)", {

  testServer(
    blk_ext_srv,
    {
      expect_null(update())

      expect_identical(stk$add, stacks())
      expect_identical(stk$rm, character())
      expect_identical(stk$mod, stacks())
      expect_null(stk$edit)

      session$setInputs(new_stack_id = "s1", add_stack = 1)

      expect_s3_class(stk$add, "stacks")
      expect_named(stk$add, "s1")
      expect_true(is_dock_stack(stk$add[["s1"]]))

      stk$edit <- list(row = "s1", col = "name", val = "My Stack")
      session$flushReact()
      stk$edit <- list(row = "s1", col = "color", val = "#123456")
      session$flushReact()
      stk$edit <- list(row = "s1", col = "blocks", val = "a")
      session$flushReact()

      staged <- stk$add[["s1"]]
      expect_identical(stack_name(staged), "My Stack")
      expect_identical(stack_color(staged), "#123456")
      expect_identical(stack_blocks(staged), "a")

      session$setInputs(modify_stacks = 1)

      res <- update()$stacks

      expect_s3_class(res$add, "stacks")
      expect_named(res$add, "s1")
      expect_null(res$rm)
      expect_null(res$mod)

      expect_identical(stk$add, stacks())
      expect_null(session$returned)
    },
    args = list(
      board = board_args(
        blocks = c(
          a = new_dataset_block("iris"),
          b = new_subset_block()
        )
      ),
      update = reactiveVal()
    )
  )

  testServer(
    blk_ext_srv,
    {
      session$flushReact()

      expect_length(stk$obs, 1L)
      expect_named(stk$obs, "s1")
      expect_named(stk$obs[["s1"]], c("name", "color", "blocks"))

      for (x in stk$obs[["s1"]]) {
        expect_s3_class(x, "Observer")
      }

      session$setInputs(s1_name = "Renamed")
      expect_identical(
        stk$edit,
        list(row = "s1", col = "name", val = "Renamed")
      )
      expect_true("s1" %in% names(stk$mod))

      session$setInputs(s1_color = "#00ff00")
      expect_identical(
        stk$edit,
        list(row = "s1", col = "color", val = "#00ff00")
      )

      session$setInputs(s1_blocks = c("a", "b"))
      expect_identical(stk$edit$col, "blocks")
      expect_setequal(stk$edit$val, c("a", "b"))

      session$setInputs(modify_stacks = 1)

      res <- update()$stacks

      expect_null(res$add)
      expect_null(res$rm)
      expect_named(res$mod, "s1")

      payload <- res$mod[["s1"]]
      expect_false(is_stack(payload))
      expect_named(payload, c("name", "blocks", "color"))
      expect_identical(payload$name, "Renamed")
      expect_identical(payload$color, "#00ff00")
      expect_setequal(payload$blocks, c("a", "b"))
    },
    args = list(
      board = board_args(
        blocks = c(
          a = new_dataset_block("iris"),
          b = new_subset_block()
        ),
        stacks = stacks(s1 = new_dock_stack(blocks = "a", name = "One"))
      ),
      update = reactiveVal()
    )
  )

  testServer(
    blk_ext_srv,
    {
      session$flushReact()

      expect_identical(stk$rm, character())

      session$setInputs(stacks_dt_rows_selected = 1, rm_stack = 1)

      expect_identical(stk$rm, "s1")

      session$setInputs(modify_stacks = 1)

      res <- update()$stacks

      expect_null(res$add)
      expect_null(res$mod)
      expect_identical(res$rm, "s1")

      expect_identical(stk$rm, character())
      expect_identical(stk$curr, stacks())

      expect_null(session$returned)
    },
    args = list(
      board = board_args(
        blocks = c(
          a = new_dataset_block("iris"),
          b = new_subset_block()
        ),
        stacks = stacks(s1 = new_dock_stack(blocks = "a", name = "One"))
      ),
      update = reactiveVal()
    )
  )
})

test_that("dummy edit extension ui test", {

  ui <- blk_ext_ui(
    "test",
    new_dock_board(blocks = c(a = new_dataset_block()))
  )

  expect_s3_class(ui, "shiny.tag")
  expect_identical(htmltools::tagGetAttribute(ui, "class"), "blockr-ext-edit")
})
