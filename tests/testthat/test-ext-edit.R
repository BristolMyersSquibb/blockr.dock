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

test_that("dummy edit extension ui test", {

  ui <- blk_ext_ui(
    "test",
    new_dock_board(blocks = c(a = new_dataset_block()))
  )

  expect_s3_class(ui, "shiny.tag.list")
  expect_length(ui, 4L)
})
