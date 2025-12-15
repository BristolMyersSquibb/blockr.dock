test_that("add link action", {

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        add_link_action(
          trigger = reactive("b"),
          board = reactiveValues(
            board = new_board(
              c(
                a = new_dataset_block("iris"),
                b = new_head_block()
              )
            )
          ),
          update = reactiveVal(list())
        )
      )
    },
    {
      session$flushReact()
      expect_length(update(), 0L)
    }
  )

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        add_link_action(
          trigger = reactive("a"),
          board = reactiveValues(
            board = new_board(
              c(
                a = new_dataset_block("iris"),
                b = new_head_block()
              )
            )
          ),
          update = reactiveVal(list())
        )
      )
    },
    {
      session$flushReact()
      expect_length(update(), 0L)

      session$setInputs(create_link = "a")
      expect_length(update(), 0L)

      session$setInputs(create_link = "b")
      expect_length(update(), 0L)

      session$setInputs(
        add_link_confirm = 1L,
        add_link_id = ""
      )

      expect_length(update(), 0L)

      session$setInputs(
        add_link_confirm = 1L,
        add_link_id = "test",
        add_link_input = "test"
      )

      expect_length(update(), 0L)

      session$setInputs(
        add_link_confirm = 1L,
        add_link_id = "test",
        add_link_input = "data"
      )

      upd <- update()

      expect_length(upd, 1L)
      expect_named(upd, "links")

      expect_length(upd$links, 1L)
      expect_named(upd$links, "add")

      expect_length(upd$links$add, 1L)
      expect_named(upd$links$add, "test")
      expect_s3_class(upd$links$add, "links")
    }
  )
})

test_that("remove link action", {

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        remove_link_action(
          trigger = reactive("ab"),
          board = reactiveValues(
            board = new_board(
              c(
                a = new_dataset_block("iris"),
                b = new_head_block()
              ),
              links = links(id = "ab", from = "a", to = "b")
            )
          ),
          update = reactiveVal(list())
        )
      )
    },
    {
      session$flushReact()

      upd <- update()

      expect_length(upd, 1L)
      expect_named(upd, "links")

      expect_length(upd$links, 1L)
      expect_named(upd$links, "rm")

      expect_length(upd$links$rm, 1L)
      expect_identical(upd$links$rm, "ab")
    }
  )
})
