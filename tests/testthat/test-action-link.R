test_that("add link action", {

  al_action_1 <- add_link_action(reactive("b"))

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        al_action_1(
          board = reactiveValues(
            board = new_board(
              c(
                a = new_dataset_block("iris"),
                b = new_head_block()
              )
            )
          ),
          update = reactiveVal(list()),
          domain = MockShinySession$new()
        )
      )
    },
    {
      session$flushReact()
      expect_length(update(), 0L)
    }
  )

  al_action_2 <- add_link_action(reactive("a"))

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        al_action_2(
          board = reactiveValues(
            board = new_board(
              c(
                a = new_dataset_block("iris"),
                b = new_head_block()
              )
            )
          ),
          update = reactiveVal(list()),
          domain = MockShinySession$new()
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

  rl_action <- remove_link_action(reactive("ab"))

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        rl_action(
          board = reactiveValues(
            board = new_board(
              c(
                a = new_dataset_block("iris"),
                b = new_head_block()
              ),
              links = links(id = "ab", from = "a", to = "b")
            )
          ),
          update = reactiveVal(list()),
          domain = MockShinySession$new()
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
