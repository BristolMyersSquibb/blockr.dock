bridge_board <- function(blocks, from, to, input) {
  new_board(
    blocks = blocks,
    links = links(from = from, to = to, input = input)
  )
}

bridge_df <- function(x) {
  res <- data.frame(from = x$from, to = x$to, input = x$input)
  res <- res[order(res$to, res$input), , drop = FALSE]
  rownames(res) <- NULL
  res
}

# Apply a removal the way blockr.core does in one update: augment (cascade the
# incident links), then apply. Errors if the result is not a valid board, which
# includes a cycle.
apply_removal <- function(board, ids) {
  upd <- augment_board_update(
    remove_blocks_update(board, ids),
    board,
    session = NULL
  )
  apply_board_update(board, upd, session = NULL)
}

test_that("a -> b -> c: removing b links a to c", {

  brd <- bridge_board(
    c(a = new_dataset_block(), b = new_head_block(), c = new_head_block()),
    from = c("a", "b"), to = c("b", "c"), input = c("data", "data")
  )

  expect_true(bridges_block(brd, "b"))
  expect_false(bridges_block(brd, "a"))

  res <- bridge_links(brd, "b")

  expect_s3_class(res, "links")
  expect_identical(
    bridge_df(res),
    data.frame(from = "a", to = "c", input = "data")
  )

  new <- apply_removal(brd, "b")

  expect_setequal(board_block_ids(new), c("a", "c"))
  expect_identical(
    bridge_df(board_links(new)),
    data.frame(from = "a", to = "c", input = "data")
  )
  expect_true(is_acyclic(new))
})

test_that("one input, three outputs: every output moves to the parent", {

  brd <- bridge_board(
    c(
      a = new_dataset_block(), b = new_head_block(),
      c = new_head_block(), d = new_head_block(), e = new_head_block()
    ),
    from = c("a", "b", "b", "b"),
    to = c("b", "c", "d", "e"),
    input = rep("data", 4L)
  )

  res <- bridge_links(brd, "b")

  expect_identical(
    bridge_df(res),
    data.frame(from = "a", to = c("c", "d", "e"), input = "data")
  )

  new <- apply_removal(brd, "b")
  expect_setequal(board_links(new)$to, c("c", "d", "e"))
  expect_true(all(board_links(new)$from == "a"))
  expect_true(is_acyclic(new))
})

test_that("a join (two inputs) is not bridged", {

  brd <- bridge_board(
    c(
      a = new_dataset_block(), b = new_dataset_block(),
      j = new_merge_block(by = "x"), c = new_head_block()
    ),
    from = c("a", "b", "j"), to = c("j", "j", "c"),
    input = c("x", "y", "data")
  )

  expect_false(bridges_block(brd, "j"))
  expect_length(bridge_links(brd, "j"), 0L)
  expect_s3_class(bridge_links(brd, "j"), "links")

  upd <- remove_blocks_update(brd, "j")
  expect_named(upd, "blocks")

  new <- apply_removal(brd, "j")
  expect_length(board_links(new), 0L)
})

test_that("a block without inputs is not bridged", {

  brd <- bridge_board(
    c(a = new_dataset_block(), b = new_head_block()),
    from = "a", to = "b", input = "data"
  )

  expect_false(bridges_block(brd, "a"))
  expect_length(bridge_links(brd, "a"), 0L)
})

test_that("variadic target inputs are kept", {

  brd <- bridge_board(
    c(
      a = new_dataset_block(), x = new_dataset_block(),
      b = new_head_block(), r = new_rbind_block()
    ),
    from = c("a", "b", "x"), to = c("b", "r", "r"),
    input = c("data", "1", "2")
  )

  res <- bridge_links(brd, "b")

  expect_identical(
    bridge_df(res),
    data.frame(from = "a", to = "r", input = "1")
  )

  new <- apply_removal(brd, "b")
  expect_identical(
    bridge_df(board_links(new)),
    data.frame(from = c("a", "x"), to = "r", input = c("1", "2"))
  )
})

test_that("a -> b -> c -> d: removing b and c links a to d", {

  brd <- bridge_board(
    c(
      a = new_dataset_block(), b = new_head_block(),
      c = new_head_block(), d = new_head_block()
    ),
    from = c("a", "b", "c"), to = c("b", "c", "d"),
    input = rep("data", 3L)
  )

  for (ids in list(c("b", "c"), c("c", "b"))) {
    expect_identical(
      bridge_df(bridge_links(brd, ids)),
      data.frame(from = "a", to = "d", input = "data")
    )
  }

  new <- apply_removal(brd, c("b", "c"))
  expect_identical(
    bridge_df(board_links(new)),
    data.frame(from = "a", to = "d", input = "data")
  )
  expect_true(is_acyclic(new))
})

test_that("a removed parent with two inputs stops the walk", {

  # a, b -> j (join) -> c -> d; removing j and c: c's output would walk up to
  # j, which has two inputs, so d is not bridged.
  brd <- bridge_board(
    c(
      a = new_dataset_block(), b = new_dataset_block(),
      j = new_merge_block(by = "x"), c = new_head_block(), d = new_head_block()
    ),
    from = c("a", "b", "j", "c"), to = c("j", "j", "c", "d"),
    input = c("x", "y", "data", "data")
  )

  expect_length(bridge_links(brd, c("j", "c")), 0L)

  new <- apply_removal(brd, c("j", "c"))
  expect_length(board_links(new), 0L)
})

test_that("an existing link is not added again", {

  # A valid board holds one link per target input, so the duplicate can only
  # show up on raw link vectors (or unresolved empty inputs).
  res <- bridge_plan(
    from = c("a", "b", "a"),
    to = c("b", "c", "c"),
    input = c("data", "data", "data"),
    ids = "b"
  )
  expect_identical(nrow(res), 0L)

  # The same output listed twice yields one link.
  res <- bridge_plan(
    from = c("a", "b", "b"),
    to = c("b", "c", "c"),
    input = c("data", "data", "data"),
    ids = "b"
  )
  expect_identical(nrow(res), 1L)
})

test_that("no self-links and no cycles", {

  # A cycle cannot exist on a board; on raw vectors the walk stops rather than
  # looping, and a bridge back into the parent is dropped.
  res <- bridge_plan(
    from = c("a", "b"), to = c("b", "a"), input = c("data", "data"),
    ids = "b"
  )
  expect_identical(nrow(res), 0L)

  res <- bridge_plan(
    from = c("b", "c"), to = c("c", "b"), input = c("data", "data"),
    ids = c("b", "c")
  )
  expect_identical(nrow(res), 0L)

  # A diamond: a -> b -> d, a -> c -> d (d a join). Removing b links a into
  # d's first input, next to the a -> c -> d path; the board stays acyclic.
  brd <- bridge_board(
    c(
      a = new_dataset_block(), b = new_head_block(), c = new_head_block(),
      d = new_merge_block(by = "x")
    ),
    from = c("a", "a", "b", "c"), to = c("b", "c", "d", "d"),
    input = c("data", "data", "x", "y")
  )

  new <- apply_removal(brd, "b")
  expect_true(is_acyclic(new))
  expect_identical(
    bridge_df(board_links(new)),
    data.frame(
      from = c("a", "a", "c"), to = c("c", "d", "d"),
      input = c("data", "x", "y")
    )
  )
})

test_that("remove block action bridges a middle block", {

  r_board <- reactiveValues(
    board = bridge_board(
      c(a = new_dataset_block(), b = new_head_block(), c = new_head_block()),
      from = c("a", "b"), to = c("b", "c"), input = c("data", "data")
    )
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        remove_block_action(
          trigger = reactive("b"), board = r_board, update = r_update
        )
      )
    },
    {
      session$flushReact()

      upd <- r_update()
      expect_named(upd, c("blocks", "links"))
      expect_identical(upd$blocks$rm, "b")
      expect_identical(
        bridge_df(upd$links$add),
        data.frame(from = "a", to = "c", input = "data")
      )

      new <- apply_board_update(
        r_board$board,
        augment_board_update(upd, r_board$board, session = NULL),
        session = NULL
      )
      expect_identical(
        bridge_df(board_links(new)),
        data.frame(from = "a", to = "c", input = "data")
      )
    }
  )
})

test_that("edit extension multi-select remove bridges through the chain", {

  testServer(
    blk_ext_srv,
    {
      session$setInputs(block_select = c("b", "c"), confirm_rm = 1)
      session$flushReact()

      upd <- update()
      expect_setequal(upd$blocks$rm, c("b", "c"))
      expect_identical(
        bridge_df(upd$links$add),
        data.frame(from = "a", to = "d", input = "data")
      )
    },
    args = list(
      board = board_args(
        blocks = c(
          a = new_dataset_block("iris"),
          b = new_head_block(),
          c = new_head_block(),
          d = new_head_block()
        ),
        links = links(
          from = c("a", "b", "c"), to = c("b", "c", "d"),
          input = rep("data", 3L)
        )
      ),
      update = reactiveVal()
    )
  )
})
