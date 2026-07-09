# blk() / ext() are the panel-op currency; augment resolves refs and bare-id
# sugar into the internal named-hint form the reducer and delivery consume.
# These drive the resolution through the real update lifecycle
# (augment_board_update) and assert the canonical result.

resolve_mod <- function(board, mod) {
  aug <- augment_board_update(list(views = list(mod = list(V = mod))), board)
  aug$views$mod$V
}

test_that("blk() / ext() are typed refs that encode to canonical panel ids", {

  r <- blk("a", near = "b", side = "right")

  expect_true(is_panel_ref(r))
  expect_identical(as.character(r), "block_panel-a")
  expect_identical(as.character(ext("dag")), "ext_panel-dag")

  # Finding 4 is structural: a misspelled hint is caught at the call site.
  expect_error(blk("a", sise = 0.4), "unused argument")
})

test_that("augment resolves refs and bare ids to the canonical hint form", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    extensions = new_edit_board_extension(),
    views = list(V = "a")
  )

  # A typed ref on `add` resolves against the post-state and carries its hint;
  # `near` resolves against membership.
  add <- resolve_mod(
    brd, list(add = list(blk("b", near = "a", side = "right")))
  )
  expect_identical(
    add$add[["block_panel-b"]], list(near = "block_panel-a", side = "right")
  )

  # Bare id sugar, resolved block-first.
  expect_identical(
    names(resolve_mod(brd, list(add = list("b")))$add), "block_panel-b"
  )

  # A bare extension alias still resolves (no block by that name).
  eid <- "edit_board"
  ep <- paste0("ext_panel-", eid)
  expect_identical(names(resolve_mod(brd, list(add = list(eid)))$add), ep)
  expect_identical(names(resolve_mod(brd, list(add = list(ext(eid))))$add), ep)

  # rm / select take refs or bare ids and resolve against membership.
  expect_identical(resolve_mod(brd, list(rm = "a"))$rm, "block_panel-a")
  expect_identical(
    resolve_mod(brd, list(rm = list(blk("a"))))$rm, "block_panel-a"
  )
  expect_identical(resolve_mod(brd, list(select = "a"))$select, "block_panel-a")
})

test_that("resolution is idempotent and passes the canonical form through", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(V = "a")
  )

  # The internal named-hint form (gestures, block-removal cascade) is untouched.
  passed <- resolve_mod(brd, list(add = list(`block_panel-b` = list())))
  expect_identical(names(passed$add), "block_panel-b")

  once <- augment_board_update(
    list(views = list(mod = list(V = list(add = list(blk("b")))))), brd
  )
  twice <- augment_board_update(once, brd)
  expect_identical(once, twice)
})

test_that("hints are contextual and duplicates / clashes are loud", {

  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(), b = new_head_block(), c = new_head_block()
    ),
    views = list(V = c("a", "b"))
  )

  # `add` takes near / side / size; `move` near / side; `rm` / `select` none.
  expect_silent(resolve_mod(brd, list(add = list(blk("c", size = 0.4)))))
  expect_error(
    resolve_mod(brd, list(move = list(blk("a", size = 0.4)))),
    class = "dock_views_mod_hint_invalid"
  )
  expect_error(
    resolve_mod(brd, list(rm = list(blk("a", near = "b")))),
    class = "dock_views_mod_hint_invalid"
  )

  # The same panel twice in one batch with conflicting hints is a caller error.
  expect_error(
    resolve_mod(
      brd, list(add = list(blk("c", side = "left"), blk("c", side = "right")))
    ),
    class = "dock_views_mod_duplicate"
  )
})

test_that("a bare id in both namespaces is a loud clash", {

  # A block whose id collides with the extension's id: in the post-state
  # namespace (where `add` resolves) the bare form is ambiguous, so it errors
  # and demands blk() / ext().
  clash <- "edit_board"
  brd <- new_dock_board(
    blocks = set_names(list(new_head_block()), clash),
    extensions = new_edit_board_extension(),
    views = list(V = character())
  )

  expect_error(
    resolve_mod(brd, list(add = list(clash))),
    class = "dock_views_mod_ref_clash"
  )

  # A typed ref disambiguates.
  add <- resolve_mod(brd, list(add = list(ext(clash))))
  expect_identical(names(add$add), paste0("ext_panel-", clash))
})

test_that("blk() / ext() are accepted in the layout-authoring DSL", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    extensions = list(dag = new_edit_board_extension()),
    views = list(V = list(ext("dag"), blk("a"), "b")),
    grids = list(V = dock_grid(ext("dag"), panels(blk("a"), "b")))
  )

  expect_setequal(
    view_members(board_views(brd)[["V"]]),
    c("ext_panel-dag", "block_panel-a", "block_panel-b")
  )

  # A placement hint is meaningless where no add / move happens.
  expect_error(
    panels(blk("a", side = "left")),
    class = "dock_layout_ref_hint"
  )
})
