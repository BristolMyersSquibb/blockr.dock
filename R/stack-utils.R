# What the stack menus (action-stack.R) need beyond the board: a fresh stack
# id and the blocks in no stack.

seed_stack_id <- function(board) {
  seed_ids(safe_board_ids(board, board_stack_ids), 1L)
}

# Block ids on the board that are not currently a member of any stack.
# `available_stack_blocks()` computes exactly this when seeded
# with the board's block ids (its default seeds with stack ids instead).
stack_eligible_blocks <- function(board) {
  if (is.null(board)) return(character())
  available_stack_blocks(
    board,
    blocks = board_block_ids(board)
  )
}
