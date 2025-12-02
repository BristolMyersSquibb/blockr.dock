is_zero_len <- function(x) {
  length(x) == 0L
}

is_empty <- function(x) {
  is_zero_len(x) || all(is.na(x) | !nchar(x))
}

filter_empty <- function(x) Filter(Negate(is_empty), x)

last <- function(x) x[[length(x)]]

#' Stack color
#'
#' While stacks created via [blockr.core::new_stack()] do not keep track of
#' a color attribute, there are stack objects that inherit from "stack" and
#' do so. Such objects may implement the generic `stack_color()` in order to
#' provide a mechanism to retrieve a color value.
#'
#' @param x Stack object
#'
#' @return A character vector of (hex encoded) color value(s).
#'
#' @rdname color
#' @export
stack_color <- function(x) {
  UseMethod("stack_color")
}

#' @export
stack_color.stack <- function(x) {
  NA_character_
}

#' @export
stack_color.stacks <- function(x) {
  chr_ply(x, stack_color)
}

#' @export
stack_color.board <- function(x) {
  stack_color(board_stacks(x))
}

#' @param colors Currently used color values
#' @param n Number of new colors to generate
#' @rdname color
#' @export
suggest_new_colors <- function(colors = character(), n = 1) {

  color_fun <- blockr_option("stack_color", next_color)

  stopifnot(is.function(color_fun), is.character(colors), is_count(n))

  res <- character()

  for (i in seq_len(n)) {
    res <- c(res, color_fun(c(colors, res)))
  }

  res
}

next_color <- function(colors = character(), lum_var = TRUE) {

  if (!pkg_avail("colorspace")) {
    blockr_abort(
      "Package 'colorspace' is required.",
      class = "colorspace_not_available"
    )
  }

  if (length(colors)) {

    if (!pkg_avail("methods")) {
      blockr_abort(
        "Package 'methods' is required.",
        class = "methods_not_available"
      )
    }

    prev_hcl <- colorspace::coords(
      methods::as(colorspace::hex2RGB(colors), "polarLUV")
    )

    base_l <- mean(prev_hcl[, "L"], na.rm = TRUE)
    base_c <- mean(prev_hcl[, "C"], na.rm = TRUE)
    base_h <- mean(prev_hcl[, "H"], na.rm = TRUE)

  } else {

    base_l <- 65
    base_c <- 60
    base_h <- 0
  }

  # Golden angle in degrees
  golden_angle <- 137.508

  idx <- length(colors)

  # Compute hues via golden-angle rotation
  new_h <- (base_h + idx * golden_angle) %% 360

  # Optional gentle luminance modulation for visual distinction
  if (isTRUE(lum_var)) {
    new_l <- base_l + 10 * sin(idx * pi / 3)
  } else {
    new_l <- base_l
  }

  new_c <- base_c

  # Convert back to hex
  colorspace::hex(
    colorspace::polarLUV(L = new_l, C = new_c, H = new_h),
    fixup = TRUE
  )
}

create_block_with_name <- function(reg_id, blk_nms, ...) {
  name_fun <- function(nms) {
    function(class) {
      last(make.unique(c(nms, default_block_name(class)), sep = " "))
    }
  }

  create_block(reg_id, ..., block_name = name_fun(blk_nms))
}

new_stack_name <- function(board) {
  existsing <- chr_ply(board_stacks(board), stack_name)
  last(make.unique(c(existsing, default_stack_name()), sep = " "))
}

available_stack_blocks <- function(board) {

  stacks <- board_stacks(board)
  blocks <- board_blocks(board)

  blk_ids <- names(blocks)

  stacked_blocks <- unlst(
    lapply(stacks, stack_blocks)
  )

  blk_ids[!(blk_ids %in% stacked_blocks)]
}
