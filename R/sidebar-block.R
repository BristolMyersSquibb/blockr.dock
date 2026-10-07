append_to <- function(block_id = NULL) {
  new_bb_target("append", block_id)
}

prepend_to <- function(block_id = NULL) {
  new_bb_target("prepend", block_id)
}

# Unlike append / prepend, the id here is a LINK id: an insert is scoped to
# the wire it splits, and both of its endpoints are read off that link.
insert_into <- function(link_id = NULL) {
  new_bb_target("insert", link_id)
}

block_browser_server <- function(id, board, target = NULL) {
  stopifnot(is.character(id), length(id) == 1L, nzchar(id))

  target_fn <- as_accessor(target)

  moduleServer(
    id,
    function(input, output, session) {
      # `input$commit` is the binding's value; the `nonce` it carries makes
      # every add a fresh event, so eventReactive fires once per add. The
      # module validates the committed ids (a non-empty duplicate notifies
      # and `req()`s out), then returns a ready-to-apply value: a `blocks`
      # object for the add flow, or `list(blocks, links)` for append /
      # prepend - parity with link_menu_server() / stack_menu_server().
      eventReactive(
        input$commit,
        {
          spec <- input$commit
          spec[["nonce"]] <- NULL
          brd <- board()
          tgt <- target_fn()
          validate_block_spec(spec, brd, tgt, session)
          block_commit_value(spec, brd, tgt)
        },
        ignoreNULL = TRUE
      )
    }
  )
}

# Turn the committed spec into a ready-to-apply value. The add flow
# yields a `blocks` object (one id-keyed block); append / prepend yield
# `list(blocks, links)` with the link's input port resolved. Block
# construction and port resolution use blockr.core primitives only (no
# blockr.dock dependency), mirroring link_menu's commit value and the
# dock's former build_block_from_spec + block_input_select assembly.
block_commit_value <- function(spec, board, target) {
  blk <- build_browser_block(spec)
  blk_id <- resolve_browser_id(spec$id, board, board_block_ids)
  blocks <- as_blocks(set_names(list(blk), blk_id))

  if (target_mode(target) == "add") {
    return(blocks)
  }

  if (target$mode == "insert") {
    return(
      c(
        list(blocks = blocks),
        block_commit_insert(spec, board, target, blk, blk_id)
      )
    )
  }

  list(
    blocks = blocks,
    links = block_commit_link(spec, board, target, blk, blk_id)
  )
}

# Build the link that wires the new block to its target. Append: the new
# block (blk_id) receives from the source (target$id), so the port is a
# free slot on the NEW block. Prepend: the new block feeds INTO the
# target, so the port is a free slot on the TARGET block.
block_commit_link <- function(spec, board, target, blk, blk_id) {
  links <- safe_board_links(board)
  link_id <- resolve_browser_id(
    spec$link_id, board, board_link_ids
  )

  if (target$mode == "append") {
    input <- new_block_slot(spec, blk, blk_id, links)
    lnk <- new_link(from = target$id, to = blk_id, input = input)
  } else {
    tgt_blk <- board_block(board, target$id)
    input <- spec$target_input
    if (is.null(input) || !nzchar(input)) {
      input <- resolve_free_input(tgt_blk, target$id, links)
    }
    lnk <- new_link(from = blk_id, to = target$id, input = input)
  }

  as_links(set_names(list(lnk), link_id))
}

# The two links that put the new block into an existing wire, and where the
# far one goes.
#
# The near end lands on a free slot of the new block, like an append. The far
# end inherits the split link's input, which keeps a named entry's binding.
#
# That is not enough on its own: `sync_dot_args()` drops every key and re-adds
# them in link order, so an entry's argument position follows the board's link
# order whether it is named or blank. A link merely appended lands last, which
# slides every sibling after the split one up a place. So the far end is placed
# where the split link was, with `before`. The anchor resolves against the links
# as they are on entry, before `rm` is applied, which is what lets it name the
# very link this payload removes.
#
# Returning the placement rather than letting the action rebuild it keeps the
# decision with the part that made it: the menu is what knows which of the two
# links is the far end.
block_commit_insert <- function(spec, board, target, blk, blk_id) {

  ends <- link_ends(board, target$id)

  if (is.null(ends)) {
    return(list(links = as_links(list())))
  }

  input <- new_block_slot(spec, blk, blk_id, safe_board_links(board))

  ids <- insert_link_ids(spec, board)

  list(
    links = as_links(
      set_names(
        list(
          new_link(from = ends$from, to = blk_id, input = input),
          new_link(from = blk_id, to = ends$to, input = ends$input)
        ),
        c(ids$near, ids$far)
      )
    ),
    before = set_names(target$id, ids$far)
  )
}

# Ids for the two new links: whatever the user typed, else generated. Both are
# resolved against the board at once so a pair of blank fields cannot collide
# with each other.
insert_link_ids <- function(spec, board) {

  taken <- safe_board_ids(board, board_link_ids)
  out <- list(near = spec$near_link_id, far = spec$far_link_id)

  for (end in names(out)) {
    if (is.null(out[[end]]) || !nzchar(out[[end]])) {
      out[[end]] <- rand_names(old_names = taken, n = 1L)
    }
    taken <- c(taken, out[[end]])
  }

  out
}

# Which slot of the NEW block receives the incoming link: the user's pick
# where the panel offered one (>= 2 slots, or a variadic name), else its
# first free slot. Shared by append and insert, whose near end is the same
# problem.
new_block_slot <- function(spec, blk, blk_id, links) {

  input <- spec$block_input

  if (is.null(input) || !nzchar(input)) {
    return(resolve_free_input(blk, blk_id, links))
  }

  input
}

# Construct the block instance. The user's title (when supplied) is the
# block name; otherwise we let blockr.core derive the default name. The
# id - not the name - is what must be board-unique, and that is handled
# separately by resolve_browser_id().
build_browser_block <- function(spec) {
  if (!is.null(spec$title) && nzchar(spec$title)) {
    create_block(spec$type, block_name = spec$title)
  } else {
    create_block(spec$type)
  }
}

# An explicit, board-unique id is kept as-is; otherwise (empty field, or
# a collision) a fresh unique id is generated against the board.
resolve_browser_id <- function(spec_id, board, getter) {
  existing <- safe_board_ids(board, getter)
  if (id_available(spec_id, existing)) {
    spec_id
  } else {
    rand_names(old_names = existing, n = 1L)
  }
}

# Reject a non-empty committed id that collides with the board (block id
# always; link id for append / prepend). An empty id is valid - it means
# "assign one for me", resolved in block_commit_value().
validate_block_spec <- function(spec, board, target, session) {
  reject_collision(
    spec$id, safe_board_ids(board, board_block_ids),
    "block", session
  )
  if (target_mode(target) %in% c("append", "prepend")) {
    reject_collision(
      spec$link_id, safe_board_ids(board, board_link_ids),
      "link", session
    )
  }

  if (target_mode(target) == "insert") {
    taken <- safe_board_ids(board, board_link_ids)
    for (id in c(spec$near_link_id, spec$far_link_id)) {
      reject_collision(id, taken, "link", session)
    }
    # Two blank fields resolve to distinct generated ids, but two identical
    # typed ones would not.
    if (length(spec$near_link_id) && length(spec$far_link_id) &&
          nzchar(spec$near_link_id) &&
          identical(spec$near_link_id, spec$far_link_id)) {
      notify(
        "The two link IDs must differ.",
        type = "warning", session = session
      )
      req(FALSE)
    }
  }

  # A prepend into a variadic target may carry a user-supplied slot name;
  # core rejects two identically named inputs on one block. Appended
  # blocks are new, so their first input never collides.
  if (target_mode(target) == "prepend") {
    name <- spec$target_input
    if (!is.null(name) && nzchar(name)) {
      links <- safe_board_links(board)
      if (name %in% links[links$to == target$id]$input) {
        notify(
          "This input name is already used on the target block.",
          type = "warning", session = session
        )
        req(FALSE)
      }
    }
  }

  invisible(TRUE)
}

reject_collision <- function(id, existing, what, session) {
  if (!is.null(id) && nzchar(id) && id %in% existing) {
    notify(
      paste0("Please choose a valid ", what, " ID."),
      type = "warning", session = session
    )
    req(FALSE)
  }
  invisible(TRUE)
}

# The board's links, or an empty links object for a NULL board.
safe_board_links <- function(board) {
  if (is.null(board)) return(links())
  tryCatch(
    board_links(board),
    error = function(e) links()
  )
}

block_browser_dep <- function() {
  htmltools::htmlDependency(
    name = "sidebar-block",
    version = utils::packageVersion("blockr.dock"),
    package = "blockr.dock",
    src = "assets",
    stylesheet = "css/sidebar-block.css",
    script = "js/sidebar-block.js",
    all_files = FALSE
  )
}

# ---- target descriptor -------------------------------------------------

# `id` is a block id for append / prepend and a link id for insert; each
# mode's own branch knows which it is holding.
new_bb_target <- function(mode, id = NULL) {
  stopifnot(
    is.null(id) || (is.character(id) && length(id) == 1L && nzchar(id))
  )
  structure(
    list(mode = mode, id = id),
    class = c(paste0("bb_target_", mode), "bb_target")
  )
}

# The resulting flow: "add" when there's no target, else the target's
# own mode. The rest of the module branches on this string.
target_mode <- function(target) {
  if (is.null(target)) "add" else target$mode
}

# ---- panel assembly ----------------------------------------------------

# Build the per-block metadata list from the registry. Independent of
# board state: default block / link ids are no longer seeded here (the
# server resolves a unique id at commit), so the rendered markup is a
# pure function of the registry. Block-input slots are still computed
# for the append flow (each `safe_block_inputs()` instantiates a block,
# so we skip the work otherwise) to drive the linkable-block filter and
# the in-card port picker.
browser_block_metas <- function(mode) {
  registry <- available_blocks()
  metas <- lapply(seq_along(registry), function(i) {
    entry <- registry[[i]]
    # `type` is the registry uid (e.g. "dataset_block"), so consumers
    # can do `create_block(spec$type, ...)` rather than
    # rely on the constructor's function name being importable.
    list(
      type = names(registry)[[i]],
      name = entry_attr(entry, "name", names(registry)[[i]]),
      description = entry_attr(entry, "description", ""),
      category = entry_attr(entry, "category", ""),
      icon = entry_attr(entry, "icon", ""),
      package = entry_attr(entry, "package", "local"),
      ctor = entry
    )
  })

  need_inputs <- mode %in% c("append", "insert")

  # For append and insert, the new block has to receive a link from the
  # source, so candidates need either a named input slot or variadic arity
  # (`NA`)
  # which accepts arbitrary fresh slots. Source-only blocks (arity 0,
  # e.g. dataset_block) can't be appended and are filtered out.
  # Variadic blocks (e.g. rbind_block) return character(0) from
  # block_inputs() but DO accept links - the server generates a fresh
  # slot name. Prepend / add are unfiltered.
  if (need_inputs) {
    for (i in seq_along(metas)) {
      metas[[i]]$inputs <- safe_block_inputs(metas[[i]]$ctor)
      metas[[i]]$variadic <- safe_block_variadic(metas[[i]]$ctor)
    }
    metas <- Filter(
      function(m) length(m$inputs) > 0L || isTRUE(m$variadic),
      metas
    )
  } else {
    for (i in seq_along(metas)) {
      metas[[i]]$inputs <- character()
      metas[[i]]$variadic <- FALSE
    }
  }

  metas
}

# The link's two ends plus the slot it lands on, or NULL when the id names
# no link on the board. One lookup, so callers cannot disagree about it.
link_ends <- function(board, link_id) {

  if (is.null(link_id) || !length(link_id) || is.na(link_id) ||
        !nzchar(link_id)) {
    return(NULL)
  }

  links <- as.data.frame(safe_board_links(board))

  if (!nrow(links) || !link_id %in% links$id) {
    return(NULL)
  }

  row <- links[links$id == link_id, ]

  list(from = row$from, to = row$to, input = row$input)
}

block_label <- function(board, id) {

  blk <- board_block(board, id)

  if (is.null(blk)) {
    return(id)
  }

  nm <- tryCatch(block_name(blk), error = function(e) NULL)

  if (is.null(nm) || !nzchar(nm)) id else nm
}

# Shared category block for the link and stack menus: the wrapper chrome
# is identical; only the per-entry card differs, so callers pass a
# `card_fn` that maps one meta to its card tag.
category_section <- function(category, entries, card_fn) {
  tags$div(
    class = "blockr-block-browser-category",
    `data-category` = category,
    tags$h3(category),
    tags$div(
      class = "blockr-block-browser-cards",
      lapply(entries, card_fn)
    )
  )
}

field_wrapper <- function(class_suffix, id, label, control) {
  tags$div(
    class = paste0(
      "blockr-block-browser-field blockr-block-browser-field-", class_suffix
    ),
    tags$label(`for` = id, label),
    control
  )
}

field_text <- function(class_suffix, id, label, value, placeholder = NULL) {
  field_wrapper(
    class_suffix, id, label,
    tags$input(
      type = "text",
      id = id,
      value = value,
      placeholder = placeholder
    )
  )
}

field_select <- function(class_suffix, id, label, options) {
  field_wrapper(
    class_suffix, id, label,
    tags$select(
      id = id,
      lapply(options, function(opt) {
        tags$option(value = opt, opt)
      })
    )
  )
}

chevron_icon <- function() {
  tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    viewBox = "0 0 16 16",
    fill = "currentColor",
    `aria-hidden` = "true",
    tags$path(
      d = paste0(
        "M1.646 4.646a.5.5 0 0 1 .708 0L8 10.293l5.646-5.647a.5.5 0 0 1",
        " .708.708l-6 6a.5.5 0 0 1-.708 0l-6-6a.5.5 0 0 1 0-.708z"
      )
    )
  )
}

# ---- small helpers -----------------------------------------------------

entry_attr <- function(entry, key, default) {
  val <- attr(entry, key, exact = TRUE)
  if (is.null(val) || (is.character(val) && !nzchar(val))) default else val
}

meta_category <- function(m) {
  if (nzchar(m$category)) m$category else "Uncategorized"
}

# Split metas into category buckets in display order, "Uncategorized"
# last. Returns a named list of meta groups, keyed and ordered by
# category; both the block-browser and stack-menu panels render from it.
category_groups <- function(metas) {
  cats <- vapply(metas, meta_category, character(1L))
  order <- unique(cats)
  order <- c(
    setdiff(order, "Uncategorized"),
    if ("Uncategorized" %in% order) "Uncategorized" else character()
  )
  split(metas, cats)[order]
}

# n unique ids avoiding `existing` (and each other); empty for n == 0.
seed_ids <- function(existing, n) {
  if (n > 0L) {
    rand_names(old_names = existing, n = n)
  } else {
    character()
  }
}

# Construct one block instance with no args to read its input slot names.
# Returns character(0) on any error.
safe_block_inputs <- function(ctor) {
  tryCatch(
    {
      blk <- ctor()
      block_inputs(blk)
    },
    error = function(e) character()
  )
}

# TRUE when the block has variadic arity (NA) - accepts an arbitrary
# number of input links with fresh slot names. False otherwise (arity
# 0 or finite). Used to keep variadic blocks as valid append targets
# even though `block_inputs()` returns character(0).
safe_block_variadic <- function(ctor) {
  tryCatch(
    is.na(block_arity(ctor())),
    error = function(e) FALSE
  )
}

safe_board_ids <- function(board, getter) {
  if (is.null(board)) return(character())
  tryCatch(getter(board), error = function(e) character())
}

board_block <- function(board, id) {
  if (is.null(board)) return(NULL)
  blocks <- tryCatch(board_blocks(board), error = function(e) NULL)
  if (is.null(blocks)) return(NULL)
  blocks[[id]]
}
