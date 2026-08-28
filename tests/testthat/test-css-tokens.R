# Every `var(--blockr-*, literal)` in this package's stylesheets is a second
# copy of the palette, and the literal is what renders wherever a sheet loads
# without the token block -- reachable, since `inputs_menu_dep()` and the
# other sidebar dependencies attach independently of `board_ui()`. Nothing
# else compares a fallback against the token it backs, so a copy-pasted
# literal drifts from the palette in silence. The vocabulary itself lives in
# blockr.ui, which owns it for the whole stack.

css_files <- function() {
  dir(
    system.file("assets", "css", package = "blockr.dock", mustWork = TRUE),
    pattern = "\\.css$",
    full.names = TRUE
  )
}

js_files <- function() {
  dir(
    system.file("assets", "js", package = "blockr.dock", mustWork = TRUE),
    pattern = "\\.js$",
    full.names = TRUE
  )
}

newline_pad <- function(x) {
  strrep("\n", nchar(gsub("[^\n]", "", x)))
}

# Comments are blanked rather than dropped, so that the offsets a match
# reports still count the lines the file actually has.
css_text <- function(path) {

  css <- paste(readLines(path, warn = FALSE), collapse = "\n")
  comment <- gregexpr("(?s)/\\*.*?\\*/", css, perl = TRUE)

  regmatches(css, comment) <- lapply(regmatches(css, comment), newline_pad)

  css
}

root_declarations <- function(path) {

  css <- css_text(path)

  root <- unlst(
    regmatches(css, gregexpr("(?s):root\\s*\\{.*?\\n\\}", css, perl = TRUE))
  )

  unlst(regmatches(root, gregexpr("--blockr-[a-z0-9-]+\\s*:[^;]+", root)))
}

token_values <- function() {

  decl <- root_declarations(
    system.file(
      "assets", "css", "blockr-tokens.css",
      package = "blockr.ui",
      mustWork = TRUE
    )
  )

  set_names(
    trimws(sub("^[^:]+:", "", decl)),
    trimws(sub(":.*$", "", decl))
  )
}

resolve_token <- function(value, tokens) {

  ref <- sub("^var\\((--blockr-[a-z0-9-]+)\\)$", "\\1", value)

  if (identical(ref, value) || !ref %in% names(tokens)) {
    return(value)
  }

  resolve_token(tokens[[ref]], tokens)
}

# A fallback may itself be a function call, so the closing parenthesis is the
# one that returns to the depth the `var(` opened from, not the next `)`.
closing_paren <- function(open, depth) {
  open + which(depth[seq.int(open + 1L, length(depth))] == depth[open] - 1L)[1L]
}

fallback_sites <- function(path) {

  css <- css_text(path)
  hit <- gregexpr("var\\((--blockr-[a-z0-9-]+)\\s*,", css, perl = TRUE)[[1L]]

  if (identical(as.integer(hit), -1L)) {
    return(NULL)
  }

  chars <- strsplit(css, "", fixed = TRUE)[[1L]]
  depth <- cumsum((chars == "(") - (chars == ")"))
  name <- attr(hit, "capture.start")[, 1L]

  data.frame(
    site = paste0(
      basename(path), ":",
      findInterval(hit, unlst(gregexpr("\n", css, fixed = TRUE))) + 1L
    ),
    token = substring(css, name, name + attr(hit, "capture.length")[, 1L] - 1L),
    fallback = trimws(
      substring(
        css,
        hit + attr(hit, "match.length"),
        int_ply(hit + 3L, closing_paren, depth = depth) - 1L
      )
    )
  )
}

all_fallback_sites <- function() {
  do.call(rbind, lapply(css_files(), fallback_sites))
}

package_source <- function() {
  c(
    unlst(eapply(asNamespace("blockr.dock"), deparse)),
    unlst(lapply(js_files(), readLines, warn = FALSE))
  )
}

# Written rather than read: an R string building a declaration, or a JS
# `setProperty()` naming the token. A `var()` read spells the name with a
# closing parenthesis after it, and does not count.
written_by_package <- function(token, src) {
  any(grepl(paste0(token, "[:\"]"), src))
}

test_that("every var() fallback carries the value its token resolves to", {

  tokens <- token_values()
  sites <- all_fallback_sites()

  expect_gt(length(tokens), 0L)
  expect_gt(nrow(sites), 0L)

  backed <- sites[sites$token %in% names(tokens), ]
  want <- chr_ply(tokens[backed$token], resolve_token, tokens = tokens)

  expect_identical(
    paste0(
      backed$site, ": ", backed$token, " falls back to ", backed$fallback,
      ", token resolves to ", want
    )[backed$fallback != want],
    character()
  )
})

test_that("a var() fallback only stands in for a token something sets", {

  # An unbacked name is not a fallback at all, it is the value: no `:root`
  # declares it and nothing writes it, so the literal always wins and the
  # palette never reaches the rule.
  unbacked <- setdiff(all_fallback_sites()$token, names(token_values()))
  src <- package_source()

  expect_identical(
    unbacked[!lgl_ply(unbacked, written_by_package, src = src)],
    character()
  )
})
