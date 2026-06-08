# Temporary diagnostic for the intermittent e2e "did not become stable"
# failures. Two traces, both produce output only on failure; no behavioural
# change.
#
#   1. `app_init_browser_log()` runs once per init, just before the navigate
#      and with the chromote session in hand. Trace it to attach CDP listeners
#      that record the navigation/context lifecycle (frameNavigated,
#      executionContext created/destroyed/cleared, loadEventFired).
#
#   2. shinytest2 collapses any browser-side exception from its init readiness
#      / idle checks into one opaque "did not become stable". Trace
#      `chromote_eval()` on exit: when an init check returns a JS exception,
#      print the real exception text, a snapshot of jQuery / readiness / page
#      state, and the recorded lifecycle timeline -- the sequence a
#      point-in-time probe cannot show.
#
# Remove once a failing run has pinned the trigger.

if (requireNamespace("shinytest2", quietly = TRUE)) {

  lc <- new.env(parent = emptyenv())
  lc$events <- character()
  lc$t0 <- NULL
  options(blockr_idle_lifecycle = lc)

  install_traces <- function() {

    trace(
      "app_init_browser_log",
      where = asNamespace("shinytest2"),
      print = FALSE,
      exit = quote({
        lc <- getOption("blockr_idle_lifecycle")

        if (!is.null(lc)) {

          lc$events <- character()
          lc$t0 <- Sys.time()

          blank <- function(x) if (is.null(x)) "" else x
          rec <- function(line) {
            ms <- as.numeric(Sys.time() - lc$t0, units = "secs") * 1000
            lc$events <- c(lc$events, sprintf("%8.1fms  %s", ms, line))
          }
          sub <- function(ev, fn) ev(wait_ = FALSE, callback_ = fn)
          sess <- self$get_chromote_session()

          tryCatch({
            sess$Page$enable(wait_ = FALSE)
            sess$Runtime$enable(wait_ = FALSE)
            sub(sess$Page$frameNavigated, function(m) {
              rec(paste0("navigated url=", blank(m$frame$url)))
            })
            sub(sess$Page$loadEventFired, function(m) rec("loadEventFired"))
            sub(sess$Runtime$executionContextCreated, function(m) {
              rec(paste0("ctx created id=", m$context$id,
                         " origin=", blank(m$context$origin)))
            })
            sub(sess$Runtime$executionContextDestroyed, function(m) {
              rec(paste0("ctx destroyed id=", m$executionContextId))
            })
            sub(sess$Runtime$executionContextsCleared, function(m) {
              rec("ctx all cleared (navigation)")
            })
          }, error = function(e) {
            msg <- paste0("[attach failed: ", conditionMessage(e), "]")
            lc$events <- c(lc$events, msg)
          })
        }
      })
    )

    trace(
      "chromote_eval",
      where = asNamespace("shinytest2"),
      print = FALSE,
      exit = quote({
        rv <- returnValue()
        errored <- length(rv$exceptionDetails) > 0 ||
          identical(rv$result$subtype, "error")

        if (errored && grepl("shiny:busy|shinytest2\\.ready", js)) {

          probe <- function(expr) {
            tryCatch(
              chromote_session$Runtime$evaluate(
                expr, returnByValue = TRUE, timeout_ = 3000
              )$result$value,
              error = function(e) "<probe-failed>"
            )
          }

          exc <- rv$exceptionDetails$exception$description
          if (is.null(exc)) exc <- rv$exceptionDetails$text
          if (is.null(exc)) exc <- rv$result$description

          message("\n--- [idle-debug] init check threw a JS exception ---")
          message("js   : ", substr(gsub("\\s+", " ", js), 1, 80))
          message("exc  : ", exc)
          message(
            "$=", probe("typeof window.$"),
            " jQuery=", probe("typeof window.jQuery"),
            " $===jQuery=", probe("window.$ === window.jQuery")
          )
          message(
            "ready=", probe("(window.shinytest2||{}).ready"),
            " readyState=", probe("document.readyState"),
            " frames=", probe("frames.length")
          )
          message("href : ", probe("window.location.href"))

          lc <- getOption("blockr_idle_lifecycle")
          if (!is.null(lc) && length(lc$events)) {
            message("CDP nav/context timeline (from init, pre-navigate):")
            for (ev in lc$events) message("  ", ev)
          }
          message("----------------------------------------------------")
        }
      })
    )
  }

  tryCatch(
    suppressMessages(install_traces()),
    error = function(e) {
      message("[idle-debug] traces not installed: ", conditionMessage(e))
    }
  )

  withr::defer(
    suppressMessages(suppressWarnings({
      untrace("app_init_browser_log", where = asNamespace("shinytest2"))
      untrace("chromote_eval", where = asNamespace("shinytest2"))
    })),
    teardown_env()
  )
}
