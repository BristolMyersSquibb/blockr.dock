# Temporary diagnostic for the intermittent e2e "did not become stable"
# failures. shinytest2 evaluates its init readiness / idle checks through
# `chromote_eval()` and collapses any browser-side exception into one opaque
# message, so a CI failure tells us nothing actionable. Trace that function's
# exit: whenever one of those checks returns a JS exception, print the real
# exception text and a snapshot of jQuery / readiness / page state at that
# instant. Fires only on failure; no behavioural change. Remove once a failing
# run has been captured.

if (requireNamespace("shinytest2", quietly = TRUE)) {

  install_idle_trace <- function() {
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
          message("----------------------------------------------------")
        }
      })
    )
  }

  tryCatch(
    suppressMessages(install_idle_trace()),
    error = function(e) {
      message("[idle-debug] trace not installed: ", conditionMessage(e))
    }
  )

  withr::defer(
    suppressMessages(suppressWarnings(
      untrace("chromote_eval", where = asNamespace("shinytest2"))
    )),
    teardown_env()
  )
}
