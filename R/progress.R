#' Show/Hide a Full-Page Progress Overlay
#'
#' Displays (or hides) a full-page loading overlay with an animated progress
#' bar in a running \code{\link{tablerApp}}. Useful for indicating that a
#' long-running computation (e.g. a database query) is in progress.
#'
#' @details
#' This is a dependency-free replacement for ad-hoc \pkg{waiter}/custom
#' JS overlay setups. It works the same way as
#' \code{\link{show}}/\code{\link{hide}}: the server sends a small message
#' over the WebSocket connection and the browser-side JS
#' (\code{tabler-progress.js}) creates/reveals or hides the overlay.
#'
#' \code{\link{tablerApp}}'s event loop only writes queued WebSocket messages
#' to the socket in between iterations of its
#' \code{repeat \{ tinyhttpserver::service(); ... \}} loop. If \code{showProgress()}
#' is immediately followed by a long, blocking computation in the same
#' observer, the "show" message sits in the queue - never reaching the
#' browser - until that observer (and thus the slow computation) finishes,
#' making the overlay flash on right at the end instead of before the work
#' starts. Calling \code{tinyhttpserver::service()} again from inside the observer to
#' force an early flush is unsafe here: it re-enters \pkg{httpuv}'s event
#' loop while it is still dispatching the current WebSocket message, which
#' can cause that message (and the input it carries) to be processed more
#' than once. Use \code{\link{withProgress}} instead, which defers the slow
#' computation with \code{later2::later()} so control returns to the event
#' loop (flushing the "show" message) before the work actually runs.
#'
#' @param session The \code{session} object passed by \code{\link{tablerApp}}
#'   to the server function. Defaults to \code{\link{getDefaultReactiveDomain}()}.
#' @param text The message displayed above the progress bar.
#' @return Invisibly, \code{NULL}.
#' @seealso \code{\link{tablerApp}}, \code{\link{withProgress}}
#' @examples
#' if (interactive()) {
#'   ui <- page(
#'     title = "Progress Example",
#'     body = body(actionButton("btn", "Load data"))
#'   )
#'
#'   server <- function(input, output, session) {
#'     observeEvent(input$btn, {
#'       withProgress(session, "Loading data...", {
#'         Sys.sleep(2)
#'       })
#'     })
#'   }
#'
#'   tablerApp(ui, server)
#' }
#' @export
showProgress <- function(session = getDefaultReactiveDomain(), text = "Loading...") {
  if (!is.list(session) || !is.function(session[["sendCustomMessage"]])) {
    warning("showProgress() requires a tablerApp session object - ignoring", call. = FALSE)
    return(invisible(NULL))
  }
  session$sendCustomMessage("tabler-showProgress", list(text = text))
  invisible(NULL)
}

#' @rdname showProgress
#' @export
hideProgress <- function(session = getDefaultReactiveDomain()) {
  if (!is.list(session) || !is.function(session[["sendCustomMessage"]])) {
    warning("hideProgress() requires a tablerApp session object - ignoring", call. = FALSE)
    return(invisible(NULL))
  }
  session$sendCustomMessage("tabler-hideProgress", list())
  invisible(NULL)
}

#' Run a Long Computation with a Progress Overlay
#'
#' Shows the full-page progress overlay (see \code{\link{showProgress}}),
#' then runs \code{expr}, then hides the overlay again - but unlike calling
#' \code{\link{showProgress}}/\code{\link{hideProgress}} directly around a
#' blocking computation, \code{expr} is deferred with \code{later2::later()}
#' so that \code{\link{tablerApp}}'s event loop gets a chance to actually
#' flush the "show" message to the browser before the slow work begins.
#'
#' @param session The \code{session} object passed by \code{\link{tablerApp}}
#'   to the server function. Defaults to \code{\link{getDefaultReactiveDomain}()}.
#' @param text The message displayed above the progress bar.
#' @param expr The (potentially slow) expression to run once the overlay is visible.
#' @return Invisibly, \code{NULL}. \code{expr} runs asynchronously, after
#'   \code{withProgress()} itself has already returned.
#' @seealso \code{\link{showProgress}}, \code{\link{tablerApp}}
#' @examples
#' if (interactive()) {
#'   ui <- page(
#'     title = "Progress Example",
#'     body = body(actionButton("btn", "Load data"))
#'   )
#'
#'   server <- function(input, output, session) {
#'     observeEvent(input$btn, {
#'       withProgress(session, "Loading data...", {
#'         Sys.sleep(2)
#'       })
#'     })
#'   }
#'
#'   tablerApp(ui, server)
#' }
#' @export
withProgress <- function(session = getDefaultReactiveDomain(), text = "Loading...", expr) {
  if (!is.list(session) || !is.function(session[["sendCustomMessage"]])) {
    warning("withProgress() requires a tablerApp session object - ignoring", call. = FALSE)
    return(invisible(NULL))
  }
  expr_q <- substitute(expr)
  env <- parent.frame()
  showProgress(session, text)
  later2::later(function() {
    tryCatch(
      eval(expr_q, env),
      error = function(e) {
        # This callback runs via later2::run_now() in tablerApp()'s event
        # loop, outside of .flush_domain()'s per-observer tryCatch (see
        # reactive.R). An uncaught error here would propagate out of
        # later2::run_now() and crash the whole event loop, silently freezing
        # the entire app - so it must be caught here instead.
        message("tabler: error in withProgress(): ", conditionMessage(e))
      },
      finally = hideProgress(session)
    )
  }, delay = 0)
  invisible(NULL)
}
