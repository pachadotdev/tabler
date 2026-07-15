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
#' @param session The \code{session} object passed by \code{\link{tablerApp}}
#'   to the server function. Defaults to \code{\link{getDefaultReactiveDomain}()}.
#' @param text The message displayed above the progress bar.
#' @return Invisibly, \code{NULL}.
#' @seealso \code{\link{tablerApp}}
#' @examples
#' if (interactive()) {
#'   ui <- page(
#'     title = "Progress Example",
#'     body = body(actionButton("btn", "Load data"))
#'   )
#'
#'   server <- function(input, output, session) {
#'     observeEvent(input$btn, {
#'       showProgress(session, "Loading data...")
#'       Sys.sleep(2)
#'       hideProgress(session)
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
