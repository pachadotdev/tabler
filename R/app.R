# tablerApp — standalone httpuv-based application runner.
# Uses httpuv for HTTP + WebSocket and jsonlite for
# the message protocol.

# ---------------------------------------------------------------------------
# MIME type table
# ---------------------------------------------------------------------------
.mime_type <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    html  = "text/html; charset=utf-8",
    css   = "text/css",
    js    = "application/javascript",
    json  = "application/json",
    png   = "image/png",
    jpg   =,
    jpeg  = "image/jpeg",
    gif   = "image/gif",
    svg   = "image/svg+xml",
    ico   = "image/x-icon",
    woff  = "font/woff",
    woff2 = "font/woff2",
    ttf   = "font/ttf",
    eot   = "application/vnd.ms-fontobject",
    otf   = "font/otf",
    "application/octet-stream"
  )
}

# ---------------------------------------------------------------------------
# Input proxy — a reactiveValues() store; $.ReactiveValues handles reactive
# reads so no custom S3 method for $ is needed here.
# ---------------------------------------------------------------------------

# Internal helper: set a reactive input value without going through $<-
.rv_set <- function(rv, name, value) {
  store   <- attr(rv, ".store")
  sources <- attr(rv, ".sources")
  assign(name, value, envir = store)
  if (!exists(name, envir = sources, inherits = FALSE)) {
    assign(name, new_source(), envir = sources)
  }
  get(name, envir = sources)$invalidate_dependents()
}

# ---------------------------------------------------------------------------
# Output proxy — a plain environment; R's built-in env $<- stores render objects
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# tablerApp
# ---------------------------------------------------------------------------

#' @title Run a Tabler Application
#' @description Launches a self-contained web application using \pkg{httpuv} for
#'   HTTP and WebSocket transport.
#'
#' @param ui        The UI definition — typically a call to \code{page()}.
#' @param server    A function with signature \code{function(input, output, session)}.
#' @param host      Host to listen on (default \code{"127.0.0.1"}).
#' @param port      Port number (default \code{3000L}).
#' @param launch.browser Open a browser automatically when in an interactive session.
#'
#' @details
#' \strong{Protocol}
#'
#' The browser and server exchange plain JSON messages over a WebSocket at
#' \code{/ws}:
#' \itemize{
#'   \item Browser → R: \code{\{"type":"input","name":"<id>","value":<v>\}}
#'   \item R → Browser: \code{\{"type":"output","id":"<id>","html":"<escaped HTML>"\}}
#' }
#'
#' \strong{Reactive system}
#'
#' Uses tabler's own dependency-tracking reactive system (see \code{\link{reactive}},
#' \code{\link{observe}}, \code{\link{reactiveVal}}, \code{\link{observeEvent}}).
#' Render functions (\code{renderText}, \code{renderUI}, \code{renderPrint}) are
#' assigned to \code{output} inside \code{server}.
#'
#' @return Invisibly, after the server is stopped.
#' @export
tablerApp <- function(ui, server, host = "127.0.0.1", port = 3000L,
                      launch.browser = interactive()) {
  # Reset the global reactive domain so stale observers from a previous run
  # do not interfere with this new session.
  .domain$pending       <- list()
  .domain$flushing      <- FALSE
  .domain$context_stack <- list()

  # Build the static page HTML once ----
  page_html <- paste0("<!DOCTYPE html><html>", render_html(ui), "</html>")

  # Shared app state ----
  connections  <- new.env(hash = TRUE, parent = emptyenv())  # id -> ws
  output_cache <- new.env(hash = TRUE, parent = emptyenv())  # id -> html string
  widget_store <- new.env(hash = TRUE, parent = emptyenv())  # id -> widget HTML

  # Input / output proxies ----
  input_proxy  <- reactiveValues()          # $.ReactiveValues gives reactive reads
  output_proxy <- new.env(parent = emptyenv())  # plain env; $<- is standard env assign

  # Session object (minimal) ----
  send_all <- function(msg_json) {
    for (ws in as.list(connections)) {
      tryCatch(ws$send(msg_json), error = function(e) NULL)
    }
  }

  session <- list(
    sendCustomMessage = function(type, message) {
      send_all(jsonlite::toJSON(
        list(type = "custom", messageType = type, message = message),
        auto_unbox = TRUE
      ))
    },
    onSessionEnded = function(fn) invisible(NULL),
    close = function() invisible(NULL)
  )

  # Call server ----
  server(input_proxy, output_proxy, session)

  # Wire up output observers ----
  # send_fn is called by .observe_output whenever an output value changes
  make_send_fn <- function(output_id) {
    function(val, type) {
      if (identical(type, "widget") && inherits(val, "htmlwidget")) {
        # Save the self-contained widget HTML and serve it via HTTP so the
        # iframe loads it as a normal page (avoids srcdoc encoding issues).
        tmp <- tempfile(fileext = ".html")
        on.exit(unlink(tmp), add = TRUE)
        htmlwidgets::saveWidget(val, tmp, selfcontained = TRUE)
        raw <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
        assign(output_id, raw, envir = widget_store)
        html <- paste0(
          '<iframe src="/widgets/', output_id, '"',
          ' style="width:100%;height:100%;border:none;"',
          ' frameborder="0"></iframe>'
        )
      } else {
        html <- .serialise_output(val, type)
      }
      assign(output_id, html, envir = output_cache)
      send_all(jsonlite::toJSON(
        list(type = "output", id = output_id, html = html),
        auto_unbox = TRUE
      ))
    }
  }

  for (nm in ls(output_proxy)) {
    render_obj <- get(nm, envir = output_proxy)
    .observe_output(render_obj, make_send_fn(nm))
  }

  # Initial flush — populates output_cache before any browser connects ----
  .flush_domain()

  # HTTP handler ----
  http_handler <- function(req) {
    path <- req$PATH_INFO

    # Main page
    if (path == "/" || path == "" || path == "/index.html") {
      return(list(
        status  = 200L,
        headers = list("Content-Type" = "text/html; charset=utf-8"),
        body    = page_html
      ))
    }

    # Widget HTML endpoint — serve self-contained widget pages
    if (grepl("^/widgets/", path)) {
      wid <- sub("^/widgets/", "", path)   # regex mode so ^ anchors correctly
      # Reject any path tricks
      if (grepl("..", wid, fixed = TRUE) || grepl("/", wid, fixed = TRUE)) {
        return(list(status = 403L,
                    headers = list("Content-Type" = "text/plain"),
                    body    = "Forbidden"))
      }
      if (exists(wid, envir = widget_store, inherits = FALSE)) {
        return(list(
          status  = 200L,
          headers = list("Content-Type" = "text/html; charset=utf-8"),
          body    = get(wid, envir = widget_store)
        ))
      }
      return(list(status = 404L,
                  headers = list("Content-Type" = "text/plain"),
                  body    = "Widget not found"))
    }

    # Static assets — resolve via system.file() for path-traversal safety
    rel <- sub("^/+", "", path)

    # Block anything that looks dangerous before hitting system.file
    if (grepl("..", rel, fixed = TRUE) || grepl("^~", rel)) {
      return(list(status = 403L,
                  headers = list("Content-Type" = "text/plain"),
                  body = "Forbidden"))
    }

    file_path <- system.file(rel, package = "tabler")
    if (!nzchar(file_path) || !file.exists(file_path) || dir.exists(file_path)) {
      return(list(status = 404L,
                  headers = list("Content-Type" = "text/plain"),
                  body = "Not found"))
    }

    fsize   <- file.info(file_path)$size
    con     <- file(file_path, "rb")
    content <- readBin(con, "raw", fsize)
    close(con)

    list(
      status  = 200L,
      headers = list("Content-Type" = .mime_type(file_path)),
      body    = content
    )
  }

  # WebSocket handler ----
  ws_handler <- function(ws) {
    ws_id <- paste(sample(c(letters, 0:9), 12L, replace = TRUE), collapse = "")
    assign(ws_id, ws, envir = connections)

    # Send all cached outputs to the new connection
    for (oid in ls(output_cache)) {
      html <- get(oid, envir = output_cache)
      tryCatch(
        ws$send(jsonlite::toJSON(
          list(type = "output", id = oid, html = html),
          auto_unbox = TRUE
        )),
        error = function(e) NULL
      )
    }

    ws$onMessage(function(binary, message) {
      tryCatch({
        msg <- jsonlite::fromJSON(message, simplifyVector = FALSE)
        if (identical(msg[["type"]], "input")) {
          .rv_set(input_proxy, msg[["name"]], msg[["value"]])
          .flush_domain()
        }
      }, error = function(e) NULL)
    })

    ws$onClose(function() {
      if (exists(ws_id, envir = connections, inherits = FALSE)) {
        rm(list = ws_id, envir = connections)
      }
    })
  }

  # Start server ----
  srv <- httpuv::startServer(host, as.integer(port), list(
    call      = http_handler,
    onWSOpen  = ws_handler
  ))
  on.exit({
    # Notify all connected browsers the app is stopping, then give httpuv a
    # moment to flush the outgoing message before closing the socket.
    stop_msg <- jsonlite::toJSON(list(type = "stop"), auto_unbox = TRUE)
    for (.ws in as.list(connections)) {
      tryCatch(.ws$send(stop_msg), error = function(e) NULL)
    }
    # Catch interrupt as well: if Ctrl+C fired inside service() above, an
    # unhandled interrupt here would skip stopServer() and leave the port bound.
    tryCatch(httpuv::service(500L), error = function(e) NULL, interrupt = function(e) NULL)
    httpuv::stopServer(srv)
  }, add = TRUE)

  url <- sprintf("http://%s:%d", host, as.integer(port))
  message("Tabler app running at ", url, "\nPress Ctrl+C to stop.")

  if (isTRUE(launch.browser)) utils::browseURL(url)

  # Event loop ----
  tryCatch(
    repeat {
      httpuv::service(1000L)   # poll for 1 s then yield
      .flush_domain()          # process any queued reactive work
    },
    interrupt = function(e) invisible(NULL)
  )

  invisible(NULL)
}
