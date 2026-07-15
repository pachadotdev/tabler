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
# Static resource path registry
# ---------------------------------------------------------------------------

# Package-level registry: prefix -> absolute directory path
.resource_paths <- new.env(parent = emptyenv())

#' @title Register a Directory of Static Resources
#' @description Serves files under \code{directoryPath} at URLs beginning
#'   with \code{/prefix/}, mirroring \code{shiny::addResourcePath()}. Use this
#'   to serve an app's own CSS/JS/image assets (e.g. from \code{inst/app/www})
#'   without depending on \pkg{shiny} or \pkg{golem}.
#' @param prefix The URL prefix (e.g. \code{"www"} serves files at \code{/www/...}).
#' @param directoryPath Absolute path to the directory to serve.
#' @return Invisibly, \code{NULL}.
#' @export
addResourcePath <- function(prefix, directoryPath) {
  prefix <- sub("^/+", "", prefix)
  prefix <- sub("/+$", "", prefix)
  assign(prefix, normalizePath(directoryPath, mustWork = TRUE), envir = .resource_paths)
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Input proxy — a reactiveValues() store; $.ReactiveValues handles reactive
# reads so no custom S3 method for $ is needed here.
# ---------------------------------------------------------------------------

# Internal helper: jsonlite::fromJSON(..., simplifyVector = FALSE) turns
# every JSON array into an R list, even ones that only contain scalars (e.g.
# a dual-thumb slider's [2018, 2022] or a checkbox-group's ["a", "b"]). Shiny
# code expects these as plain atomic vectors (input$y[1], min(input$y), ...),
# so unwrap any such "flat" list before it reaches input$<name>.
.simplify_input_value <- function(x) {
  if (is.list(x) && length(x) > 0L &&
      all(vapply(x, function(e) is.atomic(e) && length(e) == 1L, logical(1L)))) {
    return(unlist(x, use.names = FALSE))
  }
  x
}

# Internal helper: set a reactive input value without going through $<-
.rv_set <- function(rv, name, value) {
  store   <- attr(rv, ".store")
  sources <- attr(rv, ".sources")
  value   <- .simplify_input_value(value)
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
  connections    <- new.env(hash = TRUE, parent = emptyenv())  # id -> ws
  output_cache   <- new.env(hash = TRUE, parent = emptyenv())  # id -> html string
  widget_store   <- new.env(hash = TRUE, parent = emptyenv())  # id -> widget HTML
  plot_store     <- new.env(hash = TRUE, parent = emptyenv())  # id -> svg string
  download_store <- new.env(hash = TRUE, parent = emptyenv())  # id -> tabler_render (type="download")

  # Input / output proxies ----
  input_proxy  <- reactiveValues()          # $.ReactiveValues gives reactive reads
  output_proxy <- new.env(parent = emptyenv())  # plain env; $<- is standard env assign

  # URL sync config — NULL means disabled; character() means enabled (empty exclude) ----
  url_sync_exclude <- NULL

  # Session object (minimal) ----
  send_all <- function(msg_json) {
    for (ws in as.list(connections)) {
      tryCatch(ws$send(msg_json), error = function(e) NULL)
    }
  }

  session <- list(
    input  = input_proxy,
    output = output_proxy,
    ns     = function(id) id,
    sendCustomMessage = function(type, message) {
      send_all(jsonlite::toJSON(
        list(type = "custom", messageType = type, message = message),
        auto_unbox = TRUE
      ))
    },
    onSessionEnded = function(fn) invisible(NULL),
    close = function() invisible(NULL),
    # Used by syncUrl() — stores the exclude list and enables URL sync
    .setUrlSync = function(exclude) {
      url_sync_exclude <<- as.character(exclude)
    }
  )

  # Make this session discoverable via getDefaultReactiveDomain(), used as
  # the default `session` argument of moduleServer() ----
  .domain$current_session <- session

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
      } else if (identical(type, "plot_src") && is.character(val)) {
        # Serve the SVG from a dedicated HTTP endpoint so <img> gets a real
        # URL — no encoding needed and right-click "Save image as" works.
        assign(output_id, val, envir = plot_store)
        html <- paste0(
          '<img src="/plots/', output_id, '?t=', as.integer(Sys.time()), '"',
          ' style="max-width:100%;height:auto;display:block;" alt="plot">'
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
    if (inherits(render_obj, "tabler_render") && identical(render_obj$type, "download")) {
      # Download handlers are served on-demand via the /downloads/ HTTP
      # endpoint below, not pushed reactively like other outputs.
      assign(nm, render_obj, envir = download_store)
      next
    }
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

    # Download endpoint — evaluates a downloadHandler() on demand
    if (grepl("^/downloads/", path)) {
      did <- sub("^/downloads/([^?]*).*$", "\\1", path)
      if (grepl("..", did, fixed = TRUE) || grepl("/", did, fixed = TRUE)) {
        return(list(status = 403L,
                    headers = list("Content-Type" = "text/plain"),
                    body    = "Forbidden"))
      }
      if (!exists(did, envir = download_store, inherits = FALSE)) {
        return(list(status = 404L,
                    headers = list("Content-Type" = "text/plain"),
                    body    = "Download not found"))
      }
      dh <- get(did, envir = download_store, inherits = FALSE)
      result <- tryCatch({
        fname <- isolate(if (is.function(dh$filename)) dh$filename() else dh$filename)
        tmp   <- tempfile()
        on.exit(unlink(tmp), add = TRUE)
        isolate(dh$content(tmp))
        if (!file.exists(tmp)) stop("downloadHandler's content() did not write to the given file path")
        fsize   <- file.info(tmp)$size
        con     <- file(tmp, "rb")
        on.exit(close(con), add = TRUE)
        content <- readBin(con, "raw", fsize)
        ctype   <- if (!is.null(dh$contentType)) dh$contentType else .mime_type(fname)
        list(
          status  = 200L,
          headers = list(
            "Content-Type"        = ctype,
            "Content-Disposition" = sprintf('attachment; filename="%s"', fname)
          ),
          body    = content
        )
      }, error = function(e) {
        list(status = 500L,
             headers = list("Content-Type" = "text/plain"),
             body = paste("Download failed:", conditionMessage(e)))
      })
      return(result)
    }

    # Plot SVG endpoint — serves raw SVG so <img> can use a plain URL
    if (grepl("^/plots/", path)) {
      pid <- sub("^/plots/([^?]*).*$", "\\1", path)
      if (grepl("..", pid, fixed = TRUE) || grepl("/", pid, fixed = TRUE)) {
        return(list(status = 403L,
                    headers = list("Content-Type" = "text/plain"),
                    body    = "Forbidden"))
      }
      if (exists(pid, envir = plot_store, inherits = FALSE)) {
        return(list(
          status  = 200L,
          headers = list("Content-Type"  = "image/svg+xml; charset=utf-8",
                         "Cache-Control" = "no-cache, no-store, must-revalidate"),
          body    = get(pid, envir = plot_store)
        ))
      }
      return(list(status = 404L,
                  headers = list("Content-Type" = "text/plain"),
                  body    = "Plot not found"))
    }

    # Static assets — resolve via system.file() for path-traversal safety
    rel <- sub("^/+", "", path)

    # Block anything that looks dangerous before hitting system.file
    if (grepl("..", rel, fixed = TRUE) || grepl("^~", rel)) {
      return(list(status = 403L,
                  headers = list("Content-Type" = "text/plain"),
                  body = "Forbidden"))
    }

    # Registered app resource paths (see addResourcePath()) take priority
    seg <- sub("/.*$", "", rel)
    if (exists(seg, envir = .resource_paths, inherits = FALSE)) {
      base_dir  <- get(seg, envir = .resource_paths, inherits = FALSE)
      file_rel  <- sub(paste0("^", seg, "/?"), "", rel)
      file_path <- if (nzchar(file_rel)) {
        normalizePath(file.path(base_dir, file_rel), mustWork = FALSE)
      } else {
        ""
      }
      if (nzchar(file_rel) &&
          startsWith(file_path, base_dir) &&
          file.exists(file_path) && !dir.exists(file_path)) {
        fsize   <- file.info(file_path)$size
        con     <- file(file_path, "rb")
        content <- readBin(con, "raw", fsize)
        close(con)
        return(list(
          status  = 200L,
          headers = list("Content-Type" = .mime_type(file_path)),
          body    = content
        ))
      }
      return(list(status = 404L,
                  headers = list("Content-Type" = "text/plain"),
                  body = "Not found"))
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

    # Send URL sync config if syncUrl() was called in the server function
    if (!is.null(url_sync_exclude)) {
      tryCatch(
        ws$send(jsonlite::toJSON(
          list(type = "custom", messageType = "tablerSyncUrl",
               message = list(exclude = as.list(url_sync_exclude))),
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
      # Run any due later::later() callbacks (e.g. work deferred by
      # withProgress() so the "show" message can reach the browser first)
      # before flushing reactive observers that they may have scheduled.
      later::run_now(timeoutSecs = 0, all = TRUE)
      .flush_domain()          # process any queued reactive work
    },
    interrupt = function(e) invisible(NULL)
  )

  invisible(NULL)
}
