#' Log Out The Current Session
#'
#' Forces the browser to drop its session cookie and return to the login
#' page. For use inside a \code{\link{tabler_app}} \code{server} function
#' (e.g. in an \code{observe_event()} on a "Log out" button) when
#' \code{check_credentials} was supplied to \code{\link{tabler_app}}.
#'
#' @details
#' \strong{Why this can't be done by just hiding a DOM element}
#'
#' Unlike \code{\link{show_progress}}/\code{\link{hide_progress}}, which only
#' toggle a cosmetic overlay, a login gate has to withhold the actual page
#' content from the browser until the server has verified the visitor -
#' anything already sent to the browser (HTML, JS, JSON) can always be
#' revealed again with the browser's DevTools, no matter how it is hidden
#' client-side. \code{\link{tabler_app}} therefore checks a signed session
#' cookie \emph{before} it ever renders the page, serves cached outputs, or
#' accepts a WebSocket connection: an unauthenticated request gets nothing
#' but the login form, both for the initial page load and for the
#' WebSocket handshake that carries reactive output.
#'
#' \code{logout()} sends a small message over the WebSocket telling the
#' browser-side JS (\code{tabler-login.js}) to navigate to \code{/logout},
#' which is a real server-side endpoint that clears the session cookie and
#' redirects back to \code{/login}. This is the same show/hide message
#' mechanism as \code{\link{show_progress}}, but logout can only actually
#' take effect through the server, not through anything client-side.
#'
#' @param session The \code{session} object passed by \code{\link{tabler_app}}
#'   to the server function. Defaults to \code{\link{get_default_reactive_domain}()}.
#' @return Invisibly, \code{NULL}.
#' @seealso \code{\link{tabler_app}}
#' @examples
#' if (interactive()) {
#'   ui <- page(
#'     title = "Login Example",
#'     body = body(action_button("logout_btn", "Log out"))
#'   )
#'
#'   server <- function(input, output, session) {
#'     observe_event(input$logout_btn, {
#'       logout(session)
#'     })
#'   }
#'
#'   tabler_app(
#'     ui, server,
#'     check_credentials = function(username, password) {
#'       username == "admin" && password == "hunter2"
#'     }
#'   )
#' }
#' @export
logout <- function(session = get_default_reactive_domain()) {
  if (!is.list(session) || !is.function(session[["sendCustomMessage"]])) {
    warning("logout() requires a tabler_app session object - ignoring", call. = FALSE)
    return(invisible(NULL))
  }
  session$sendCustomMessage("tabler-logout", list())
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Internal helpers used by tabler_app()'s login gate (see app.R). Not exported.
# ---------------------------------------------------------------------------

.tabler_session_cookie <- "tabler_session"

# Resolves the HMAC secret used to sign session cookies. Falls back to a
# random per-run secret (with a warning) so cookies still work, but every
# session is invalidated on restart - set TABLER_SESSION_SECRET (or pass
# `session_secret`) to a fixed value to keep users logged in across restarts.
.resolve_login_secret <- function(session_secret) {
  if (nzchar(session_secret)) {
    return(session_secret)
  }
  warning(
    "tabler: no `session_secret` (or TABLER_SESSION_SECRET env var) was set - ",
    "using a random secret for this run. All logged-in sessions will be ",
    "invalidated the next time the app restarts.",
    call. = FALSE
  )
  paste(sample(c(letters, LETTERS, 0:9), 40L, replace = TRUE), collapse = "")
}

# Constant-time-ish comparison of two equal-length hex strings, to avoid
# leaking timing information about how many leading characters of a
# submitted signature happen to match the expected one.
.secure_string_equal <- function(a, b) {
  if (!is.character(a) || !is.character(b) || nchar(a) != nchar(b)) {
    return(FALSE)
  }
  sum(bitwXor(utf8ToInt(a), utf8ToInt(b))) == 0L
}

# Signs `user` + an expiry timestamp into an opaque cookie value:
# base64url(json payload) + "." + hex HMAC-SHA256 of that base64 payload.
# HMAC-SHA256 is computed by openssl::sha256(x, key = secret), which binds
# directly to libssl's HMAC() - a real, widely audited implementation.
.sign_session_token <- function(user, secret, expires_at) {
  payload <- jsonlite::toJSON(list(user = user, exp = expires_at), auto_unbox = TRUE)
  payload_b64 <- jsonlite::base64_enc(charToRaw(payload))
  sig <- as.character(openssl::sha256(payload_b64, key = secret))
  paste0(payload_b64, ".", sig)
}

# Verifies a cookie value produced by .sign_session_token(). Returns the
# logged-in username on success, or NULL if the token is missing, malformed,
# tampered with, or expired.
.verify_session_token <- function(token, secret) {
  if (is.null(token) || !is.character(token) || !nzchar(token)) {
    return(NULL)
  }
  parts <- strsplit(token, ".", fixed = TRUE)[[1]]
  if (length(parts) != 2L) {
    return(NULL)
  }
  payload_b64 <- parts[[1]]
  sig <- parts[[2]]
  expected <- as.character(openssl::sha256(payload_b64, key = secret))
  if (!.secure_string_equal(sig, expected)) {
    return(NULL)
  }
  payload <- tryCatch(
    jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(payload_b64))),
    error = function(e) NULL
  )
  if (is.null(payload) || is.null(payload$user) || is.null(payload$exp)) {
    return(NULL)
  }
  if (as.numeric(payload$exp) < as.numeric(Sys.time())) {
    return(NULL)
  }
  payload$user
}

# Minimal "Cookie" request header parser -> named list.
.parse_cookie_header <- function(header) {
  if (is.null(header) || !is.character(header) || !nzchar(header)) {
    return(list())
  }
  parts <- strsplit(header, ";\\s*")[[1]]
  out <- list()
  for (p in parts) {
    kv <- strsplit(p, "=", fixed = TRUE)[[1]]
    if (length(kv) >= 2L) {
      out[[trimws(kv[[1]])]] <- paste(kv[-1], collapse = "=")
    }
  }
  out
}

.get_cookie <- function(req, name) {
  cookies <- .parse_cookie_header(req[["HTTP_COOKIE"]])
  cookies[[name]]
}

# Builds a Set-Cookie header value. `max_age` in seconds; NULL means no
# Max-Age attribute (a browser-session cookie that disappears when the
# browser fully closes - used when `session_expires = 0`).
.set_cookie_header <- function(name, value, max_age = NULL) {
  cookie <- paste0(name, "=", value, "; Path=/; HttpOnly; SameSite=Lax")
  if (!is.null(max_age)) {
    cookie <- paste0(cookie, "; Max-Age=", as.integer(max_age))
  }
  cookie
}

.clear_cookie_header <- function(name) {
  paste0(name, "=; Path=/; HttpOnly; SameSite=Lax; Max-Age=0")
}

# application/x-www-form-urlencoded body parser -> named list of strings.
.parse_urlencoded_body <- function(req) {
  body_raw <- tryCatch(req[["rook.input"]]$read(), error = function(e) raw(0L))
  body_str <- rawToChar(body_raw)
  out <- list()
  for (p in strsplit(body_str, "&", fixed = TRUE)[[1]]) {
    if (!nzchar(p)) next
    kv <- strsplit(p, "=", fixed = TRUE)[[1]]
    key <- utils::URLdecode(gsub("+", " ", kv[[1]], fixed = TRUE))
    val <- if (length(kv) >= 2L) {
      utils::URLdecode(gsub("+", " ", paste(kv[-1], collapse = "="), fixed = TRUE))
    } else {
      ""
    }
    out[[key]] <- val
  }
  out
}

.html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

# Renders the standalone login page (not built through page()/layout_*(),
# which assume a full dashboard shell) using Tabler's own auth-card markup.
.render_login_page <- function(title, error = FALSE) {
  title_esc <- .html_escape(title)
  error_html <- if (isTRUE(error)) {
    '<div class="alert alert-danger" role="alert">Invalid username or password.</div>'
  } else {
    ""
  }
  paste0(
    "<!DOCTYPE html><html><head>",
    '<meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    "<title>", title_esc, "</title>",
    '<link rel="stylesheet" href="/tabler-1.4.0/css/tabler.min.css">',
    '<link rel="stylesheet" href="/tabler-icons-3.55.0/tabler-icons.min.css">',
    "</head>",
    '<body class="border-top-wide border-primary d-flex flex-column">',
    '<div class="page page-center">',
    '<div class="container container-tight py-4">',
    '<div class="text-center mb-4">',
    '<span class="navbar-brand navbar-brand-autodark">', title_esc, "</span>",
    "</div>",
    error_html,
    '<div class="card card-md">',
    '<div class="card-body">',
    '<h2 class="h2 text-center mb-4">', title_esc, "</h2>",
    '<form method="post" action="/login" autocomplete="off">',
    '<div class="mb-3">',
    '<label class="form-label">Username</label>',
    '<input type="text" name="username" class="form-control" autofocus required>',
    "</div>",
    '<div class="mb-2">',
    '<label class="form-label">Password</label>',
    '<input type="password" name="password" class="form-control" required>',
    "</div>",
    '<div class="form-footer">',
    '<button type="submit" class="btn btn-primary w-100">Sign in</button>',
    "</div>",
    "</form>",
    "</div>",
    "</div>",
    "</div>",
    "</div>",
    "</body></html>"
  )
}
