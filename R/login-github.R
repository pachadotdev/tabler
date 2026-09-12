# GitHub OAuth helpers for tabler_app()
# -----------------------------------------------------------------------
# Internal — not exported.
# -----------------------------------------------------------------------

# Package-level store for pending OAuth state tokens.
# Entry: state_hex_string -> list(created_at = numeric POSIX seconds).
.github_states <- new.env(hash = TRUE, parent = emptyenv())

# Remove state tokens older than 10 minutes to prevent unbounded growth.
.github_gc_states <- function() {
  now <- as.numeric(Sys.time())
  stale <- Filter(
    function(key) {
      (now - get(key, envir = .github_states, inherits = FALSE)$created_at) > 600
    },
    ls(.github_states)
  )
  if (length(stale)) rm(list = stale, envir = .github_states)
}

# Generate a new cryptographically-random CSRF state token and record it.
.github_new_state <- function() {
  .github_gc_states()
  state <- paste(sprintf("%02x", as.integer(openssl::rand_bytes(16L))), collapse = "")
  assign(state, list(created_at = as.numeric(Sys.time())), envir = .github_states)
  state
}

# Validate and consume a state token (single-use). Returns TRUE on success.
.github_consume_state <- function(state) {
  if (!is.character(state) || !nzchar(state)) return(FALSE)
  if (!exists(state, envir = .github_states, inherits = FALSE)) return(FALSE)
  rm(list = state, envir = .github_states)
  TRUE
}

# Build the GitHub OAuth authorization URL.
.github_auth_url <- function(client_id, redirect_uri, state, scopes) {
  scope <- paste(scopes, collapse = " ")
  # Use reserved = FALSE for scope so `:` in e.g. `read:org` is NOT percent-
  # encoded to `%3A`. GitHub's scope parser is strict and does not decode
  # percent-encoded colons, so `read%3Aorg` would be silently ignored,
  # resulting in a token with no read:org permission.
  paste0(
    "https://github.com/login/oauth/authorize",
    "?client_id=", utils::URLencode(client_id, reserved = TRUE),
    "&redirect_uri=", utils::URLencode(redirect_uri, reserved = TRUE),
    "&state=", utils::URLencode(state, reserved = TRUE),
    "&scope=", utils::URLencode(scope, reserved = FALSE)
  )
}

# Exchange an authorization code for an access token (server-to-server POST).
# The client_secret never leaves the server. Returns the token string or NULL.
.github_exchange_code <- function(client_id, client_secret, code, redirect_uri) {
  body <- paste0(
    "client_id=", utils::URLencode(client_id, reserved = TRUE),
    "&client_secret=", utils::URLencode(client_secret, reserved = TRUE),
    "&code=", utils::URLencode(code, reserved = TRUE),
    "&redirect_uri=", utils::URLencode(redirect_uri, reserved = TRUE)
  )
  h <- curl::new_handle()
  curl::handle_setopt(h,
    post = TRUE,
    postfields = body,
    httpheader = c(
      "Accept: application/json",
      "Content-Type: application/x-www-form-urlencoded"
    )
  )
  resp <- tryCatch(
    curl::curl_fetch_memory(
      "https://github.com/login/oauth/access_token",
      handle = h
    ),
    error = function(e) NULL
  )
  if (is.null(resp) || resp$status_code != 200L) return(NULL)
  parsed <- tryCatch(
    jsonlite::fromJSON(rawToChar(resp$content)),
    error = function(e) NULL
  )
  if (is.null(parsed) || is.null(parsed$access_token)) return(NULL)
  parsed$access_token
}

# Fetch the authenticated GitHub user's login name.
# Returns the username string, or NULL on any failure.
.github_get_user <- function(token) {
  h <- curl::new_handle()
  curl::handle_setopt(h, httpheader = c(
    paste0("Authorization: Bearer ", token),
    "Accept: application/vnd.github+json",
    "X-GitHub-Api-Version: 2022-11-28"
  ))
  resp <- tryCatch(
    curl::curl_fetch_memory("https://api.github.com/user", handle = h),
    error = function(e) NULL
  )
  if (is.null(resp) || resp$status_code != 200L) return(NULL)
  parsed <- tryCatch(
    jsonlite::fromJSON(rawToChar(resp$content)),
    error = function(e) NULL
  )
  parsed$login
}

# Check GitHub org membership for the authenticated user. Tries three
# progressively less privileged endpoints to handle org-level third-party
# app restrictions and tokens issued without full read:org scope:
#
#  Tier 1 — /user/memberships/orgs/{org}  (read:org scope + app approved by org)
#  Tier 2 — /orgs/{org}/members/{username} (authenticated; works if app not blocked)
#  Tier 3 — /orgs/{org}/public_members/{username} (no scope/approval needed,
#            but user must have set their org membership visibility to Public at
#            https://github.com/orgs/{org}/people?query={username})
#
# Returns TRUE when the user is confirmed as an active member via any tier.
.github_check_org <- function(token, org, username) {
  gh_headers <- c(
    paste0("Authorization: Bearer ", token),
    "Accept: application/vnd.github+json",
    "X-GitHub-Api-Version: 2022-11-28"
  )

  # Tier 1: authenticated membership state (needs read:org + org app approval)
  h1 <- curl::new_handle()
  curl::handle_setopt(h1, httpheader = gh_headers)
  resp1 <- tryCatch(
    curl::curl_fetch_memory(
      paste0("https://api.github.com/user/memberships/orgs/",
        utils::URLencode(org, reserved = TRUE)),
      handle = h1
    ),
    error = function(e) NULL
  )
  message("[tabler/github] tier1 /user/memberships/orgs/ status: ",
    if (is.null(resp1)) "(error)" else resp1$status_code)
  if (!is.null(resp1) && resp1$status_code == 200L) {
    parsed <- tryCatch(jsonlite::fromJSON(rawToChar(resp1$content)), error = function(e) NULL)
    state <- parsed$state %||% ""
    message("[tabler/github] membership state: ", state)
    if (identical(state, "active")) return(TRUE)
  }

  # Tier 2: org members list (authenticated; works when app is not blocked
  # by org restrictions, even without read:org)
  h2 <- curl::new_handle()
  curl::handle_setopt(h2, followlocation = FALSE, httpheader = gh_headers)
  resp2 <- tryCatch(
    curl::curl_fetch_memory(
      paste0("https://api.github.com/orgs/",
        utils::URLencode(org, reserved = TRUE),
        "/members/",
        utils::URLencode(username, reserved = TRUE)),
      handle = h2
    ),
    error = function(e) NULL
  )
  message("[tabler/github] tier2 /orgs/{org}/members/ status: ",
    if (is.null(resp2)) "(error)" else resp2$status_code)
  if (!is.null(resp2) && resp2$status_code == 204L) return(TRUE)

  # Tier 3: public membership check — no scope or app approval needed, but
  # the user's org membership must be set to Public on GitHub.
  h3 <- curl::new_handle()
  curl::handle_setopt(h3, httpheader = gh_headers)
  resp3 <- tryCatch(
    curl::curl_fetch_memory(
      paste0("https://api.github.com/orgs/",
        utils::URLencode(org, reserved = TRUE),
        "/public_members/",
        utils::URLencode(username, reserved = TRUE)),
      handle = h3
    ),
    error = function(e) NULL
  )
  message("[tabler/github] tier3 /orgs/{org}/public_members/ status: ",
    if (is.null(resp3)) "(error)" else resp3$status_code)
  !is.null(resp3) && resp3$status_code == 204L
}

# Minimal URL query string parser -> named list of decoded strings.
.parse_query_string <- function(qs) {
  if (is.null(qs) || !nzchar(qs)) return(list())
  qs <- sub("^\\?", "", qs)
  out <- list()
  for (p in strsplit(qs, "&", fixed = TRUE)[[1]]) {
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

# Render the standalone GitHub login page (Tabler auth-card layout).
.render_github_login_page <- function(title, error = FALSE) {
  title_esc <- .html_escape(title)
  error_html <- if (isTRUE(error)) {
    paste0(
      '<div class="alert alert-danger" role="alert">',
      "Access denied. If you are a member of the required GitHub organisation, ",
      "make sure your membership visibility is set to ",
      '<strong>Public</strong> at ',
      '<a href="https://github.com/orgs" target="_blank" rel="noopener">',
      "github.com/orgs/{org}/people</a>.",
      "</div>"
    )
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
    render_html(
      button(
        "Sign in with GitHub",
        href = "/github/login",
        icon = social("github"),
        outline = TRUE,
        color = "secondary",
        block = TRUE
      )
    ),
    "</div>",
    "</div>",
    "</div>",
    "</div>",
    "</body></html>"
  )
}
