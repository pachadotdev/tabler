# Minimal reactive system with full dependency tracking.
# Reactive programming primitives for tabler.

# ---------------------------------------------------------------------------
# Global domain state
# ---------------------------------------------------------------------------
.domain <- local({
  e <- new.env(parent = emptyenv())
  e$id_counter    <- 0L
  e$context_stack <- list()   # currently-executing reactive contexts (stack)
  e$pending       <- list()   # observers scheduled for next flush
  e$flushing      <- FALSE
  e
})

.new_id <- function() {
  .domain$id_counter <- .domain$id_counter + 1L
  .domain$id_counter
}

.current_context <- function() {
  n <- length(.domain$context_stack)
  if (n == 0L) NULL else .domain$context_stack[[n]]
}

.push_context <- function(ctx) {
  .domain$context_stack <- c(.domain$context_stack, list(ctx))
}

.pop_context <- function() {
  n <- length(.domain$context_stack)
  if (n > 0L) .domain$context_stack <- .domain$context_stack[-n]
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Reactive source - tracks which contexts depend on it
# ---------------------------------------------------------------------------
new_source <- function() {
  # `deps` gives O(1) existence checks (for dedup); `dep_order` is the
  # insertion-ordered list of ids, and is what invalidate_dependents()
  # actually iterates over. A plain hashed environment's as.list()/ls() order
  # is *not* guaranteed to match insertion order, which matters here: several
  # sibling eventReactive()/bindEvent()/observeEvent() calls gated on the same
  # event (e.g. input$go) rely on being invalidated/re-run in the order they
  # were defined (module setup runs top-to-bottom, synchronously) - e.g. a
  # later observer that eagerly forces several earlier bindEvent()-wrapped
  # reactives to compute (for progress-bar bracketing) needs those reactives'
  # own dirty-flagging watchers to have already run.
  deps      <- new.env(hash = TRUE, parent = emptyenv())
  dep_order <- character(0)

  list(
    depend = function() {
      ctx <- .current_context()
      if (is.null(ctx)) return(invisible(NULL))
      id <- as.character(ctx$id)
      if (exists(id, envir = deps, inherits = FALSE)) return(invisible(NULL))
      assign(id, ctx, envir = deps)
      dep_order <<- c(dep_order, id)
      ctx$on_invalidate(function() {
        if (exists(id, envir = deps, inherits = FALSE)) rm(list = id, envir = deps)
        dep_order <<- dep_order[dep_order != id]
      })
      invisible(NULL)
    },
    invalidate_dependents = function() {
      ids       <- dep_order
      dep_order <<- character(0)
      ctxs      <- mget(ids, envir = deps, ifnotfound = list(NULL))
      rm(list = ls(deps), envir = deps)
      for (ctx in ctxs) if (!is.null(ctx)) ctx$invalidate()
    }
  )
}

# ---------------------------------------------------------------------------
# Reactive context - can be invalidated; runs cleanup callbacks on invalidation
# ---------------------------------------------------------------------------
new_context <- function(on_invalidate_fn) {
  id          <- .new_id()
  invalidated <- FALSE
  cleanups    <- list()

  list(
    id = id,
    invalidate = function() {
      if (invalidated) return(invisible(NULL))
      invalidated <<- TRUE
      fns <- cleanups; cleanups <<- list()
      for (fn in fns) fn()
      on_invalidate_fn()
    },
    on_invalidate = function(fn) {
      if (invalidated) fn() else cleanups <<- c(cleanups, list(fn))
    },
    is_invalidated = function() invalidated
  )
}

# ---------------------------------------------------------------------------
# Scheduler / flush
# ---------------------------------------------------------------------------
.schedule <- function(fn) {
  .domain$pending <- c(.domain$pending, list(fn))
  .flush_domain()
}

.flush_domain <- function() {
  if (.domain$flushing) return(invisible(NULL))
  .domain$flushing <- TRUE
  on.exit(.domain$flushing <- FALSE)
  while (length(.domain$pending) > 0L) {
    fn <- .domain$pending[[1L]]
    .domain$pending <- .domain$pending[-1L]
    # An uncaught error in one observer must not abort the whole flush: any
    # other observers still pending (e.g. one that calls hideProgress(), or
    # reveals a UI section) would otherwise silently never run. This mirrors
    # shiny, where an error in one observer/output doesn't take down
    # unrelated ones.
    tryCatch(
      fn(),
      error = function(e) {
        message("tabler: error in reactive observer: ", conditionMessage(e))
      }
    )
  }
}

#' Flush All Pending Reactive Observers
#' @description Runs all observers that have been scheduled for re-execution.
#'   Normally called automatically, but can be invoked manually if needed.
#' @keywords internal
#' @noRd
flush_reactive <- .flush_domain

# ---------------------------------------------------------------------------
# Public reactive primitives
# ---------------------------------------------------------------------------

#' @title Reactive Value
#' @description A single mutable reactive value.  Call with no argument to read
#'   (tracks the caller as a dependent); call with one argument to write (invalidates
#'   all current dependents).
#' @param value Initial value.
#' @return A function that acts as getter/setter.
#' @rdname reactive-primitives
#' @export
reactiveVal <- function(value = NULL) {
  src <- new_source()
  function(x) {
    if (missing(x)) {
      src$depend()
      value
    } else {
      value <<- x
      src$invalidate_dependents()
      invisible(x)
    }
  }
}

#' @title Named Reactive Values
#' @description A named collection of reactive values similar to a reactive list.
#'   Access with \code{rv$name} (reactive read) or \code{rv$name <- val} (reactive write).
#' @param ... Initial \code{name = value} pairs.
#' @return An environment of class \code{"ReactiveValues"}.
#' @rdname reactive-primitives
#' @export
reactiveValues <- function(...) {
  initial <- list(...)
  sources <- new.env(hash = TRUE, parent = emptyenv())
  store   <- new.env(hash = TRUE, parent = emptyenv())

  for (nm in names(initial)) {
    assign(nm, initial[[nm]], envir = store)
    assign(nm, new_source(),  envir = sources)
  }

  rv <- new.env(parent = emptyenv())
  attr(rv, ".sources") <- sources
  attr(rv, ".store")   <- store
  class(rv) <- "ReactiveValues"
  rv
}

#' @title Convert Reactive Values To A List
#' @description Returns a plain list snapshot of a \code{\link{reactiveValues}}
#'   object's current values, establishing a reactive read dependency on each
#'   contained value, similar to \code{shiny::reactiveValuesToList()}.
#' @param x A \code{reactiveValues()} object.
#' @param all.names Include names starting with a dot (default \code{FALSE}).
#' @return A named list.
#' @rdname reactive-primitives
#' @export
reactiveValuesToList <- function(x, all.names = FALSE) {
  store <- attr(x, ".store")
  if (is.null(store)) {
    stop("reactiveValuesToList() requires a reactiveValues() object", call. = FALSE)
  }
  nms <- ls(store, all.names = all.names)
  out <- lapply(nms, function(nm) .rv_dollar(x, nm))
  names(out) <- nms
  out
}

# Not named `$.ReactiveValues` / `$<-.ReactiveValues` to avoid roxygen2
# generating invalid NAMESPACE entries for operator generics.
# Registered as S3 methods in .onLoad() (see tabler-package.R).
.rv_dollar <- function(x, name) {
  sources <- attr(x, ".sources")
  store   <- attr(x, ".store")
  if (!exists(name, envir = sources, inherits = FALSE)) {
    assign(name, new_source(), envir = sources)
  }
  get(name, envir = sources)$depend()
  if (exists(name, envir = store, inherits = FALSE)) get(name, envir = store) else NULL
}

.rv_dollar_assign <- function(x, name, value) {
  sources <- attr(x, ".sources")
  store   <- attr(x, ".store")
  assign(name, value, envir = store)
  if (!exists(name, envir = sources, inherits = FALSE)) {
    assign(name, new_source(), envir = sources)
  }
  get(name, envir = sources)$invalidate_dependents()
  x
}

#' @title Reactive Expression
#' @description A lazy, cached computation that re-evaluates only when its
#'   reactive dependencies are invalidated.
#' @param expr Expression (use \code{\{ \}} for multi-line).
#' @return A zero-argument function returning the cached value.
#' @rdname reactive-primitives
#' @export
reactive <- function(expr) {
  expr_q  <- substitute(expr)
  env     <- parent.frame()
  src     <- new_source()
  cached  <- NULL
  invalid <- TRUE
  cur_ctx <- NULL

  function() {
    src$depend()
    if (!invalid) return(cached)

    # Reuse the current internal context across repeated evaluation attempts
    # made while still invalid (e.g. several req()-aborted attempts, each
    # triggered by a different output, before an upstream input first has a
    # value) instead of creating a brand new context on every call. A fresh
    # context is only needed once the previous one has actually been
    # invalidated by a real upstream change - otherwise every failed attempt
    # would register its own separate, never-cleaned-up dependency on the
    # same upstream source (e.g. an input), and each of those stale contexts
    # would independently fire its own invalidate+recompute cascade once the
    # upstream source finally changes, instead of exactly one.
    if (is.null(cur_ctx) || cur_ctx$is_invalidated()) {
      cur_ctx <<- new_context(function() {
        invalid <<- TRUE
        src$invalidate_dependents()
      })
    }
    ctx <- cur_ctx
    .push_context(ctx)
    tryCatch(
      { cached <<- eval(expr_q, env); invalid <<- FALSE },
      finally = .pop_context()
    )
    cached
  }
}

# Internal: shiny-style "truthiness" check used by req().
.is_truthy <- function(x) {
  if (is.null(x) || length(x) == 0L) return(FALSE)
  if (length(x) == 1L) {
    if (is.na(x)) return(FALSE)
    if (is.character(x) && !nzchar(x)) return(FALSE)
    if (is.logical(x) && !isTRUE(x)) return(FALSE)
  }
  TRUE
}

#' @title Ensure Values Are Available Before Proceeding
#' @description Stops execution of the current reactive expression, observer,
#'   or render function if any argument is not "truthy" (i.e. is \code{NULL},
#'   \code{NA}, \code{FALSE}, an empty string, or an empty vector), similar to
#'   \code{shiny::req()}. Unlike a normal error, this stop is silent: an
#'   \code{observe()}/\code{observeEvent()} block simply does nothing for this
#'   run, and a render function (\code{renderUI}, \code{renderText}, ...)
#'   simply leaves its output unchanged, instead of showing an error.
#' @param ... Values to check; all must be truthy for \code{req()} to return.
#' @param cancelOutput Ignored (kept for signature compatibility with Shiny).
#' @return The last argument, invisibly, if all checks pass.
#' @rdname reactive-primitives
#' @export
req <- function(..., cancelOutput = FALSE) {
  vals <- list(...)
  for (v in vals) {
    if (!.is_truthy(v)) {
      stop(structure(
        class = c("tabler_req_stop", "error", "condition"),
        list(message = "req: value not truthy", call = NULL)
      ))
    }
  }
  invisible(if (length(vals)) vals[[length(vals)]] else NULL)
}

#' @title Evaluate Without Tracking Dependencies
#' @description Any reactive reads inside \code{expr} are not registered as
#'   dependencies of the current context.
#' @param expr Expression to evaluate.
#' @return The result of \code{expr}.
#' @rdname reactive-primitives
#' @export
isolate <- function(expr) {
  expr_q <- substitute(expr)
  env    <- parent.frame()
  saved  <- .domain$context_stack
  .domain$context_stack <- list()
  on.exit(.domain$context_stack <- saved)
  eval(expr_q, env)
}

#' @title Reactive Observer
#' @description Eagerly re-runs \code{expr} whenever its reactive dependencies
#'   change.
#' @param expr Expression with side effects.
#' @return An invisible observer handle with \code{$suspend()} / \code{$resume()}.
#' @rdname reactive-primitives
#' @export
observe <- function(expr) {
  expr_q    <- substitute(expr)
  env       <- parent.frame()
  suspended <- FALSE

  # Snapshot the reactive domain (session) that is active when this observer
  # is *created* - e.g. the module-scoped session set up by moduleServer()
  # while it synchronously runs the module's server function. Without this,
  # functions that default to getDefaultReactiveDomain() (show(), hide(),
  # showProgress(), ...) would resolve to the wrong session - the top-level
  # one - whenever this observer's body actually runs later (on a button
  # click, say), because moduleServer() already restored the previous
  # session by the time that happens. Restoring the captured session for the
  # duration of each run mirrors shiny's withReactiveDomain() behaviour.
  captured_session <- .domain$current_session

  run <- function() {
    if (suspended) return(invisible(NULL))
    ctx <- new_context(function() {
      if (!suspended) .schedule(run)
    })
    .push_context(ctx)
    prev_session <- .domain$current_session
    .domain$current_session <- captured_session
    tryCatch(
      eval(expr_q, env),
      tabler_req_stop = function(e) invisible(NULL),
      finally = {
        .domain$current_session <- prev_session
        .pop_context()
      }
    )
  }

  .schedule(run)

  invisible(list(
    suspend = function() { suspended <<- TRUE },
    resume  = function() { suspended <<- FALSE; .schedule(run) }
  ))
}

#' @title Event-Based Observer
#' @description Runs \code{handlerExpr} whenever \code{eventExpr} changes.
#' @param eventExpr   Reactive expression whose change triggers the handler.
#' @param handlerExpr Expression to run when the event fires.
#' @param ignoreInit  If \code{TRUE} (default), skip the first evaluation.
#' @rdname reactive-primitives
#' @export
observeEvent <- function(eventExpr, handlerExpr, ignoreInit = TRUE) {
  event_q   <- substitute(eventExpr)
  handler_q <- substitute(handlerExpr)
  env       <- parent.frame()
  # State is kept in an environment (reference semantics) rather than a plain
  # local, because the observer body below is eval()'d in this frame and a
  # `<<-` superassignment would skip the local binding and write to the global
  # environment, leaving `init_done` permanently FALSE.
  state <- new.env(parent = emptyenv())
  state$init_done <- !isTRUE(ignoreInit)

  observe({
    eval(event_q, env)          # read to register dependency
    if (!state$init_done) {
      state$init_done <- TRUE
      return(invisible(NULL))
    }
    isolate(eval(handler_q, env))
  })
}

#' @title Event-Based Reactive Expression
#' @description A reactive expression that recomputes \code{valueExpr} only
#'   when \code{eventExpr} changes, similar to \code{shiny::eventReactive()}.
#' @param eventExpr  Reactive expression whose change triggers re-evaluation.
#' @param valueExpr  Expression to evaluate (and cache) when the event fires.
#' @param ignoreNULL If \code{TRUE} (default), do not (re)compute while
#'   \code{eventExpr} evaluates to \code{NULL}.
#' @param ignoreInit If \code{TRUE} (default), \code{valueExpr} is not
#'   evaluated until \code{eventExpr} first changes (e.g. an actionButton's
#'   click counter starts at \code{0}, not \code{NULL}, so without this,
#'   \code{valueExpr} would run once immediately on creation, before any
#'   click).
#' @return A zero-argument function returning the cached value.
#' @rdname reactive-primitives
#' @export
eventReactive <- function(eventExpr, valueExpr, ignoreNULL = TRUE, ignoreInit = TRUE) {
  event_q <- substitute(eventExpr)
  value_q <- substitute(valueExpr)
  env     <- parent.frame()

  src   <- new_source()
  state <- new.env(parent = emptyenv())
  state$init_done <- !isTRUE(ignoreInit)
  state$has_value <- FALSE
  state$value     <- NULL
  state$dirty     <- FALSE

  # Eagerly track invalidation of the event expression (exactly like a plain
  # observe()) so the dependency is registered, and the once-only "init"
  # check happens, right away - this can't race a real click that occurs in
  # the same flush pass. Crucially, this does NOT compute valueExpr; it only
  # flags the cached value as stale. Computation stays fully lazy (pull-based,
  # like reactive()), because several sibling eventReactive()/bindEvent()
  # calls are often gated by the same event (e.g. input$go): if one of them
  # eagerly recomputed here, another one reading it moments later in the
  # *same* flush pass (observers run in scheduling order, not dependency
  # order) could still see its stale pre-event value.
  observe({
    ev        <- eval(event_q, env)   # read to register dependency
    first_run <- !state$init_done
    state$init_done <- TRUE
    if (first_run && isTRUE(ignoreInit)) return(invisible(NULL))
    if (isTRUE(ignoreNULL) && is.null(ev)) return(invisible(NULL))
    state$dirty <- TRUE
    src$invalidate_dependents()
  })

  function() {
    src$depend()
    if (state$dirty) {
      state$value     <- isolate(eval(value_q, env))
      state$has_value <- TRUE
      state$dirty     <- FALSE
    }
    if (!state$has_value) return(NULL)
    state$value
  }
}

#' @title Bind an Event Trigger to a Reactive Expression
#' @description Modifies a reactive expression created by \code{\link{reactive}}
#'   so that it only (re)executes when the given event expression(s) change,
#'   similar to \code{shiny::bindEvent()}. Typically used with the pipe:
#'   \code{reactive({...}) |> bindEvent(input$go)}.
#' @param x A reactive expression created by \code{\link{reactive}}.
#' @param ... One or more (unevaluated) event expressions. The reactive fires
#'   whenever any of them change.
#' @param ignoreNULL If \code{TRUE} (default), do not (re)compute while all
#'   event expressions evaluate to \code{NULL}.
#' @param ignoreInit If \code{TRUE} (default), \code{x} is not computed until
#'   an event expression first changes (e.g. an actionButton's click counter
#'   starts at \code{0}, not \code{NULL}, so without this, \code{x} would
#'   compute once immediately on creation, before any click).
#' @return A new zero-argument function returning the cached value.
#' @rdname reactive-primitives
#' @export
bindEvent <- function(x, ..., ignoreNULL = TRUE, ignoreInit = TRUE) {
  if (!is.function(x)) {
    stop("bindEvent() only supports reactive expressions created by reactive()", call. = FALSE)
  }
  dots <- substitute(list(...))[-1]
  env  <- parent.frame()

  src   <- new_source()
  state <- new.env(parent = emptyenv())
  state$init_done <- !isTRUE(ignoreInit)
  state$has_value <- FALSE
  state$value     <- NULL
  state$dirty     <- FALSE

  # See the comment in eventReactive() above: only invalidation-tracking is
  # eager here; the wrapped x() is pulled lazily on next read so that sibling
  # reactives gated by the same event always see each other's fresh values.
  observe({
    evs       <- lapply(dots, function(e) eval(e, env))   # register dependencies
    first_run <- !state$init_done
    state$init_done <- TRUE
    if (first_run && isTRUE(ignoreInit)) return(invisible(NULL))
    if (isTRUE(ignoreNULL) && all(vapply(evs, is.null, logical(1L)))) return(invisible(NULL))
    state$dirty <- TRUE
    src$invalidate_dependents()
  })

  function() {
    src$depend()
    if (state$dirty) {
      state$value     <- isolate(x())
      state$has_value <- TRUE
      state$dirty     <- FALSE
    }
    if (!state$has_value) return(NULL)
    state$value
  }
}

#' @title Set Tabler Package Options
#' @description Configure package-wide options for tabler. Currently this
#'   only sets the cache backend used by \code{\link{bindCache}}, mirroring
#'   \code{shiny::shinyOptions(cache = ...)}.
#' @param cache A \pkg{cachem} cache object (e.g. \code{cachem::cache_disk(dir
#'   = "/path/to/cache")} or \code{cachem::cache_mem()}) used to store
#'   \code{\link{bindCache}} values. If never set, an in-memory
#'   \code{cachem::cache_mem()} is created and used automatically - which
#'   (like a plain \code{\link{reactive}}) does not survive an R restart.
#'   Pass a \code{cache_disk()} object to persist values across app restarts
#'   and sessions.
#' @return Invisibly, the previous options (as a named list, for restoring
#'   later).
#' @rdname reactive-primitives
#' @export
tablerOptions <- function(cache) {
  old <- list(cache = .tabler_opts$cache)
  if (!missing(cache)) .tabler_opts$cache <- cache
  invisible(old)
}

# Internal: package-wide option storage (mirrors shiny's .globals env).
.tabler_opts <- new.env(parent = emptyenv())

# Internal: lazily create a default in-memory cache if none was configured
# via tablerOptions(cache = ...).
.tabler_cache <- function() {
  if (is.null(.tabler_opts$cache)) {
    .tabler_opts$cache <- cachem::cache_mem()
  }
  .tabler_opts$cache
}

# Internal: build a valid cachem cache key (lowercase hex hash) from a list
# of evaluated key expression values. cachem's cache_disk/cache_mem require
# keys matching a filesystem-safe, lowercase-alphanumeric pattern, so the
# key expressions' values (which can be arbitrary R objects) must be hashed
# rather than used as a literal string.
.tabler_cache_key <- function(keys) {
  rlang::hash(keys)
}

#' @title Cache a Reactive Expression's Value
#' @description Persistently caches a reactive expression's value, keyed by
#'   one or more key expressions, similar to \code{shiny::bindCache()}.
#'   Unlike \code{\link{reactive}}'s built-in caching (in-memory, lost as
#'   soon as its dependencies change), \code{bindCache()} stores values in
#'   the cache backend configured via \code{\link{tablerOptions}} (e.g.
#'   \code{cachem::cache_disk()}), so identical key combinations are served
#'   instantly - even across app restarts or different sessions - without
#'   re-running \code{x}. Typically used with the pipe:
#'   \code{reactive({...}) |> bindCache(key1, key2) |> bindEvent(input$go)}.
#' @param x A reactive expression created by \code{\link{reactive}}.
#' @param ... One or more (unevaluated) key expressions. Whenever their
#'   combined value changes, \code{x} is (re)computed and the result is
#'   cached; otherwise the previously cached value is returned directly,
#'   without calling \code{x} again.
#' @return A new zero-argument function returning the (possibly cached) value.
#' @rdname reactive-primitives
#' @export
bindCache <- function(x, ...) {
  if (!is.function(x)) {
    stop("bindCache() only supports reactive expressions created by reactive()", call. = FALSE)
  }
  key_q <- substitute(list(...))[-1]
  env   <- parent.frame()

  # Every bindCache() call site gets its own namespace, folded into every key
  # it generates. Without this, two different reactives bound to the same
  # cache with identical key *values* (e.g. two outputs both keyed on the same
  # inputs) would collide on the exact same cache slot and silently return
  # each other's cached value instead of each computing its own. The
  # namespace is the literal source text of the `x` argument (e.g. the whole
  # `reactive({...})` block passed in via the pipe) - unique per call site,
  # and (unlike a runtime counter) stable across app restarts, so a
  # cache_disk() cache still hits after a restart.
  ns <- paste(deparse(substitute(x)), collapse = "\n")

  function() {
    keys  <- lapply(key_q, function(e) eval(e, env))
    cache <- .tabler_cache()
    key   <- .tabler_cache_key(c(list(ns), keys))
    if (cache$exists(key)) {
      return(cache$get(key))
    }
    val <- x()
    cache$set(key, val)
    val
  }
}


# ---------------------------------------------------------------------------
# URL sync
# ---------------------------------------------------------------------------

#' @title Sync App Inputs with the URL
#' @description Call once inside the server function to enable two-way URL
#'   parameter synchronisation: the URL query string initialises inputs on
#'   page load, and every subsequent input change updates the URL in-place
#'   (no page reload, no browser-history spam).
#'
#' @param session The \code{session} object passed by \code{\link{tablerApp}}
#'   to the server function.
#' @param exclude Character vector of input IDs to omit from the URL.
#'   Action buttons are \emph{always} omitted regardless of this setting.
#'
#' @details
#' The resulting URL is clean and quote-free, e.g.
#' \preformatted{http://localhost:3000/?dataset=mtcars&n_rows=10&stat=mean}
#'
#' Sharing or bookmarking that URL restores the exact input state.
#'
#' @return Invisibly, \code{session} (for chaining).
#' @rdname reactive-primitives
#' @export
syncUrl <- function(session, exclude = character(0L)) {
  if (!is.function(session[[".setUrlSync"]])) {
    warning("syncUrl() requires a tablerApp session object - ignoring")
    return(invisible(session))
  }
  session$.setUrlSync(as.character(exclude))
  invisible(session)
}

# ---------------------------------------------------------------------------
# Internal: observer for output renderers (avoids NSE at the tablerApp level)
# ---------------------------------------------------------------------------
.observe_output <- function(render_obj, send_fn) {
  # Without this, render_obj/send_fn are lazy promises bound to the loop
  # variables in tablerApp()'s `for (nm in ls(output_proxy))` loop; run()
  # (called later via .schedule()) would resolve them using whatever value
  # those loop variables hold *at that later time*, not at the time
  # .observe_output() was called for this particular output.
  force(render_obj)
  force(send_fn)

  run <- function() {
    ctx <- new_context(function() .schedule(run))
    .push_context(ctx)
    tryCatch({
      if (inherits(render_obj, "tabler_render")) {
        if (identical(render_obj$type, "plot")) {
          # Render to SVG so the output is resolution-independent and works for
          # base-R, tinyplot, ggplot2, lattice, and any other grid/base device.
          tmp <- tempfile(fileext = ".svg")
          on.exit(unlink(tmp), add = TRUE)
          local({
            grDevices::svg(tmp,
                           width  = render_obj$width  / 96,  # px -> inches
                           height = render_obj$height / 96)
            on.exit(
              if (grDevices::dev.cur() > 1L) grDevices::dev.off(),
              add = TRUE
            )
            eval(render_obj$expr, render_obj$env)
          })
          svg_txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
          if (!nzchar(svg_txt))
            stop("renderPlot produced an empty graphic")
          val  <- svg_txt
          type <- "plot_src"
        } else if (identical(render_obj$type, "print")) {
          # Capture printed output (cat(), print(), str(), ...) rather than
          # the expression's return value, which is often invisible NULL.
          val <- paste(
            utils::capture.output(eval(render_obj$expr, render_obj$env)),
            collapse = "\n"
          )
          type <- "print"
        } else {
          val  <- eval(render_obj$expr, render_obj$env)
          type <- render_obj$type
        }
      } else if (is.function(render_obj)) {
        # External render function from any htmlwidgets-based package
        # (render_d3po, renderWidget, leaflet::renderLeaflet, ...).
        # We never call Shiny internals directly.  Priority order:
        #
        #  1. tabler_expr / tabler_env attributes - explicit opt-in kept for
        #     backward compatibility (e.g. renderWidget() in outputs.R).
        #
        #  2. origUserFunc - the un-wrapped user closure stored by
        #     shiny::createRenderFunction (which htmlwidgets::shinyRenderWidget
        #     delegates to).  It takes no arguments, evaluates the raw expression
        #     in the original env, and reactive reads inside it are tracked
        #     normally because our context is already pushed above.
        #
        #  3. Direct call - last resort; will fail for true Shiny render fns
        #     that require a session, but works for simple zero-arg wrappers.
        expr <- attr(render_obj, "tabler_expr", exact = TRUE)
        env  <- attr(render_obj, "tabler_env",  exact = TRUE)
        if (!is.null(expr)) {
          val <- eval(expr, env)
        } else {
          ouf <- environment(render_obj)[["origUserFunc"]]
          if (is.function(ouf)) {
            val <- ouf()
          } else {
            val <- tryCatch(
              render_obj(),
              error = function(e) {
                stop("Could not evaluate widget render function without a ",
                     "Shiny session. Use renderWidget() or any render helper ",
                     "built on htmlwidgets::shinyRenderWidget.\n",
                     "Original error: ", conditionMessage(e))
              }
            )
          }
        }
        type <- "widget"
      } else {
        stop("output must be a tabler render function")
      }
      .pop_context()
      send_fn(val, type)
    }, tabler_req_stop = function(e) {
      .pop_context()
      invisible(NULL)
    }, error = function(e) {
      .pop_context()
      send_fn(
        paste0('<span class="text-danger">Error: ', html_escape(conditionMessage(e)), "</span>"),
        "html"
      )
    })
  }
  .schedule(run)
  invisible(run)
}
