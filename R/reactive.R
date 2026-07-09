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
# Reactive source — tracks which contexts depend on it
# ---------------------------------------------------------------------------
new_source <- function() {
  deps <- new.env(hash = TRUE, parent = emptyenv())

  list(
    depend = function() {
      ctx <- .current_context()
      if (is.null(ctx)) return(invisible(NULL))
      id <- as.character(ctx$id)
      if (exists(id, envir = deps, inherits = FALSE)) return(invisible(NULL))
      assign(id, ctx, envir = deps)
      ctx$on_invalidate(function() {
        if (exists(id, envir = deps, inherits = FALSE)) rm(list = id, envir = deps)
      })
      invisible(NULL)
    },
    invalidate_dependents = function() {
      ctxs <- as.list(deps)
      rm(list = ls(deps), envir = deps)
      for (ctx in ctxs) ctx$invalidate()
    }
  )
}

# ---------------------------------------------------------------------------
# Reactive context — can be invalidated; runs cleanup callbacks on invalidation
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
    fn()
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

  function() {
    src$depend()
    if (!invalid) return(cached)

    ctx <- new_context(function() {
      invalid <<- TRUE
      src$invalidate_dependents()
    })
    .push_context(ctx)
    tryCatch(
      { cached <<- eval(expr_q, env); invalid <<- FALSE },
      finally = .pop_context()
    )
    cached
  }
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

  run <- function() {
    if (suspended) return(invisible(NULL))
    ctx <- new_context(function() {
      if (!suspended) .schedule(run)
    })
    .push_context(ctx)
    tryCatch(eval(expr_q, env), finally = .pop_context())
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
  init_done <- !isTRUE(ignoreInit)

  observe({
    eval(event_q, env)          # read to register dependency
    if (!init_done) { init_done <<- TRUE; return(invisible(NULL)) }
    isolate(eval(handler_q, env))
  })
}

# ---------------------------------------------------------------------------
# Internal: observer for output renderers (avoids NSE at the tablerApp level)
# ---------------------------------------------------------------------------
.observe_output <- function(render_obj, send_fn) {
  run <- function() {
    ctx <- new_context(function() .schedule(run))
    .push_context(ctx)
    tryCatch({
      val <- eval(render_obj$expr, render_obj$env)
      .pop_context()
      send_fn(val, render_obj$type)
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
