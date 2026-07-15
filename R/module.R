# Module id-namespacing helpers (dependency-free equivalent of shiny::NS()).

#' @title Namespace an Id
#' @description Prefixes an id with a namespace, mirroring the id-prefixing
#'   behaviour of \code{shiny::NS()} without depending on \pkg{shiny} or its
#'   module system.
#' @param namespace Namespace prefix.
#' @param id Id to namespace. If missing, returns a function that namespaces
#'   any id it is given (as \code{shiny::NS()} does).
#' @return If \code{id} is given, a namespaced id (character string).
#'   Otherwise, a function that namespaces any id passed to it.
#' @examples
#' ns <- NS("mymodule")
#' ns("button") # "mymodule-button"
#' NS("mymodule", "button") # "mymodule-button"
#' @export
NS <- function(namespace, id = NULL) {
  if (is.null(id)) {
    function(id) paste(namespace, id, sep = "-")
  } else {
    paste(namespace, id, sep = "-")
  }
}

# ---------------------------------------------------------------------------
# moduleServer() — dependency-free equivalent of shiny::moduleServer().
# ---------------------------------------------------------------------------

#' @title The Currently Running tablerApp Session
#' @description Returns the \code{session} object of the \code{tablerApp}
#'   server function currently executing, or \code{NULL} if none is running.
#'   Used as the default \code{session} argument of \code{\link{moduleServer}}.
#' @return A \code{tablerApp} session object, or \code{NULL}.
#' @export
getDefaultReactiveDomain <- function() {
  .domain$current_session
}

# Scoped (module) input proxy — wraps a parent reactiveValues() store so that
# unprefixed names are read/written under the module's namespace.
.mi_dollar <- function(x, name) {
  x <- unclass(x)
  .rv_dollar(x$.parent, x$.ns(name))
}
.mi_dollar_assign <- function(x, name, value) {
  raw <- unclass(x)
  .rv_dollar_assign(raw$.parent, raw$.ns(name), value)
  x
}

# Scoped (module) output proxy — wraps a parent output environment so that
# unprefixed names are read/written under the module's namespace.
.mo_dollar <- function(x, name) {
  x <- unclass(x)
  get0(x$.ns(name), envir = x$.parent, ifnotfound = NULL)
}
.mo_dollar_assign <- function(x, name, value) {
  raw <- unclass(x)
  assign(raw$.ns(name), value, envir = raw$.parent)
  x
}

#' @title Create a Namespaced Module Server
#' @description Dependency-free equivalent of \code{shiny::moduleServer()}.
#'   Wraps \code{input}/\code{output} so that unprefixed names accessed
#'   inside \code{module} are automatically namespaced with \code{id},
#'   matching the ids produced by \code{NS(id)} in the module's UI function.
#' @param id The module's namespace id (must match the id used to call the
#'   module's \code{*_ui} function).
#' @param module A function with signature \code{function(input, output, session)}.
#' @param session The parent session object. Defaults to the currently
#'   running \code{\link{tablerApp}} session (see \code{\link{getDefaultReactiveDomain}}).
#' @return The return value of \code{module}, invisibly.
#' @export
moduleServer <- function(id, module, session = getDefaultReactiveDomain()) {
  if (is.null(session)) {
    stop("moduleServer() must be called while a tablerApp server function is running", call. = FALSE)
  }
  ns <- NS(id)

  scoped_input <- structure(
    list(.parent = session$input, .ns = ns),
    class = "tabler.module.input"
  )
  scoped_output <- structure(
    list(.parent = session$output, .ns = ns),
    class = "tabler.module.output"
  )

  scoped_session         <- session
  scoped_session$ns      <- ns
  scoped_session$input   <- scoped_input
  scoped_session$output  <- scoped_output

  # Make getDefaultReactiveDomain() (and functions defaulting to it, like
  # show()/hide()/showProgress()) resolve to this module's scoped session
  # for the duration of the module's own execution, mirroring shiny's
  # withReactiveDomain() behaviour inside moduleServer().
  previous_session <- .domain$current_session
  .domain$current_session <- scoped_session
  on.exit(.domain$current_session <- previous_session, add = TRUE)

  invisible(module(scoped_input, scoped_output, scoped_session))
}
