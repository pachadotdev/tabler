# Module id-namespacing helpers (dependency-free equivalent of shiny::NS()).

#' @title Namespace an Id
#' @description Prefixes an id with a namespace, mirroring the id-prefixing
#'   behaviour of \code{shiny::ns()} without depending on \pkg{shiny} or its
#'   module system.
#' @param namespace Namespace prefix.
#' @param id Id to namespace. If missing, returns a function that namespaces
#'   any id it is given (as \code{shiny::ns()} does).
#' @return If \code{id} is given, a namespaced id (character string).
#'   Otherwise, a function that namespaces any id passed to it.
#' @examples
#' ns <- ns("mymodule")
#' ns("button") # "mymodule-button"
#' ns("mymodule", "button") # "mymodule-button"
#' @export
ns <- function(namespace, id = NULL) {
  if (is.null(id)) {
    function(id) paste(namespace, id, sep = "-")
  } else {
    paste(namespace, id, sep = "-")
  }
}

# ---------------------------------------------------------------------------
# module_server() — dependency-free equivalent of shiny::module_server().
# ---------------------------------------------------------------------------

#' @title The Currently Running tabler_app Session
#' @description Returns the \code{session} object of the \code{tabler_app}
#'   server function currently executing, or \code{NULL} if none is running.
#'   Used as the default \code{session} argument of \code{\link{module_server}}.
#' @return A \code{tabler_app} session object, or \code{NULL}.
#' @export
get_default_reactive_domain <- function() {
  .domain$current_session
}

# Scoped (module) input proxy — wraps a parent reactive_values() store so that
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
#' @description Dependency-free equivalent of \code{shiny::module_server()}.
#'   Wraps \code{input}/\code{output} so that unprefixed names accessed
#'   inside \code{module} are automatically namespaced with \code{id},
#'   matching the ids produced by \code{ns(id)} in the module's UI function.
#' @param id The module's namespace id (must match the id used to call the
#'   module's \code{*_ui} function).
#' @param module A function with signature \code{function(input, output, session)}.
#' @param session The parent session object. Defaults to the currently
#'   running \code{\link{tabler_app}} session (see \code{\link{get_default_reactive_domain}}).
#' @return The return value of \code{module}, invisibly.
#' @export
module_server <- function(id, module, session = get_default_reactive_domain()) {
  if (is.null(session)) {
    stop("module_server() must be called while a tabler_app server function is running", call. = FALSE)
  }
  ns <- ns(id)

  scoped_input <- structure(
    list(.parent = session$input, .ns = ns),
    class = "tabler.module.input"
  )
  scoped_output <- structure(
    list(.parent = session$output, .ns = ns),
    class = "tabler.module.output"
  )

  scoped_session <- session
  scoped_session$ns <- ns
  scoped_session$input <- scoped_input
  scoped_session$output <- scoped_output

  # Make get_default_reactive_domain() (and functions defaulting to it, like
  # show()/hide()/show_progress()) resolve to this module's scoped session
  # for the duration of the module's own execution, mirroring shiny's
  # withReactiveDomain() behaviour inside module_server().
  previous_session <- .domain$current_session
  .domain$current_session <- scoped_session
  on.exit(.domain$current_session <- previous_session, add = TRUE)

  invisible(module(scoped_input, scoped_output, scoped_session))
}
