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
