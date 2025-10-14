#' Add Tabler Dependencies
#'
#' Attach necessary CSS and JavaScript dependencies for Tabler dashboard
#'
#' @param tag HTML tag object to attach dependencies to
#' @return HTML tag with dependencies attached
#' @keywords internal
add_deps <- function(tag) {
  # Core dependencies
  deps <- list(
    # Tabler Core CSS and JS (mandatory)
    htmltools::htmlDependency(
      name = "tabler-core",
      version = "1.4.0",
      src = c(file = system.file("tabler-1.4.0", package = "tabler")),
      stylesheet = "css/tabler.min.css",
      script = "js/tabler.min.js"
    ),

    # Tabler Plugin Styles
    htmltools::htmlDependency(
      name = "tabler-plugins",
      version = "1.4.0",
      src = c(file = system.file("tabler-1.4.0", package = "tabler")),
      stylesheet = c(
        "css/tabler-flags.min.css",
        "css/tabler-socials.min.css",
        "css/tabler-payments.min.css",
        "css/tabler-vendors.min.css",
        "css/tabler-marketing.min.css",
        "css/tabler-themes.min.css"
      )
    ),

    # Tabler Theme Script
    htmltools::htmlDependency(
      name = "tabler-theme",
      version = "1.4.0",
      src = c(file = system.file("tabler-1.4.0", package = "tabler")),
      script = "js/tabler-theme.min.js"
    )
  )

  htmltools::attachDependencies(tag, deps)
}

#' Check if Tabler package dependencies are available
#'
#' @return TRUE if all required dependencies are available
#' @export
check_deps <- function() {
  required_packages <- c("shiny", "htmltools")
  missing <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

  if (length(missing) > 0) {
    message("Missing required packages: ", paste(missing, collapse = ", "))
    return(FALSE)
  }

  TRUE
}
