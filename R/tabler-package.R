#' Tabler: Create Dashboards with Tabler and Shiny
#'
#' @description
#' The tabler package provides a simplified way to create beautiful dashboards
#' using the modern Tabler CSS framework with Shiny. This package offers a
#' clean, consistent API with essential components for building professional
#' web applications.
#'
#' @section Main Functions:
#'
#' Core dashboard structure:
#' \itemize{
#'   \item \code{\link{tablerPage}}: Main dashboard page
#'   \item \code{\link{tablerBody}}: Dashboard body container
#'   \item \code{\link{tablerNavbar}}: Navigation bar
#'   \item \code{\link{tablerFooter}}: Footer component
#' }
#'
#' UI Components:
#' \itemize{
#'   \item \code{\link{tablerCard}}: Card component
#'   \item \code{\link{tablerValueBox}}: Value/metric display boxes
#'   \item \code{\link{tablerIcon}}: Icon component
#'   \item \code{\link{tablerProgress}}: Progress bars
#'   \item \code{\link{tablerAlert}}: Alert/notification messages
#'   \item \code{\link{tablerButton}}: Styled buttons
#' }
#'
#' @examples
#' if (interactive()) {
#' }
#'
#' @keywords internal
"_PACKAGE"

#' Add Tabler Dependencies
#'
#' Attach necessary CSS and JavaScript dependencies for Tabler dashboard
#'
#' @param tag HTML tag object to attach dependencies to
#' @param layout Layout type (for RTL-specific CSS)
#' @return HTML tag with dependencies attached
#' @keywords internal
add_deps <- function(tag, layout = "default") {
  # Determine if RTL CSS is needed
  is_rtl <- layout == "rtl"

  # Core dependencies
  deps <- list(
    # Tabler Core CSS and JS (mandatory)
    htmltools::htmlDependency(
      name = "tabler-core",
      version = "1.4.0",
      src = c(file = system.file("tabler-1.4.0", package = "tabler")),
      stylesheet = if (!is_rtl) "css/tabler.min.css" else "css/tabler.rtl.min.css",
      script = "js/tabler.min.js"
    ),

    # Tabler Plugin Styles
    htmltools::htmlDependency(
      name = "tabler-plugins",
      version = "1.4.0",
      src = c(file = system.file("tabler-1.4.0", package = "tabler")),
      stylesheet = c(
        "css/tabler-socials.min.css",
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
