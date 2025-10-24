#' @keywords internal
"_PACKAGE"

#' @title Add Tabler Dependencies
#' @description Attach necessary CSS and JavaScript dependencies for Tabler dashboard
#' @param tag HTML tag object to attach dependencies to
#' @param layout Layout type (for RTL-specific CSS)
#' @return HTML tag with dependencies attached
#' @source \url{https://github.com/tabler/}
#' @keywords internal
#' @noRd
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

    # Tabler Icons (webfont)
    htmltools::htmlDependency(
      name = "tabler-icons",
      version = "3.55.0",
      src = c(file = system.file("tabler-icons-3.55.0", package = "tabler")),
      stylesheet = "tabler-icons.min.css"
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
    ),

    # Tabler Tabs Script (for tab switching)
    htmltools::htmlDependency(
      name = "tabler-tabs",
      version = "1.0.0",
      src = c(file = system.file(package = "tabler")),
      script = "js/tabler-tabs.js"
    )
  )

  htmltools::attachDependencies(tag, deps)
}

#' @title Tabler Icons Data
#' @description A dataset containing Tabler icon names and their categories.
#' @format A data frame with 6,019 rows and 3 variables:
#' \describe{
#'  \item{root}{The root category of the icon (e.g., "arrow/arrows", "brand", "device", etc.)}
#'  \item{icon}{The full icon name (e.g., "arrow-up", "arrow-down", "circle-arrow-up", "circle-arrow-up-filled", etc.)}
#'  \item{filled}{Logical indicating whether the icon is a filled or not.}
#' }
#' @source Derived from the icons from \url{https://github.com/tabler/tabler-icons}
#' @examples
#' icons[grepl("paw", icons$icon), ]
#' @keywords datasets
"icons"
