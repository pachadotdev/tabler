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
add_deps <- function(tag, layout = "default", theme = "light", color = "minimal") {
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

    # Tabler Theme Script
    htmltools::htmlDependency(
      name = "tabler-theme",
      version = "1.4.0",
      src = c(file = system.file("tabler-1.4.0", package = "tabler")),
      script = "js/tabler-theme.min.js"
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

#' Re-Export Functions for Internal Use
#' @keywords internal
#' @rdname re-exports
#' @importFrom htmltools div a img tags span p h2 h3 h4 HTML tagList tagAppendAttributes
#' @noRd
ul <- tags$ul

#' Re-Export Functions for Internal Use
#' @keywords internal
#' @rdname re-exports
#' @noRd
li <- tags$li

#' Re-Export Functions for Internal Use
#' @keywords internal
#' @rdname re-exports
#' @noRd
header <- tags$header

#' Re-Export Functions for Internal Use
#' @keywords internal
#' @rdname re-exports
#' @noRd
aside <- tags$aside

#' Re-Export Functions for Internal Use
#' @keywords internal
#' @rdname re-exports
#' @noRd
head <- tags$head

#' Re-Export Functions for Internal Use
#' @keywords internal
#' @rdname re-exports
#' @noRd
script <- tags$script

#' Re-Export Functions for Internal Use
#' @keywords internal
#' @rdname re-exports
#' @noRd
i <- tags$i

#' Re-Export Functions for Internal Use
#' @keywords internal
#' @rdname re-exports
#' @noRd
meta <- tags$meta

#' Re-Export Functions for Internal Use
#' @keywords internal
#' @rdname re-exports
#' @noRd
href <- tags$href

#' Re-Export Functions for Internal Use
#' @keywords internal
#' @rdname re-exports
#' @noRd
nav <- tags$nav

#' Re-Export Functions for Internal Use
#' @keywords internal
#' @rdname re-exports
#' @noRd
body_tag <- tags$body

#' Re-Export Functions for Internal Use
#' @keywords internal
#' @rdname re-exports
#' @noRd
footer_tag <- tags$footer

#' Re-Export Functions for Internal Use
#' @keywords internal
#' @rdname re-exports
#' @noRd
button_tag <- tags$button
