#' @useDynLib tabler, .registration = TRUE
#' @keywords internal
"_PACKAGE"

# Register S3 methods for operator generics at load time.
# Doing this via .onLoad avoids roxygen2 generating unquoted `S3method($,...)`
# entries in NAMESPACE, which R's parser rejects.
.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  registerS3method("$",            "ReactiveValues",  .rv_dollar,               envir = ns)
  registerS3method("$<-",          "ReactiveValues",  .rv_dollar_assign,        envir = ns)
  registerS3method("$",            "tabler.module.input",  .mi_dollar,          envir = ns)
  registerS3method("$<-",          "tabler.module.input",  .mi_dollar_assign,   envir = ns)
  registerS3method("$",            "tabler.module.output", .mo_dollar,          envir = ns)
  registerS3method("$<-",          "tabler.module.output", .mo_dollar_assign,   envir = ns)
  registerS3method("as.character", "tabler.tag",       .as_char_tag,      envir = ns)
  registerS3method("as.character", "tabler.tag.list",  .as_char_tag_list, envir = ns)
}

#' @title Add Tabler Dependencies
#' @description Return link/script tags that load Tabler CSS/JS.  The httpuv
#'   server in \code{\link{tablerApp}} serves these paths directly from the
#'   package's installed \code{inst/} directory.
#' @param layout Layout type (for RTL-specific CSS).
#' @return A tagList of \code{<link>} and \code{<script>} tags.
#' @keywords internal
#' @noRd
add_deps <- function(layout = "default") {
  is_rtl   <- layout == "rtl"
  css_file <- if (!is_rtl) "css/tabler.min.css" else "css/tabler.rtl.min.css"

  tagList(
    tags$link(rel = "stylesheet", href = paste0("/tabler-1.4.0/", css_file)),
    tags$link(rel = "stylesheet", href = "/tabler-icons-3.55.0/tabler-icons.min.css"),
    tags$link(rel = "stylesheet", href = "/tabler-1.4.0/css/tabler-socials.min.css"),
    tags$link(rel = "stylesheet", href = "/tabler-1.4.0/css/tabler-themes.min.css"),
    tags$link(rel = "stylesheet", href = "/css/tabler-progress.css"),
    tags$link(rel = "stylesheet", href = "/css/tabler-range2.css"),
    tags$script(src = "/tabler-1.4.0/js/tabler.min.js"),
    tags$script(src = "/tabler-1.4.0/js/tabler-theme.min.js"),
    tags$script(src = "/js/tabler-tabs.js"),
    tags$script(src = "/js/tabler-reactive.js"),
    tags$script(src = "/js/tabler-visibility.js"),
    tags$script(src = "/js/tabler-update-input.js"),
    tags$script(src = "/js/tabler-progress.js")
  )
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

# Internal tag aliases used throughout the layout/component files.
# All come from R/html.R (our pure-R tag system — no external dependency).
#' @noRd
ul         <- tags$ul
#' @noRd
li         <- tags$li
#' @noRd
aside      <- tags$aside
#' @noRd
head       <- tags$head
#' @noRd
script     <- tags$script
#' @noRd
i          <- tags$i
#' @noRd
meta       <- tags$meta
#' @noRd
href       <- tags$href
#' @noRd
nav        <- tags$nav
#' @noRd
body_tag   <- tags$body
#' @noRd
footer_tag <- tags$footer
#' @noRd
button_tag <- tags$button
