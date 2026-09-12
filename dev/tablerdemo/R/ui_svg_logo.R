#' SVG Logo (UI)
#' @noRd
svg_data_uri <- function() {
  paste0(
    "data:image/svg+xml;utf8,",
    utils::URLencode(
      paste(
        readLines(
          system.file("extdata", "app-template", "tabler-logo.svg", package = "tabler"),
          warn = FALSE
        ),
        collapse = "\n"
      ),
      reserved = TRUE
    )
  )
}
