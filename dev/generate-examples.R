this_file <- normalizePath(sys.frames()[[1]]$ofile %||% "dev/generate-examples.R")

pkg_root <- normalizePath(file.path(dirname(this_file), ".."), mustWork = TRUE)

load_all()

library(htmltools)

layouts <- c("boxed", "combo", "condensed", "fluid-vertical", "fluid", "horizontal", "navbar-dark",
                "navbar-overlap", "navbar-sticky", "rtl", "vertical-right", "vertical-transparent",
                "vertical")

out_dir <- file.path(pkg_root, "official-examples")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

for (layout in layouts) {
  page <- tabler_page(title = "Tabler Demo", navbar = NULL, body = tabler_body(shiny::tags$div("Hello world")), footer = tabler_footer(left = "Left"), layout = layout)
  html <- as.character(htmltools::browsable(page))
  out_file <- file.path(out_dir, paste0("r-out-", layout, ".html"))
  writeLines(html, con = out_file)
  message("Wrote ", out_file)
}
