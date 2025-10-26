finp <- list.files("official-examples/preview.tabler.io", pattern = "layout.*html", full.names = TRUE)

try(dir.create("examples/official"))

for (i in seq_along(finp)) {
  # i = 1

  fout <- file.path("examples/official", basename(finp[i]))

  if (file.exists(fout)) {
    next
  }

  # remove text before "<body"
  ui_string <- paste(readLines(finp[i]), collapse = "\n")
  body_start <- regexpr("<body", ui_string)
  ui_string <- substr(ui_string, body_start, nchar(ui_string))

  # remove text after "</body>"
  body_end <- regexpr("</body>", ui_string)
  ui_string <- substr(ui_string, 1, body_end + attr(body_end, "match.length") - 1)

  writeLines(as.character(ui_string), fout)
}
