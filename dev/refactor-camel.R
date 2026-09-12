finp <- list.files(".", pattern = "^*.*\\.R$", full.names = T, recursive = T)

for (f in finp) {
  parsed_code <- parse(f, keep.source = TRUE)
  parse_data <- utils::getParseData(parsed_code)

  # Filter for camelCase identifiers (e.g., myVariable, camelCase)
  camel_case_tokens <- parse_data[
    parse_data$token == "SYMBOL" & 
    grepl("^[a-z]+[A-Z][a-zA-Z0-9]*$", parse_data$text), 
  ]

  if (nrow(camel_case_tokens) == 0L) next

  print("=======================================")
  print(f)

  print(camel_case_tokens[, c("line1", "col1", "text")])
  print("=======================================")
}
