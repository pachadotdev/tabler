# pattern:
# .ti-brand-github:before {
#     content: "\ec1c"
# }
# .ti-brand-github-copilot:before {
#     content: "\f4a8"
# }
# .ti-brand-github-filled:before {
#     content: "\f7e7"
# }
# => use regex to create a txt with ti-{ ICON NAME}:before => ICON NAME

library(stringr)
library(dplyr)

icons <- readLines("dev/icons.css")
icons <- str_match(icons, "\\.ti-([a-z0-9-]+):before")[,2]
icons <- icons[!is.na(icons)]

icons <- as_tibble(icons) %>%
  rename(icon = value)

# root: what's to the left of the 1st hyphen
icons <- icons %>%
  mutate(root = str_extract(icon, "^[a-z0-9]+"))

# filled: T/F if ends with -filled
icons <- icons %>%
  mutate(filled = ifelse(str_detect(icon, "-filled$"), TRUE, FALSE))

icons <- icons %>%
  select(root, icon, filled)

icons %>%
    filter(root == "brand")

icons %>%
    filter(root == "paw")

icons <- icons %>%
  mutate(root = as.factor(root))

icons %>%
    count(root) %>%
    filter(n > 100)

use_data(icons, overwrite = TRUE, compress = "xz")
