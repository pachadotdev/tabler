test_that("layout_condensed uses aside as header and includes brand and theme items properly", {
  brand_tag <- img(src = "logo.png")
  brand_div <- div(class = "navbar-brand", brand_tag)

  theme_a <- a(href = "#", class = "theme-toggle", "T")
  theme_li <- li(class = "mt-auto", theme_a)

  li_item <- li(a(href = "#", "One"))
  ul <- ul(li_item, theme_li)

  aside_tag <- aside(
    class = "navbar navbar-vertical",
    div(class = "container-fluid", brand_div, div(class = "collapse navbar-collapse", id = "sidebar-menu", ul))
  )

  out <- layout_condensed(aside_tag, NULL, body("B"), NULL, show_theme_button = TRUE)
  s <- as.character(out)

  expect_true(grepl("navbar-brand", s))
  expect_true(grepl("theme-toggle", s))
  # Collapsible menu should include the nav item
  expect_true(grepl("One", s))
})
