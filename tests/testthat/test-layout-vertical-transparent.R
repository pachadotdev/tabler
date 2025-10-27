test_that("layout_vertical_transparent appends classes and filters theme when requested", {
  theme_a <- a(href = "#", class = "theme-toggle", "T")
  theme_li <- li(class = "mt-auto", theme_a)
  ul <- ul(theme_li)

  aside_tag <- aside(
    class = "navbar navbar-vertical",
    div(
      class = "container-fluid",
      div(class = "collapse navbar-collapse", id = "sidebar-menu", ul)
    )
  )

  out <- layout_vertical_transparent(aside_tag, NULL, body("B"), NULL, show_theme_button = FALSE)
  s <- as.character(out)

  expect_true(grepl("navbar-transparent", s))
  expect_true(grepl("navbar-expand-lg", s))
})
