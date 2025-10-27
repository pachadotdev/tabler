test_that("layout_fluid_vertical sets data-bs-theme dark and respects show_theme_button flag", {
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

  out <- layout_fluid_vertical(aside_tag, NULL, body("B"), NULL, show_theme_button = TRUE)
  s <- as.character(out)
  expect_true(grepl('data-bs-theme="dark"', s))
  expect_true(grepl("navbar-expand-lg", s))
})
