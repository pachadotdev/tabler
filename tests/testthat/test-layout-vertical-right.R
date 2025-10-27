test_that("layout_vertical_right appends classes and data-bs-theme and filters theme li", {
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

  out <- layout_vertical_right(aside_tag, NULL, body("Z"), NULL, show_theme_button = FALSE)
  s <- as.character(out)

  # class navbar-end and navbar-expand-lg and data-bs-theme dark should be present
  expect_true(grepl("navbar-end", s))
  expect_true(grepl('data-bs-theme="dark"', s))
})
