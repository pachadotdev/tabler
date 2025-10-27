test_that("layout_horizontal respects show_theme_button = FALSE and skips theme items", {
  theme_a <- a(href = "#", class = "theme-toggle", "Tabler Unit Testing")
  theme_li <- li(class = "mt-auto", theme_a)
  ul <- ul(theme_li)

  aside_tag <- aside(
    class = "navbar navbar-vertical",
    div(
      class = "container-fluid",
      div(class = "collapse navbar-collapse", id = "sidebar-menu", ul)
    )
  )

  out_with <- layout_horizontal(aside_tag, NULL, body("H"), NULL, show_theme_button = TRUE)
  out_without <- layout_horizontal(aside_tag, NULL, body("H"), NULL, show_theme_button = FALSE)

  expect_true(grepl("Tabler Unit Testing", as.character(out_with)))
  # When show_theme_button is FALSE the theme link should not be present
  expect_false(grepl("Tabler Unit Testing", as.character(out_without)))
})
