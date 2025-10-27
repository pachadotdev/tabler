test_that("layout_navbar_dark produces header with data-bs-theme dark and extracts theme item when shown", {
  a_dropdown <- a(href = "#", `data-bs-toggle` = "dropdown", "D")
  li_dd <- li(class = "nav-item", a_dropdown)

  theme_a <- a(href = "#", class = "theme-toggle", "ThemeDark")
  theme_li <- li(class = "mt-auto", div(theme_a))

  ul <- ul(li_dd, theme_li)

  aside_tag <- aside(
    class = "navbar navbar-vertical",
    div(
      class = "container-fluid",
      div(class = "collapse navbar-collapse", id = "sidebar-menu", ul)
    )
  )

  out_with <- layout_navbar_dark(aside_tag, NULL, body("B"), NULL, show_theme_button = TRUE)
  out_without <- layout_navbar_dark(aside_tag, NULL, body("B"), NULL, show_theme_button = FALSE)

  expect_true(grepl('data-bs-theme="dark"', as.character(out_with)))
  expect_true(grepl('aria-expanded="false"', as.character(out_with)))
  expect_false(grepl("ThemeDark", as.character(out_without)))
})
