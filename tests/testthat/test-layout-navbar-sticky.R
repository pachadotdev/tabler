test_that("layout_navbar_sticky converts aside to sticky header and sets dropdown attrs", {
  # construct an aside with a dropdown and a theme toggle
  a_dropdown <- a(href = "#", `data-bs-toggle` = "dropdown", "Drop")
  li_dd <- li(class = "nav-item", a_dropdown)

  theme_a <- a(href = "#", class = "theme-toggle", "Theme")
  theme_li <- li(class = "mt-auto", div(theme_a))

  ul <- ul(li_dd, theme_li)

  aside_tag <- aside(
    class = "navbar navbar-vertical",
    div(
      class = "container-fluid",
      div(class = "collapse navbar-collapse", id = "sidebar-menu", ul)
    )
  )

  out <- layout_navbar_sticky(aside_tag, NULL, body("BODY"), NULL)
  s <- as.character(out)

  expect_true(grepl('aria-expanded="false"', s))
  expect_true(grepl('data-bs-auto-close="outside"', s))
  expect_true(grepl("BODY", s))
})
