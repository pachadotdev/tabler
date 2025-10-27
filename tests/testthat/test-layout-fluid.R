test_that("layout_fluid extracts nav items and theme buttons from aside", {
  # Build an aside-like navbar with a container-fluid, collapse, ul with items and a theme li
  a_dropdown <- a(href = "#", `data-bs-toggle` = "dropdown", "Menu")
  li_dd <- li(class = "nav-item dropdown", a_dropdown)

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

  out <- layout_fluid(aside_tag, NULL, body("BODY"), NULL)
  out_str <- as.character(out)

  # Dropdown should have aria-expanded and data-bs-auto-close set to false/outside in the produced nav_items
  expect_true(grepl('aria-expanded="false"|data-bs-auto-close="outside"', out_str))

  # Body content must be present
  expect_true(grepl("BODY", out_str))
})
