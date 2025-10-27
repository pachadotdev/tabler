test_that("layout_navbar_overlap sets data-bs-theme dark and adds text-white to page-header", {
  brand_div <- div(class = "navbar-brand", "Brand")
  li_item <- li(a(href = "#", "OverlapItem"))
  ul <- ul(li_item)

  aside_tag <- aside(
    class = "navbar navbar-vertical",
    div(class = "container-fluid", brand_div, div(class = "collapse navbar-collapse", id = "sidebar-menu", ul))
  )

  # Body with a page-header div that should receive text-white
  page_header <- div(class = "page-header", "Head")
  body <- list(page_header, div(class = "page-body", "Content"))

  out <- layout_navbar_overlap(aside_tag, NULL, body, NULL, show_theme_button = FALSE)
  s <- as.character(out)

  expect_true(grepl('data-bs-theme="dark"', s))
  expect_true(grepl("text-white", s))
  expect_true(grepl("OverlapItem", s))
})
