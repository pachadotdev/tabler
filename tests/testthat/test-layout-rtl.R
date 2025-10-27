test_that("layout_rtl produces dir=rtl and places brand inside ul and theme inside ul", {
  brand_tag <- img(src = "b.png")
  brand_div <- div(class = "navbar-brand", brand_tag)

  li_item <- li(a(href = "#", "ItemR"))
  theme_li <- li(class = "mt-auto", a(href = "#", "T"))
  ul <- ul(li_item, theme_li)

  aside_tag <- aside(
    class = "navbar navbar-vertical",
    div(class = "container-fluid", brand_div, div(class = "collapse navbar-collapse", id = "sidebar-menu", ul))
  )

  out <- layout_rtl(aside_tag, NULL, body("R"), NULL, show_theme_button = TRUE)
  s <- as.character(out)

  expect_true(grepl('dir="rtl"', s))
  expect_true(grepl("ItemR", s))
  expect_true(grepl("navbar-brand", s))
})
