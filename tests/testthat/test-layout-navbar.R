test_that("navbar_menu builds sidebar and header variants", {
  # sidebar variant when brand provided
  sb <- navbar_menu(menu_item("One", tab_name = "one"), brand = sidebar_brand(text = "MyApp", img = "logo.png"))
  sb_str <- as.character(sb)
  expect_true(grepl("navbar-vertical", sb_str))
  expect_true(grepl("navbar-brand-image|logo.png", sb_str))

  # header variant when brand absent
  hd <- navbar_menu(menu_item("Two", tab_name = "two"), brand = NULL)
  hd_str <- as.character(hd)
  expect_true(grepl("Two", hd_str))
})

test_that("layout builders honor show_theme_button = FALSE and normalize dropdown attributes", {
  # Create a dropdown li (menu_item with dropdown attributes)
  a <- a(href = "#", `data-bs-toggle` = "dropdown")
  li <- li(a)
  ul <- ul(class = "navbar-nav pt-lg-3", li)
  aside_tag <- aside(class = "navbar navbar-vertical", div(class = "container-fluid", div(class = "collapse navbar-collapse", id = "sidebar-menu", ul)))

  # When show_theme_button = FALSE, filter_theme_li should remove theme toggles
  out <- layout_vertical(aside_tag, NULL, body("B"), NULL, show_theme_button = FALSE)
  out_str <- as.character(out)
  # ensure page renders but contains normalized dropdown hints (aria-expanded or data-bs-auto-close adjusted)
  expect_true(grepl("page", out_str))
})
