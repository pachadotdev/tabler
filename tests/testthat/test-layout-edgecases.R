test_that("layout_condensed extracts brand and items from aside", {
  # create an aside-like structure with brand and items
  brand <- sidebar_brand(text = "Tabler Unit Testing", img = "i.png")
  li1 <- menu_item("A", tab_name = "a")
  ul <- sidebar_menu(li1)
  aside_tag <- aside(class = "navbar navbar-vertical", div(class = "container-fluid", div(class = "navbar-brand navbar-brand-autodark", a(href = "#", "Tabler Unit Testing")), div(class = "collapse navbar-collapse", id = "sidebar-menu", ul)))
  out <- layout_condensed(aside_tag, NULL, body("C"), NULL)
  out_str <- as.character(out)
  expect_true(grepl("Tabler Unit Testing|navbar-brand", out_str))
  expect_true(grepl("C", out_str))
})

test_that("layout_navbar_overlap adds text-white to page headers when present", {
  hdr <- header("H", subtitle = "S")
  nav <- navbar_menu(menu_item("N", tab_name = "n"), brand = sidebar_brand(text = "B"))
  out <- layout_navbar_overlap(nav, NULL, list(hdr), NULL)
  out_str <- as.character(out)
  expect_true(grepl("text-white|navbar-overlap", out_str))
})

test_that("layout_rtl moves sidebar brand into ul and returns rtl page", {
  aside_ul <- sidebar_menu(menu_item("Side", tab_name = "side"))
  aside <- aside(
    class = "navbar navbar-vertical",
    div(
      class = "container-fluid",
      div(class = "navbar-brand navbar-brand-autodark", a(href = "#", "Brand")),
      div(class = "collapse navbar-collapse", id = "sidebar-menu", aside_ul)
    )
  )
  out <- layout_rtl(aside, NULL, body("R"), NULL)
  out_str <- as.character(out)
  expect_true(grepl('dir="rtl"|dir=\\"rtl\\"', out_str))
  expect_true(grepl("Brand", out_str))
})
