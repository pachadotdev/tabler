test_that("layout_boxed combo and boxed variants behave correctly", {
  body <- body("body-content")

  # combo variant: list with top and side
  top <- topbar(title = "TTop")
  side <- sidebar_menu(menu_item("SI", tab_name = "si"))
  combo <- layout_boxed(list(top = top, side = side), NULL, body, NULL)
  combo_str <- as.character(combo)
  expect_true(grepl("page", combo_str))
  expect_true(grepl("SI|navbar-vertical", combo_str))

  # boxed variant: header with brand
  boxed_nav <- navbar_menu(menu_item("Home", tab_name = "home"), brand = sidebar_brand(text = "B", img = "i.png"))
  boxed <- layout_boxed(boxed_nav, NULL, body, NULL)
  boxed_str <- as.character(boxed)
  expect_true(grepl("page-wrapper|page-body", boxed_str))
  expect_true(grepl("B|Home|navbar-brand-image", boxed_str))
})
