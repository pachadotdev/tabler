test_that("layout_boxed builds expected structure for boxed and combo", {
  body <- tabler_body("body-content")

  # combo: navbar as list with top and side
  top <- topbar(title = "Top")
  side <- sidebar_menu(menu_item("SideItem", tab_name = "side"))
  combo <- layout_boxed(list(top = top, side = side), NULL, body, NULL)
  expect_s3_class(combo, "shiny.tag")
  expect_true(grepl("page", paste0(combo)))
  expect_true(grepl("SideItem|Side", paste0(combo)))

  # boxed: supply a navbar header
  boxed_nav <- navbar_menu(menu_item("Home", tab_name = "home"), brand = sidebar_brand(text = "Brand"))
  boxed <- layout_boxed(boxed_nav, NULL, body, NULL)
  expect_s3_class(boxed, "shiny.tag")
  expect_true(grepl("page-wrapper|page-body", paste0(boxed)))
  expect_true(grepl("Brand|Home", paste0(boxed)))
})
