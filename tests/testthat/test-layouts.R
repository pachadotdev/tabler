test_that("build_vertical_simple_layout produces expected structure", {
  # Create a simple sidebar and body
  sidebar <- sidebar_menu(menu_item("Side", tab_name = "side"))
  body <- tabler_body("main")
  out <- build_vertical_simple_layout(NULL, sidebar, body, NULL)
  expect_s3_class(out, "shiny.tag")
  # sidebar should be present in the output
  expect_true(grepl("navbar-vertical", paste0(out)))
  expect_true(grepl("main", paste0(out)))
})

test_that("build_fluid_vertical_layout delegates to vertical builder", {
  sb <- sidebar_menu(menu_item("S", tab_name = "s"))
  b <- tabler_body("B")
  out <- build_fluid_vertical_layout(NULL, sb, b, NULL)
  expect_s3_class(out, "shiny.tag")
  # fluid vertical should still contain the sidebar and body
  expect_true(grepl("navbar-vertical", paste0(out)))
  expect_true(grepl("B", paste0(out)))
})

test_that("build_horizontal_layout includes topbar when provided", {
  topbar <- horizontal_menu(menu_item("A", tab_name = "a"))
  b <- tabler_body("HB")
  out_with <- build_horizontal_layout(NULL, topbar, b, NULL)
  expect_s3_class(out_with, "shiny.tag")
  # horizontal layout should include the topbar content
  expect_true(grepl("navbar", paste0(out_with)))
  expect_true(grepl("HB", paste0(out_with)))
})

test_that("build_navbar_overlap_layout adjusts navbar attributes", {
  nav <- topbar(title = "N")
  out <- build_navbar_overlap_layout(nav, NULL, tabler_body("X"), NULL)
  expect_s3_class(out, "shiny.tag")
  # navbar-overlap should appear and dark theme attr set
  expect_true(grepl("navbar-overlap", paste0(out)))
  expect_true(grepl("data-bs-theme=\"dark\"|data-bs-theme=\\\"dark\\\"", paste0(out)))
})

test_that("build_navbar_dark_layout sets dark theme on navbar", {
  nav <- topbar(title = "N")
  out <- build_navbar_dark_layout(nav, NULL, tabler_body("Y"), NULL)
  expect_s3_class(out, "shiny.tag")
  expect_true(grepl("data-bs-theme=\"dark\"|data-bs-theme=\\\"dark\\\"", paste0(out)))
  expect_true(grepl("Y", paste0(out)))
})

test_that("build_navbar_sticky_layout wraps navbar in sticky-top", {
  nav <- topbar(title = "Sticky")
  out <- build_navbar_sticky_layout(nav, NULL, tabler_body("Z"), NULL)
  expect_s3_class(out, "shiny.tag")
  # should include sticky-top class
  expect_true(grepl("sticky-top", paste0(out)))
  expect_true(grepl("Z", paste0(out)))
})

test_that("build_condensed_layout places navbar directly and includes wrapper", {
  nav <- topbar(title = "CNav")
  out <- build_condensed_layout(nav, NULL, tabler_body("CB"), NULL)
  expect_s3_class(out, "shiny.tag")
  # navbar should appear directly in page and page-wrapper present
  expect_true(grepl("CNav", paste0(out)))
  expect_true(grepl("page-wrapper", paste0(out)))
  expect_true(grepl("CB", paste0(out)))
})
