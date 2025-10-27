test_that("build_vertical_simple_layout produces expected structure", {
  # Create a simple sidebar and body
  # Build an <aside> wrapper around the sidebar_menu so layout builders
  # recognize it as a vertical sidebar (as produced by navbar_menu(..., brand=...)).
  sidebar_ul <- sidebar_menu(menu_item("Side", tab_name = "side"))
  sidebar <- shiny::tags$aside(
    class = "navbar navbar-vertical",
    shiny::tags$div(
      class = "container-fluid",
      shiny::tags$div(class = "collapse navbar-collapse", id = "sidebar-menu", sidebar_ul)
    )
  )
  body <- tabler_body("main")
  out <- layout_vertical(list(side = sidebar), NULL, body, NULL)
  expect_s3_class(out, "shiny.tag.list")
  # sidebar should be present in the output
  out_str <- as.character(out)
  expect_true(grepl("navbar-vertical", out_str))
  expect_true(grepl("main", out_str))
})

test_that("build_fluid_vertical_layout delegates to vertical builder", {
  sb_ul <- sidebar_menu(menu_item("S", tab_name = "s"))
  sb <- shiny::tags$aside(
    class = "navbar navbar-vertical",
    shiny::tags$div(
      class = "container-fluid",
      shiny::tags$div(class = "collapse navbar-collapse", id = "sidebar-menu", sb_ul)
    )
  )
  b <- tabler_body("B")
  out <- layout_fluid_vertical(list(side = sb), NULL, b, NULL)
  expect_s3_class(out, "shiny.tag.list")
  # fluid vertical should still contain the sidebar and body
  out_str <- as.character(out)
  expect_true(grepl("navbar-vertical", out_str))
  expect_true(grepl("B", out_str))
})

test_that("build_horizontal_layout includes topbar when provided", {
  topbar <- topbar(title = "A")
  b <- tabler_body("HB")
  out_with <- layout_horizontal(topbar, NULL, b, NULL)
  expect_s3_class(out_with, "shiny.tag.list")
  # horizontal layout should include the topbar content
  out_with_str <- as.character(out_with)
  expect_true(grepl("navbar", out_with_str))
  expect_true(grepl("HB", out_with_str))
})

test_that("build_navbar_overlap_layout adjusts navbar attributes", {
  # Provide an <aside>-style navbar source so the builder will transform it
  nav <- navbar_menu(menu_item("N", tab_name = "n"), brand = sidebar_brand(text = "Brand"))
  out <- layout_navbar_overlap(nav, NULL, tabler_body("X"), NULL)
  expect_s3_class(out, "shiny.tag.list")
  # navbar-overlap should appear and dark theme attr set
  out_str <- as.character(out)
  expect_true(grepl("navbar-overlap", out_str))
  expect_true(grepl("data-bs-theme=\"dark\"|data-bs-theme=\\\"dark\\\"", out_str))
})

test_that("build_navbar_dark_layout sets dark theme on navbar", {
  nav <- navbar_menu(menu_item("N", tab_name = "n"), brand = sidebar_brand(text = "Brand"))
  out <- layout_navbar_dark(nav, NULL, tabler_body("Y"), NULL)
  expect_s3_class(out, "shiny.tag.list")
  out_str <- as.character(out)
  expect_true(grepl("data-bs-theme=\"dark\"|data-bs-theme=\\\"dark\\\"", out_str))
  expect_true(grepl("Y", out_str))
})

test_that("build_navbar_sticky_layout wraps navbar in sticky-top", {
  nav <- navbar_menu(menu_item("Sticky", tab_name = "sticky"), brand = sidebar_brand(text = "Brand"))
  out <- layout_navbar_sticky(nav, NULL, tabler_body("Z"), NULL)
  expect_s3_class(out, "shiny.tag.list")
  # should include sticky-top class
  out_str <- as.character(out)
  expect_true(grepl("sticky-top", out_str))
  expect_true(grepl("Z", out_str))
})

test_that("build_condensed_layout places navbar directly and includes wrapper", {
  nav <- topbar(title = "CNav")
  out <- layout_condensed(nav, NULL, tabler_body("CB"), NULL)
  expect_s3_class(out, "shiny.tag.list")
  # navbar should appear directly in page and page-wrapper present
  expect_true(grepl("CNav", paste0(out)))
  expect_true(grepl("page-wrapper", paste0(out)))
  expect_true(grepl("CB", paste0(out)))
})
