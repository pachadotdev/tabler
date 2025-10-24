test_that("get_layout_structure routes to correct builders", {
  body <- tabler_body("body-content")

  # combo: navbar as list with top and side
  top <- tabler_navbar(title = "Top")
  side <- tabler_sidebar(title = "Side")
  combo <- get_layout_structure("combo", list(top = top, side = side), body, NULL)
  expect_s3_class(combo, "shiny.tag")
  expect_true(grepl("page", paste0(combo)))
  expect_true(grepl("Side", paste0(combo)))

  # vertical: pass an aside sidebar tag as navbar arg to be detected
  aside <- tabler_sidebar(title = "OnlySide")
  vertical <- get_layout_structure("vertical", aside, body, NULL)
  expect_s3_class(vertical, "shiny.tag")
  expect_true(grepl("OnlySide", paste0(vertical)))

  # vertical-right: ensure sidebar-right class applied
  vr <- get_layout_structure("vertical-right", aside, body, NULL)
  expect_s3_class(vr, "shiny.tag")
  # right class will be in the sidebar when appended
  expect_true(grepl("navbar-right|navbar-vertical", paste0(vr)))

  # horizontal: provide topbar as ul (horizontal menu)
  topbar <- horizontal_menu(menu_item("H1", tab_name = "h1"))
  # when passing a header (tabler_navbar) as navbar it should be treated as top_nav
  horizontal1 <- get_layout_structure("horizontal", tabler_navbar(title = "N"), body, NULL)
  expect_s3_class(horizontal1, "shiny.tag")

  # when passing a ul (horizontal menu) as navbar, it should be treated as topbar
  horizontal2 <- get_layout_structure("horizontal", topbar, body, NULL)
  expect_s3_class(horizontal2, "shiny.tag")
  expect_true(grepl("H1", paste0(horizontal2)))

  # fluid-vertical should use fluid vertical builder
  fv <- get_layout_structure("fluid-vertical", aside, body, NULL)
  expect_s3_class(fv, "shiny.tag")
  expect_true(grepl("OnlySide|body-content", paste0(fv)))

  # navbar-overlap -> should include navbar-overlap class somewhere
  no <- get_layout_structure("navbar-overlap", tabler_navbar("X"), body, NULL)
  expect_s3_class(no, "shiny.tag")
  expect_true(grepl("navbar-overlap|data-bs-theme", paste0(no)))

  # navbar-dark -> should include data-bs-theme attribute
  nd <- get_layout_structure("navbar-dark", tabler_navbar("D"), body, NULL)
  expect_s3_class(nd, "shiny.tag")
  expect_true(grepl("data-bs-theme=\"dark\"|data-bs-theme=\\\"dark\\\"", paste0(nd)))

  # navbar-sticky -> should include sticky-top
  ns <- get_layout_structure("navbar-sticky", tabler_navbar("S"), body, NULL)
  expect_s3_class(ns, "shiny.tag")
  expect_true(grepl("sticky-top", paste0(ns)))

  # condensed -> condensed layout structure
  cd <- get_layout_structure("condensed", tabler_navbar("C"), body, NULL)
  expect_s3_class(cd, "shiny.tag")
  expect_true(grepl("page-wrapper", paste0(cd)))

  # vertical-transparent should behave like vertical and include sidebar when provided
  vt <- get_layout_structure("vertical-transparent", aside, body, NULL)
  expect_s3_class(vt, "shiny.tag")
  expect_true(grepl("OnlySide|navbar-vertical|page-wrapper", paste0(vt)))
})
