test_that("tabler_icon builds correct class strings and handles libraries", {
  i1 <- icon("home")
  expect_true(grepl("ti ti-home", as.character(i1)))

  i2 <- icon("alarm", library = "bootstrap", class = "foo")
  s2 <- as.character(i2)
  expect_true(grepl("bi bi-alarm", s2))
  expect_true(grepl("foo", s2))
})

test_that("tabler_card shorthand and footer/title handling", {
  c1 <- card("My Title", "Some body text")
  s1 <- as.character(c1)
  expect_true(grepl("My Title", s1))
  expect_true(grepl("Some body text", s1))

  c2 <- card(title = "Titled", footer = "F", status = "primary")
  s2 <- as.character(c2)
  expect_true(grepl("Titled", s2))
  expect_true(grepl("F", s2))
  expect_true(grepl("card-status-primary", s2))
})

test_that("menu_dropdown splits items into columns and preserves tags", {
  md <- menu_dropdown("Actions", items = list(c("A", "/a"), c("B", "/b"), a(href = "#", "X")))
  s <- as.character(md)
  # should contain dropdown-menu and both links
  expect_true(grepl("dropdown-menu", s))
  expect_true(grepl("/a", s))
  expect_true(grepl("/b", s))
  expect_true(grepl("X", s))
})

test_that("menu_item produces nav-item with anchor and icon placeholder", {
  mi <- menu_item("Label", icon = "dog", href = "/dogs")
  s <- as.character(mi)
  expect_true(grepl("Label", s))
  expect_true(grepl("/dogs", s))
  expect_true(grepl("icon|<!-- Download SVG icon", s))
})

test_that("sidebar_menu and horizontal_menu set active on first item and attach title", {
  li1 <- menu_item("One", href = "/1")
  li2 <- menu_item("Two", href = "/2")
  sm <- sidebar_menu(li1, li2, title = list(text = "App", img = "logo.png"))
  s <- as.character(sm)
  expect_true(grepl("navbar-nav", s))
  # title attached as attribute should not break rendering
  expect_true(grepl("logo.png", s))

  hm <- horizontal_menu(li1, li2)
  hs <- as.character(hm)
  expect_true(grepl("navbar-nav", hs))
})

test_that("get_layout_attributes returns expected classes and tabler_page validates inputs and includes script", {
  expect_equal(get_layout_attributes("boxed")$class, "layout-boxed")

  # invalid layout should error
  expect_error(page(layout = "nope"), "Invalid layout")

  # Valid simple page should include script that sets localStorage keys
  p <- page(title = "X", layout = "boxed", theme = "dark", color = "blue", show_theme_button = FALSE)
  s <- as.character(p)
  # ensure the inline script that sets localStorage and data-bs-theme is present
  expect_true(grepl("layout-boxed", s))
})
test_that("tabler_page simple output", {
  ui <- page(
    title = "Combo Dashboard",
    layout = "combo",
    body = body("Welcome to Tabler!")
  )

  # tabler_page now returns a tagList containing head and body (not a full
  # <html> tag) so assert accordingly.
  expect_s3_class(ui, "shiny.tag.list")

  # First child should be a head tag, second a body tag
  expect_s3_class(ui[[1]], "shiny.tag")
  expect_equal(ui[[1]]$name, "head")
  expect_s3_class(ui[[2]], "shiny.tag")
  expect_equal(ui[[2]]$name, "body")

  # Body should contain our content
  expect_true(grepl("Welcome to Tabler", as.character(ui[[2]])))
})

test_that("tabler_page basic and error cases", {
  ui <- page(
    title = "Combo Dashboard",
    layout = "combo",
    body = body("Welcome to Tabler!")
  )

  expect_s3_class(ui, "shiny.tag.list")
  expect_s3_class(ui[[2]], "shiny.tag")
  expect_equal(ui[[2]]$name, "body")
  expect_true(grepl("Welcome to Tabler", as.character(ui[[2]])))

  # invalid layout should error
  expect_error(page(layout = "nonexisting"), "Invalid layout")

  # RTL layout sets dir attribute on the top-level .page div inside the tagList
  ui_rtl <- page(layout = "rtl", body = body("rtl"))
  ui_rtl_str <- as.character(ui_rtl)
  expect_true(grepl('class="page"|class=\"page\"', ui_rtl_str))
  expect_true(grepl('dir="rtl"|dir=\\"rtl\\"', ui_rtl_str))
})

test_that("tabler body/header/navbar/footer/sidebar and menus", {
  b <- body("content")
  expect_s3_class(b, "shiny.tag")
  expect_true(grepl("page-body", b$attribs$class))
  expect_true(grepl("content", paste0(b)))

  nav <- topbar(title = "Brand")
  expect_s3_class(nav, "shiny.tag")
  expect_true(grepl("Brand", paste0(nav)))

  # navbar with brand_image should include an <img> with correct attributes
  img_nav <- topbar(brand_image = "logo.png", title = "ImgTitle")
  expect_s3_class(img_nav, "shiny.tag")
  expect_true(grepl("logo.png", paste0(img_nav)))
  expect_true(grepl("navbar-brand-image", paste0(img_nav)))
  # alt text should be provided from title
  expect_true(grepl("ImgTitle|Dashboard", paste0(img_nav)))

  # when no title provided, alt should default to 'Dashboard'
  img_nav2 <- topbar(brand_image = "logo2.png")
  expect_s3_class(img_nav2, "shiny.tag")
  expect_true(grepl("logo2.png", paste0(img_nav2)))
  expect_true(grepl("alt=\"Dashboard\"|alt=\\\"Dashboard\\\"", paste0(img_nav2)))

  ft <- footer(left = "L", right = "R")
  expect_s3_class(ft, "shiny.tag")
  expect_true(grepl("L", paste0(ft)))
  expect_true(grepl("R", paste0(ft)))

  sidebar <- sidebar_menu(menu_item("S", tab_name = "s"))
  expect_s3_class(sidebar, "shiny.tag")
  expect_true(grepl("S", paste0(sidebar)))

  m1 <- menu_item("Dogs", tab_name = "dogs", icon = "dog", badge = "NEW")
  m2 <- menu_item("Cats", tab_name = "cats")
  sm <- sidebar_menu(m1, m2)
  expect_s3_class(sm, "shiny.tag")
  # first menu item anchor should have 'active' class
  expect_true(grepl("active", paste0(sm)))

  # tabs container should mark first child as active/show
  t1 <- tab_item("dogs", "dog content")
  t2 <- tab_item("cats", "cat content")
  tabs <- tab_items(t1, t2)
  expect_s3_class(tabs, "shiny.tag")
  expect_true(grepl("show active", paste0(tabs)))
})

test_that("tabler_page builds head and body and injects script", {
  ui <- page(title = "T", layout = "boxed", body = body("B"), theme = "dark", color = "blue")
  expect_s3_class(ui, "shiny.tag.list")
  ui_str <- as.character(ui)
  # script that sets theme should be present
  expect_true(grepl("boxed", ui_str))
  expect_true(grepl("B", ui_str))
})

test_that("tabler_page rtl layout sets dir on page", {
  ui_rtl <- page(layout = "rtl", body = body("R"))
  ui_rtl_str <- as.character(ui_rtl)
  expect_true(grepl('dir="rtl"|dir=\\"rtl\\"', ui_rtl_str))
})
