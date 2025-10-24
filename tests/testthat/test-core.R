test_that("tabler_page simple output", {
  ui <- tabler_page(
    title = "Combo Dashboard",
    layout = "combo",
    body = tabler_body("Welcome to Tabler!")
  )

  expect_s3_class(ui, "shiny.tag")

  expect_equal(ui$name, "html")

  expect_equal(ui$attribs$lang, "en")

  expect_type(ui$children, "list")

  expect_true(grepl("Welcome to Tabler", ui$children[[2]]))
})

test_that("tabler_page basic and error cases", {
  ui <- tabler_page(
    title = "Combo Dashboard",
    layout = "combo",
    body = tabler_body("Welcome to Tabler!")
  )

  expect_s3_class(ui, "shiny.tag")
  expect_equal(ui$name, "html")
  expect_equal(ui$attribs$lang, "en")
  expect_type(ui$children, "list")
  expect_true(grepl("Welcome to Tabler", ui$children[[2]]))

  # invalid layout should error
  expect_error(tabler_page(layout = "nonexisting"), "Invalid layout")

  # RTL layout sets dir attribute
  ui_rtl <- tabler_page(layout = "rtl", body = tabler_body("rtl"))
  expect_equal(ui_rtl$attribs$dir, "rtl")
})

test_that("tabler body/header/navbar/footer/sidebar and menus", {
  b <- tabler_body("content", class = "my-class")
  expect_s3_class(b, "shiny.tag")
  expect_true(grepl("my-class", b$attribs$class))
  expect_true(grepl("content", paste0(b)))

  hdr <- tabler_page_header("Main", subtitle = "sub")
  expect_s3_class(hdr, "shiny.tag")
  expect_true(grepl("Main", paste0(hdr)))
  expect_true(grepl("sub", paste0(hdr)))

  # header with extra content via ... should render the extra column
  hdr2 <- tabler_page_header("H2", subtitle = "s2", shiny::tags$button("Click"))
  expect_s3_class(hdr2, "shiny.tag")
  expect_true(grepl("Click", paste0(hdr2)))
  # extra column class should be present when ... provided
  expect_true(grepl("col-auto ms-auto", paste0(hdr2)))

  nav <- tabler_navbar(title = "Brand")
  expect_s3_class(nav, "shiny.tag")
  expect_true(grepl("Brand", paste0(nav)))

  # navbar with brand_image should include an <img> with correct attributes
  img_nav <- tabler_navbar(brand_image = "logo.png", title = "ImgTitle")
  expect_s3_class(img_nav, "shiny.tag")
  expect_true(grepl("logo.png", paste0(img_nav)))
  expect_true(grepl("navbar-brand-image", paste0(img_nav)))
  # alt text should be provided from title
  expect_true(grepl("ImgTitle|Dashboard", paste0(img_nav)))

  # when no title provided, alt should default to 'Dashboard'
  img_nav2 <- tabler_navbar(brand_image = "logo2.png")
  expect_s3_class(img_nav2, "shiny.tag")
  expect_true(grepl("logo2.png", paste0(img_nav2)))
  expect_true(grepl("alt=\"Dashboard\"|alt=\\\"Dashboard\\\"", paste0(img_nav2)))
  # sidebar brand image dimensions (sidebar includes explicit width/height)
  sb <- tabler_sidebar(brand_image = "slogo.png", title = "S")
  expect_true(grepl("slogo.png", paste0(sb)))
  expect_true(grepl("width=\\\"110\\\"|width=\"110\"", paste0(sb)))
  expect_true(grepl("height=\\\"32\\\"|height=\"32\"", paste0(sb)))

  ft <- tabler_footer(left = "L", right = "R")
  expect_s3_class(ft, "shiny.tag")
  expect_true(grepl("L", paste0(ft)))
  expect_true(grepl("R", paste0(ft)))

  sidebar <- tabler_sidebar(title = "S")
  expect_s3_class(sidebar, "shiny.tag")
  expect_true(grepl("S", paste0(sidebar)))

  m1 <- menu_item("Dogs", tab_name = "dogs", icon = "dog", badge = "NEW")
  m2 <- menu_item("Cats", tab_name = "cats")
  sm <- sidebar_menu(m1, m2)
  expect_s3_class(sm, "shiny.tag")
  # first menu item anchor should have 'active' class
  expect_true(grepl("active", paste0(sm)))

  # tabs container should mark first child as active/show
  t1 <- tabler_tab_item("dogs", "dog content")
  t2 <- tabler_tab_item("cats", "cat content")
  tabs <- tabler_tab_items(t1, t2)
  expect_s3_class(tabs, "shiny.tag")
  expect_true(grepl("show active", paste0(tabs)))
})
