test_that("get_layout_structure treats <aside> as sidebar for vertical layout", {
  # Build a minimal aside tag as a sidebar
  aside_tag <- aside(
    class = "custom-aside navbar-vertical",
    ul(class = "navbar-nav pt-lg-3", li("item"))
  )

  # Use page to build the full page for vertical layout
  page <- page(title = "Test", navbar = aside_tag, body = body("hi"), layout = "vertical")

  # Convert to HTML string for assertions
  html_str <- as.character(page)

  # Expect the aside to appear and have the vertical sidebar class present
  expect_true(grepl("<aside", html_str))
  expect_true(grepl("navbar-vertical", html_str))
})
