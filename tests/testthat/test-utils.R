test_that("null-coalescing operator works", {
  expect_equal((NULL %||% 5), 5)
  expect_equal((3 %||% 5), 3)
})

test_that("validate_color and validate_size behave as expected", {
  expect_true(validate_color("primary"))
  expect_false(validate_color("not-a-color"))

  expect_true(validate_size("sm"))
  expect_false(validate_size("xxl"))
})

test_that("css_class combines classes and ignores NULL/empty", {
  expect_equal(css_class("a", NULL, "b"), "a b")
  expect_null(css_class(NULL, ""))
})

test_that("validate_tab_name errors when name contains a dot", {
  expect_error(validate_tab_name("bad.name"), "tab_name must not have a '.' in it")
  # allowed name
  expect_null(validate_tab_name("goodname"))
})

test_that("utils helpers and filter_theme_li work", {
  # %||%
  expect_equal(NULL %||% 5, 5)
  expect_equal(1 %||% 5, 1)

  # validate_color / validate_size
  expect_true(validate_color("primary"))
  expect_false(validate_color("not-a-color"))
  expect_true(validate_size("sm"))
  expect_false(validate_size("xxl"))

  # css_class
  expect_equal(css_class("a", NULL, "b"), "a b")
  expect_null(css_class(NULL, ""))

  # validate_tab_name error
  expect_error(validate_tab_name("bad.name"), "tab_name must not have a '.'")

  # filter_theme_li should remove li items that contain theme anchors
  li_theme <- li(
    class = "nav-item mt-auto",
    list(
      a(href = "?theme=dark", class = "hide-theme-dark", "D"),
      a(href = "?theme=light", class = "hide-theme-light", "L")
    )
  )
  li_keep <- li(class = "nav-item", a(href = "#", "Keep"))
  nav_tag <- ul(class = "navbar-nav", li_keep, li_theme)
  cleaned <- filter_theme_li(nav_tag)
  cleaned_str <- as.character(cleaned)
  expect_true(grepl("Keep", cleaned_str))
  expect_false(grepl("hide-theme-dark|hide-theme-light", cleaned_str))
})
