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
