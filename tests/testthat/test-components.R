test_that("components: card, value box, icon, alert, button", {
  card <- tabler_card("body", title = "T", footer = "F", status = "danger", class = "extra")
  expect_s3_class(card, "shiny.tag")
  expect_true(grepl("card-status-danger", paste0(card)))
  expect_true(grepl("T", paste0(card)))
  expect_true(grepl("F", paste0(card)))
  expect_true(grepl("extra", card$attribs$class))

  vb <- tabler_value_box("42", "Label", icon = "star", color = "success", width = 4)
  expect_s3_class(vb, "shiny.tag")
  expect_true(grepl("col-4", paste0(vb)))
  expect_true(grepl("bg-success", paste0(vb)))
  expect_true(grepl("star", paste0(vb)))

  # icon prefixes
  ti <- tabler_icon("home")
  expect_true(grepl("ti ti-home", paste0(ti)))
  bi <- tabler_icon("alarm", library = "bootstrap")
  expect_true(grepl("bi bi-alarm", paste0(bi)))
  fe <- tabler_icon("x", library = "feather")
  expect_true(grepl("fe fe-x", paste0(fe)))

  # alert with title and dismissible
  al <- tabler_alert("msg", type = "warning", dismissible = TRUE, title = "Heads up")
  expect_s3_class(al, "shiny.tag")
  expect_true(grepl("Heads up", paste0(al)))
  expect_true(grepl("btn-close", paste0(al)))

  # button size/outline/icon
  btn <- tabler_button("Go", color = "primary", size = "sm", outline = TRUE, icon = "search")
  expect_s3_class(btn, "shiny.tag")
  expect_true(grepl("btn-sm", paste0(btn)))
  expect_true(grepl("btn-outline-primary", paste0(btn)))
  expect_true(grepl("search", paste0(btn)))
})

test_that("edge cases and errors for components", {
  # menu_item with only text should still build anchor
  mi <- menu_item("JustText")
  expect_s3_class(mi, "shiny.tag")
  expect_true(grepl("JustText", paste0(mi)))

  # card without title/footer
  c2 <- tabler_card("only body")
  expect_s3_class(c2, "shiny.tag")
  expect_true(grepl("only body", paste0(c2)))

  # button defaults
  b2 <- tabler_button("Hi")
  expect_s3_class(b2, "shiny.tag")
  expect_true(grepl("btn", paste0(b2)))
})
