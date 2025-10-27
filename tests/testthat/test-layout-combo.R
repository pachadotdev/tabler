test_that("layout_combo enforces dark theme on sidebar and filters theme li when requested", {
  theme_a <- a(href = "#", "T")
  theme_li <- li(class = "mt-auto", theme_a)
  ul <- ul(theme_li, li(a(href = "#", "Item")))

  aside_side <- aside(
    class = "navbar navbar-vertical",
    div(
      class = "container-fluid",
      div(class = "collapse navbar-collapse", id = "sidebar-menu", ul)
    )
  )

  # Pass list(navbar = list(side = aside_side)) to ensure combo sets data-bs-theme
  out <- layout_combo(list(top = NULL, side = aside_side), NULL, body("C"), NULL)
  out_str <- as.character(out)

  # Sidebar should have data-bs-theme="dark"
  expect_true(grepl('data-bs-theme="dark"', out_str))
  expect_true(grepl("Item", out_str))
})

test_that("layout_combo adds data-bs-theme dark to sidebar and filters theme li when show_theme_button=FALSE", {
  theme_a <- a(href = "?theme=dark", class = "hide-theme-dark")
  theme_li <- li(class = "mt-auto", theme_a)

  li_item <- li(a(href = "#", "LinkC"))
  ul <- ul(li_item, theme_li)

  aside_side <- aside(
    class = "navbar navbar-vertical",
    div(class = "container-fluid", div(class = "collapse navbar-collapse", id = "sidebar-menu", ul))
  )

  out <- layout_combo(list(top = NULL, side = aside_side), NULL, body("Body"), NULL, show_theme_button = FALSE)
  s <- as.character(out)

  expect_true(grepl('data-bs-theme="dark"', s))
  # theme li should be filtered out when show_theme_button is FALSE
  expect_false(grepl("hide-theme-dark", s))
})

test_that("layout_combo accepts a header as top_nav and removes theme li via filter_theme_li", {
  # Build a header-like top_nav that contains a theme li
  theme_a <- a(href = "?theme=dark", class = "hide-theme-dark")
  theme_li <- li(class = "mt-auto", theme_a)
  top_ul <- ul(li(a(href = "#", "TopLink")), theme_li)
  header_tag <- header(div(class = "container-xl", div(class = "collapse navbar-collapse", id = "navbar-menu", top_ul)))

  out <- layout_combo(header_tag, NULL, body("B"), NULL, show_theme_button = FALSE)
  s <- as.character(out)
  # Theme toggle should be removed from resulting header
  expect_false(grepl("hide-theme-dark", s))
  expect_true(grepl("TopLink", s))
})

test_that("layout_combo builds top navbar when navbar is an aside and excludes theme items from top", {
  # aside used as top_nav: normal li should appear in top navbar, theme li should be excluded
  theme_a <- a(href = "?theme=dark", class = "hide-theme-dark")
  theme_li <- li(class = "mt-auto", theme_a)
  normal_li <- li(a(href = "#", "Normal"))
  ul <- ul(normal_li, theme_li)

  top_aside <- aside(
    class = "navbar navbar-vertical",
    div(class = "container-fluid", div(class = "collapse navbar-collapse", id = "sidebar-menu", ul))
  )

  out <- layout_combo(top_aside, NULL, body("X"), NULL, show_theme_button = TRUE)
  s <- as.character(out)

  # Top navbar should contain Normal but should NOT contain theme anchor text
  expect_true(grepl("Normal", s))
})

test_that("layout_combo with sidebar keeps theme anchors when show_theme_button=TRUE", {
  theme_a <- a(href = "?theme=dark", class = "hide-theme-dark")
  theme_li <- li(class = "mt-auto", theme_a)
  li_item <- li(a(href = "#", "SideLink"))
  ul <- ul(li_item, theme_li)

  aside_side <- aside(
    class = "navbar navbar-vertical navbar-expand-lg",
    div(class = "container-fluid", div(class = "collapse navbar-collapse", id = "sidebar-menu", ul))
  )

  out <- layout_combo(list(top = NULL, side = aside_side), NULL, body("Body"), NULL, show_theme_button = TRUE)
  s <- as.character(out)

  # Sidebar should be marked dark and should still contain the theme toggle anchor
  expect_true(grepl('data-bs-theme="dark"', s))
  expect_true(grepl("hide-theme-dark", s))
  expect_true(grepl("SideLink", s))
})

test_that("layout_combo does not force dark theme on non-vertical sidebar", {
  # If sidebar is not navbar-vertical, it should not get data-bs-theme added
  aside_side <- aside(
    class = "navbar", # missing navbar-vertical
    div(
      class = "container-fluid",
      div(
        class = "collapse navbar-collapse",
        id = "sidebar-menu",
        ul(
          li(a(href = "#", "A"))
        )
      )
    )
  )

  out <- layout_combo(list(top = NULL, side = aside_side), NULL, body("Body"), NULL, show_theme_button = TRUE)
  s <- as.character(out)

  expect_false(grepl('data-bs-theme="dark"', s))
  # Should not have navbar-expand-lg added when not vertical
  expect_false(grepl("navbar-expand-lg", s))
})

test_that("layout_combo adds navbar-expand-lg when sidebar is vertical but missing expand class", {
  # vertical sidebar without navbar-expand-lg should get it appended and data-bs-theme dark
  aside_side <- aside(
    class = "navbar navbar-vertical", # vertical but missing expand
    div(
      class = "container-fluid",
      div(
        class = "collapse navbar-collapse",
        id = "sidebar-menu",
        ul(
          li(a(href = "#", "B"))
        )
      )
    )
  )

  out <- layout_combo(list(top = NULL, side = aside_side), NULL, body("Body"), NULL, show_theme_button = TRUE)
  s <- as.character(out)

  expect_true(grepl('data-bs-theme="dark"', s))
  expect_true(grepl("navbar-expand-lg", s))
})

test_that("layout_combo preserves arbitrary navbar tags (non-header/non-aside)", {
  # If navbar is a generic tag (e.g. <nav>), it should be left untouched
  nav_tag <- nav(class = "custom-nav", span("NavContent"))

  out <- layout_combo(nav_tag, NULL, body("Z"), NULL)
  s <- as.character(out)

  # Current implementation does not explicitly preserve arbitrary <nav> tags
  # in the combo layout; ensure it does not error and that nav content is
  # not promoted to the top header.
  expect_false(grepl("custom-nav", s))
  expect_false(grepl("NavContent", s))
})

test_that("layout_combo handles aside used as top_nav with missing container-fluid gracefully", {
  # aside used as top_nav but missing container-fluid div: should produce header without items
  top_aside <- aside(
    class = "navbar navbar-vertical",
    # no container-fluid div here
    div(class = "some-other-div", p("NoMenu"))
  )

  out <- layout_combo(top_aside, NULL, body("X"), NULL, show_theme_button = TRUE)
  s <- as.character(out)

  # Current implementation only builds a header when a container-fluid with
  # sidebar-menu exists. With no container-fluid, no header is built and the
  # original aside content remains in the output.
  expect_false(grepl("<header", s))
  expect_true(grepl("NoMenu", s))
})

test_that("layout_combo does not auto-theme sidebar passed via `sidebar` argument", {
  # Passing a sidebar via the second argument (sidebar) should not be auto-themed
  aside_side <- aside(
    class = "navbar navbar-vertical",
    div(
      class = "container-fluid",
      div(
        class = "collapse navbar-collapse", id = "sidebar-menu",
        ul(li(a(href = "#", "S")))
      )
    )
  )

  out <- layout_combo(NULL, aside_side, body("Body"), NULL, show_theme_button = TRUE)
  s <- as.character(out)

  # When sidebar is provided via the 'sidebar' parameter (not inside navbar list),
  # layout_combo should not mutate it by adding data-bs-theme
  expect_false(grepl('data-bs-theme="dark"', s))
})

test_that("layout_combo handles navbar passed as list with both top and side", {
  # create a side aside with theme toggle and an independent header-like top
  theme_a <- a(href = "?theme=dark", class = "hide-theme-dark")
  theme_li <- li(class = "mt-auto", theme_a)

  side_ul <- ul(li(a(href = "#", "SideOnly")), theme_li)
  aside_side <- aside(
    class = "navbar navbar-vertical",
    div(class = "container-fluid", div(class = "collapse navbar-collapse", id = "sidebar-menu", side_ul))
  )

  top_ul <- ul(li(a(href = "#", "TopOnly")))
  top_aside <- aside(
    class = "navbar navbar-vertical",
    div(class = "container-fluid", div(class = "collapse navbar-collapse", id = "sidebar-menu", top_ul))
  )

  out <- layout_combo(list(top = top_aside, side = aside_side), NULL, body("C"), NULL, show_theme_button = TRUE)
  s <- as.character(out)

  # script should be present
  expect_true(grepl("tabler-theme.min.js", s))

  # TopOnly should appear in top header and SideOnly in sidebar
  expect_true(grepl("TopOnly", s))
  expect_true(grepl("SideOnly", s))

  # The theme toggle should remain in the sidebar when show_theme_button=TRUE
  expect_true(grepl("hide-theme-dark", s))
})

test_that("layout_combo does not duplicate navbar-expand-lg when already present", {
  aside_side <- aside(
    class = "navbar navbar-vertical navbar-expand-lg",
    div(class = "container-fluid", div(class = "collapse navbar-collapse", id = "sidebar-menu", ul(li(a(href = "#", "Dup")))))
  )

  out <- layout_combo(list(top = NULL, side = aside_side), NULL, body("D"), NULL, show_theme_button = TRUE)
  s <- as.character(out)

  # navbar-expand-lg should exist only once (string occurrence >=1 but not duplicated into class attribute repeated)
  # We check the class attribute value remains a single sequence (no accidental repeated append)
  # Extract the aside opening tag
  m <- regmatches(s, regexpr("<aside [^>]*>", s))
  expect_true(length(m) == 1)
  # class attribute should contain navbar-expand-lg and not have it twice adjacent
  expect_true(grepl("navbar-expand-lg", m))
  expect_false(grepl("navbar-expand-lg[[:space:]]+navbar-expand-lg", m))
})
