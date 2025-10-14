#' Create a Tabler Dashboard Page
#'
#' Main function to create a complete dashboard page with Tabler theme
#'
#' @param title App title
#' @param navbar Dashboard navbar (optional)
#' @param sidebar Dashboard sidebar (optional)
#' @param body Dashboard body content
#' @param footer Dashboard footer (optional)
#' @param layout Layout type: "default", "boxed", "combo", "condensed", "fluid",
#'   "fluid-vertical", "horizontal", "navbar-dark", "navbar-overlap",
#'   "navbar-sticky", "rtl", "vertical", "vertical-right", "vertical-transparent"
#'
#' @export
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(tabler)
#'
#'   # Default layout
#'   ui1 <- tablerPage(
#'     title = "My Dashboard",
#'     body = tablerBody(
#'       h1("Welcome to Tabler!")
#'     )
#'   )
#'
#'   # Boxed layout
#'   ui2 <- tablerPage(
#'     title = "Boxed Dashboard",
#'     layout = "boxed",
#'     body = tablerBody(
#'       h1("Boxed Layout!")
#'     )
#'   )
#'
#'   # Combo layout with sidebar
#'   ui3 <- tablerPage(
#'     title = "Combo Dashboard",
#'     layout = "combo",
#'     navbar = tablerNavbar(title = "My App"),
#'     sidebar = tablerSidebar(
#'       sidebarMenu(
#'         menuItem("Dashboard", tabName = "dashboard", icon = "home"),
#'         menuItem("Users", tabName = "users", icon = "users")
#'       )
#'     ),
#'     body = tablerBody(
#'       h1("Combo Layout!")
#'     )
#'   )
#'
#'   # Vertical layout (sidebar-only)
#'   ui4 <- tablerPage(
#'     title = "Vertical Dashboard",
#'     layout = "vertical",
#'     sidebar = tablerSidebar(
#'       sidebarMenu(
#'         menuItem("Dashboard", icon = "home"),
#'         menuItem("Users", icon = "users")
#'       )
#'     ),
#'     body = tablerBody(h1("Vertical Layout!"))
#'   )
#'
#'   # Fluid layout (full width)
#'   ui5 <- tablerPage(
#'     title = "Fluid Dashboard",
#'     layout = "fluid",
#'     navbar = tablerNavbar(title = "Fluid Layout"),
#'     body = tablerBody(h1("Fluid Layout!"))
#'   )
#'
#'   server <- function(input, output, session) {}
#'
#'   shinyApp(ui1, server)
#' }
tablerPage <- function(title = NULL, navbar = NULL, sidebar = NULL, body = NULL, footer = NULL, layout = "default") {
  # Validate layout
  valid_layouts <- c(
    "default", "boxed", "combo", "condensed", "fluid", "fluid-vertical",
    "horizontal", "navbar-dark", "navbar-overlap", "navbar-sticky",
    "rtl", "vertical", "vertical-right", "vertical-transparent"
  )
  if (!layout %in% valid_layouts) {
    stop("Invalid layout. Must be one of: ", paste(valid_layouts, collapse = ", "))
  }

  # Body classes and attributes based on layout
  body_attrs <- get_layout_attributes(layout)

  # Build page structure based on layout
  page_content <- get_layout_structure(layout, navbar, sidebar, body, footer)

  # Build the HTML structure
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$meta(charset = "utf-8"),
      shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, viewport-fit=cover"),
      shiny::tags$meta(`http-equiv` = "X-UA-Compatible", content = "ie=edge"),
      if (!is.null(title)) shiny::tags$title(title)
    ),
    do.call(shiny::tags$body, c(
      body_attrs,
      list(page_content)
    ))
  ) |>
    add_deps()
}

# Get layout-specific body attributes
get_layout_attributes <- function(layout) {
  base_class <- "antialiased"

  layout_class <- switch(layout,
    "boxed" = "layout-boxed",
    "fluid" = "layout-fluid",
    "fluid-vertical" = "layout-fluid",
    "condensed" = "layout-condensed",
    "navbar-overlap" = "layout-navbar-overlap",
    "navbar-sticky" = "layout-navbar-sticky",
    "vertical-transparent" = "layout-vertical-transparent",
    "rtl" = "layout-rtl",
    NULL
  )

  # Additional attributes for specific layouts
  attrs <- list(class = css_class(base_class, layout_class))

  # RTL layout needs dir attribute
  if (layout == "rtl") {
    attrs$dir <- "rtl"
  }

  # Dark navbar layouts
  if (layout == "navbar-dark") {
    attrs$`data-bs-theme` <- "dark"
  }

  attrs
}

# Get layout-specific page structure
get_layout_structure <- function(layout, navbar, sidebar, body, footer) {
  switch(layout,
    "combo" = build_combo_layout(navbar, sidebar, body, footer),
    "vertical" = build_vertical_layout(navbar, sidebar, body, footer),
    "vertical-right" = build_vertical_layout(navbar, sidebar, body, footer, side = "right"),
    "vertical-transparent" = build_vertical_layout(navbar, sidebar, body, footer),
    "horizontal" = build_horizontal_layout(navbar, sidebar, body, footer),
    "fluid-vertical" = build_vertical_layout(navbar, sidebar, body, footer),
    build_default_layout(navbar, sidebar, body, footer)
  )
}

# Helper function for default/boxed/fluid/condensed/navbar layouts
build_default_layout <- function(navbar, sidebar, body, footer) {
  shiny::tags$div(
    class = "page",
    # Sidebar (if present)
    if (!is.null(sidebar)) sidebar,

    # Navbar
    if (!is.null(navbar)) navbar,

    # Main content wrapper
    shiny::tags$div(
      class = "page-wrapper",

      # Page body
      shiny::tags$div(
        class = "page-body",
        body
      ),

      # Footer
      if (!is.null(footer)) footer
    )
  )
}

# Helper function for combo layout (sidebar + top navbar)
build_combo_layout <- function(navbar, sidebar, body, footer) {
  shiny::tags$div(
    class = "page",
    # Sidebar
    if (!is.null(sidebar)) sidebar,

    # Header (navbar for combo)
    if (!is.null(navbar)) {
      shiny::tags$header(
        class = "navbar navbar-expand-md d-none d-lg-flex d-print-none",
        navbar
      )
    },

    # Main content
    shiny::tags$div(
      class = "page-wrapper",

      # Page body
      shiny::tags$div(
        class = "page-body",
        body
      ),

      # Footer
      if (!is.null(footer)) footer
    )
  )
}

# Helper function for vertical layouts (with sidebar)
build_vertical_layout <- function(navbar, sidebar, body, footer, side = "left") {
  sidebar_class <- if (side == "right") {
    "navbar navbar-vertical navbar-expand-lg navbar-right"
  } else {
    "navbar navbar-vertical navbar-expand-lg"
  }

  # Update sidebar class if provided
  if (!is.null(sidebar)) {
    sidebar <- shiny::tagAppendAttributes(sidebar, class = sidebar_class)
  }

  shiny::tags$div(
    class = "page",
    # Sidebar
    if (!is.null(sidebar)) sidebar,

    # Main content
    shiny::tags$div(
      class = "page-wrapper",

      # Navbar (inside page wrapper for vertical layouts)
      if (!is.null(navbar)) navbar,

      # Page body
      shiny::tags$div(
        class = "page-body",
        body
      ),

      # Footer
      if (!is.null(footer)) footer
    )
  )
}

# Helper function for horizontal layout
build_horizontal_layout <- function(navbar, sidebar, body, footer) {
  shiny::tags$div(
    class = "page page-center",
    # Navbar with horizontal nav
    if (!is.null(navbar)) {
      shiny::tagAppendAttributes(navbar, class = "navbar-horizontal")
    },

    # Container for horizontal content
    shiny::tags$div(
      class = "container-xl d-flex flex-column justify-content-center",
      body
    ),

    # Footer
    if (!is.null(footer)) footer
  )
}

#' Create a Dashboard Body
#'
#' Container for dashboard content
#'
#' @param ... Content to include in the body
#' @param class Additional CSS classes
#'
#' @export
tablerBody <- function(..., class = NULL) {
  shiny::tags$div(
    class = paste("container-xl", class),
    ...
  )
}

#' Create a Dashboard Navbar
#'
#' Top navigation bar for the dashboard
#'
#' @param title Brand title/text
#' @param brand_image URL or path to brand image
#' @param ... Additional navbar items
#'
#' @export
tablerNavbar <- function(title = NULL, brand_image = NULL, ...) {
  shiny::tags$header(
    class = "navbar navbar-expand-md navbar-light d-print-none",
    shiny::tags$div(
      class = "container-xl",

      # Brand
      shiny::tags$h1(
        class = "navbar-brand navbar-brand-autodark d-none-navbar-horizontal pe-0 pe-md-3",
        if (!is.null(brand_image)) {
          shiny::tags$a(
            href = "#",
            shiny::tags$img(
              src = brand_image,
              alt = title %||% "Dashboard",
              class = "navbar-brand-image"
            )
          )
        } else if (!is.null(title)) {
          shiny::tags$a(href = "#", title)
        }
      ),

      # Navbar items
      shiny::tags$div(
        class = "navbar-nav flex-row order-md-last",
        ...
      )
    )
  )
}

#' Create a Dashboard Footer
#'
#' Footer for the dashboard
#'
#' @param left Left-aligned content
#' @param right Right-aligned content
#'
#' @export
tablerFooter <- function(left = NULL, right = NULL) {
  shiny::tags$footer(
    class = "footer footer-transparent d-print-none",
    shiny::tags$div(
      class = "container-xl",
      shiny::tags$div(
        class = "row text-center align-items-center flex-row-reverse",
        if (!is.null(right)) {
          shiny::tags$div(
            class = "col-lg-auto ms-lg-auto",
            right
          )
        },
        if (!is.null(left)) {
          shiny::tags$div(
            class = "col-12 col-lg-auto mt-3 mt-lg-0",
            left
          )
        }
      )
    )
  )
}

#' Create a Dashboard Sidebar
#'
#' Vertical sidebar for navigation
#'
#' @param ... Sidebar content (typically tabler_nav_menu)
#' @param theme Sidebar theme: "light" or "dark"
#'
#' @export
tablerSidebar <- function(..., theme = "dark") {
  shiny::tags$aside(
    class = "navbar navbar-vertical navbar-expand-lg",
    `data-bs-theme` = if (theme == "dark") "dark" else NULL,
    shiny::tags$div(
      class = "container-fluid",
      # Sidebar content
      shiny::tags$div(
        class = "collapse navbar-collapse",
        id = "sidebar-menu",
        ...
      )
    )
  )
}

#' Create a Navigation Menu
#'
#' Container for navigation items in sidebar
#'
#' @param ... Navigation items (tabler_nav_item)
#'
#' @export
sidebarMenu <- function(...) {
  shiny::tags$ul(
    class = "navbar-nav pt-lg-3",
    ...
  )
}

#' Create a Navigation Item
#'
#' Individual navigation item for sidebar
#'
#' @param text Item text/label
#' @param tabName Tab name for routing
#' @param icon Icon name (optional)
#' @param href Link URL (optional, alternative to tabName)
#' @param badge Badge text (optional)
#'
#' @export
menuItem <- function(text, tabName = NULL, icon = NULL, href = NULL, badge = NULL) {
  # Icon element
  icon_el <- if (!is.null(icon)) {
    list(
      shiny::tags$span(
        class = "nav-link-icon d-md-none d-lg-inline-block",
        tablerIcon(icon)
      ),
      " "
    )
  }

  # Badge element
  badge_el <- if (!is.null(badge)) {
    shiny::tags$span(
      class = "badge badge-sm bg-green-lt text-uppercase ms-auto",
      badge
    )
  }

  # Link attributes
  link_attrs <- list(
    class = "nav-link",
    href = href %||% "#"
  )

  if (!is.null(tabName)) {
    link_attrs[["data-toggle"]] <- "tab"
    link_attrs[["data-target"]] <- paste0("#", tabName)
  }

  shiny::tags$li(
    class = "nav-item",
    do.call(shiny::tags$a, c(
      link_attrs,
      list(
        icon_el,
        shiny::tags$span(class = "nav-link-title", text),
        badge_el
      )
    ))
  )
}

# Helper operator for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x
