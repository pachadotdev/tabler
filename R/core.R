#' Create a Tabler Dashboard Page
#'
#' Main function to create a complete dashboard page with Tabler theme
#'
#' @param title App title
#' @param navbar Dashboard navbar/menu. Can be:
#'   - `tablerNavbar()` for top navigation bar (most layouts)
#'   - `tablerSidebar()` for vertical sidebar (vertical layouts)
#'   - `horizontalMenu()` for horizontal menu (horizontal layout only)
#' @param body Dashboard body content
#' @param footer Dashboard footer (optional)
#' @param layout Layout type: "boxed", "combo", "condensed", "fluid",
#'   "fluid-vertical", "horizontal", "navbar-dark", "navbar-overlap",
#'   "navbar-sticky", "rtl", "vertical", "vertical-right", "vertical-transparent"
#'
#' @export
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(tabler)
#'
#'   # Boxed layout (default)
#'   ui1 <- tablerPage(
#'     title = "My Dashboard",
#'     body = tablerBody(
#'       h1("Welcome to Tabler!")
#'     )
#'   )
#'
#'   # Boxed layout with navbar
#'   ui2 <- tablerPage(
#'     title = "Boxed Dashboard",
#'     layout = "boxed",
#'     navbar = tablerNavbar(title = "My App"),
#'     body = tablerBody(
#'       h1("Boxed Layout!")
#'     )
#'   )
#'
#'   # Vertical layout with sidebar
#'   ui3 <- tablerPage(
#'     title = "Vertical Dashboard",
#'     layout = "vertical",
#'     navbar = tablerSidebar(
#'       sidebarMenu(
#'         menuItem("Dashboard", icon = "home"),
#'         menuItem("Users", icon = "users")
#'       )
#'     ),
#'     body = tablerBody(h1("Vertical Layout!"))
#'   )
#'
#'   # Horizontal layout with menu
#'   ui4 <- tablerPage(
#'     title = "Horizontal Dashboard",
#'     layout = "horizontal",
#'     navbar = horizontalMenu(
#'       menuItem("Home", icon = "home"),
#'       menuItem("Analytics", icon = "chart-bar")
#'     ),
#'     body = tablerBody(h1("Horizontal Layout!"))
#'   )
#'
#'   # Combo layout with both navbar and sidebar
#'   ui5 <- tablerPage(
#'     title = "Combo Dashboard",
#'     layout = "combo",
#'     navbar = list(
#'       top = tablerNavbar(title = "My App"),
#'       side = tablerSidebar(
#'         sidebarMenu(
#'           menuItem("Dashboard", icon = "home")
#'         )
#'       )
#'     ),
#'     body = tablerBody(h1("Combo Layout!"))
#'   )
#'
#'   server <- function(input, output, session) {}
#'
#'   shinyApp(ui1, server)
#' }
tablerPage <- function(title = NULL, navbar = NULL, body = NULL, footer = NULL, layout = "boxed") {
  # Validate layout
  valid_layouts <- c(
    "boxed", "combo", "condensed", "fluid", "fluid-vertical",
    "horizontal", "navbar-dark", "navbar-overlap", "navbar-sticky",
    "rtl", "vertical", "vertical-right", "vertical-transparent"
  )
  if (!layout %in% valid_layouts) {
    stop("Invalid layout. Must be one of: ", paste(valid_layouts, collapse = ", "))
  }

  # Body classes and attributes based on layout
  body_attrs <- get_layout_attributes(layout)

  # Build page structure based on layout
  page_content <- get_layout_structure(layout, navbar, body, footer)

  # Build the HTML structure
  # For RTL, we need to set dir on html tag
  html_attrs <- if (layout == "rtl") list(dir = "rtl", lang = "en") else list(lang = "en")
  
  html_tag <- do.call(shiny::tags$html, c(
    html_attrs,
    list(
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
    )
  ))

  add_deps(html_tag, layout = layout)
}

# Get layout-specific body attributes
get_layout_attributes <- function(layout) {
  base_class <- "antialiased"

  layout_class <- switch(layout,
    "boxed" = "layout-boxed",
    "fluid" = "layout-fluid",
    "fluid-vertical" = "layout-fluid",
    "condensed" = NULL,  # condensed has no body class
    "navbar-overlap" = "layout-navbar-overlap",
    "navbar-sticky" = "layout-navbar-sticky",
    "vertical-transparent" = "layout-vertical-transparent",
    NULL
  )

  # Additional attributes for specific layouts
  attrs <- list(class = css_class(base_class, layout_class))

  attrs
}

# Get layout-specific page structure
get_layout_structure <- function(layout, navbar, body, footer) {
  # Parse navbar - can be a single component or a list for combo layout
  top_nav <- NULL
  sidebar <- NULL
  topbar <- NULL
  
  if (is.list(navbar) && !inherits(navbar, "shiny.tag")) {
    # List format for combo layout: list(top = ..., side = ...)
    top_nav <- navbar$top
    sidebar <- navbar$side
  } else if (!is.null(navbar)) {
    # Determine component type by class
    if (inherits(navbar, "shiny.tag")) {
      tag_name <- navbar$name
      if (tag_name == "aside") {
        sidebar <- navbar
      } else if (tag_name == "header") {
        top_nav <- navbar
      } else if (tag_name == "ul") {
        # This is horizontalMenu
        topbar <- navbar
      }
    }
  }
  
  switch(layout,
    "combo" = build_combo_layout(top_nav, sidebar, body, footer),
    "vertical" = build_vertical_simple_layout(top_nav, sidebar, body, footer),
    "vertical-right" = build_vertical_simple_layout(top_nav, sidebar, body, footer, side = "right"),
    "vertical-transparent" = build_vertical_simple_layout(top_nav, sidebar, body, footer),
    "horizontal" = build_horizontal_layout(top_nav, topbar, body, footer),
    "fluid-vertical" = build_fluid_vertical_layout(top_nav, sidebar, body, footer),
    "navbar-overlap" = build_navbar_overlap_layout(top_nav, sidebar, body, footer),
    "navbar-dark" = build_navbar_dark_layout(top_nav, sidebar, body, footer),
    "navbar-sticky" = build_navbar_sticky_layout(top_nav, sidebar, body, footer),
    "condensed" = build_condensed_layout(top_nav, sidebar, body, footer),
    build_default_layout(top_nav, sidebar, body, footer)
  )
}

# Helper function for default/boxed/fluid layouts
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

# Helper function for condensed layout (navbar without wrapper)
build_condensed_layout <- function(navbar, sidebar, body, footer) {
  shiny::tags$div(
    class = "page",
    # Navbar (directly in page, not in wrapper)
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

    # Header (navbar for combo - outside page wrapper)
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

# Helper function for vertical layouts (sidebar only, no top navbar)
build_vertical_simple_layout <- function(navbar, sidebar, body, footer, side = "left") {
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

    # Main content (no top navbar in simple vertical layout)
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

# Helper function for fluid-vertical layout (sidebar + uses layout-fluid)
build_fluid_vertical_layout <- function(navbar, sidebar, body, footer) {
  # Fluid-vertical is like vertical but with fluid body class
  build_vertical_simple_layout(navbar, sidebar, body, footer)
}

# Helper function for horizontal layout (with horizontal menu)
build_horizontal_layout <- function(navbar, topbar, body, footer) {
  shiny::tags$div(
    class = "page",
    # Top Navbar
    if (!is.null(navbar)) navbar,

    # Horizontal menu (separate from navbar)
    if (!is.null(topbar)) {
      shiny::tags$header(
        class = "navbar-expand-md",
        shiny::tags$div(
          class = "collapse navbar-collapse",
          id = "navbar-menu",
          shiny::tags$div(
            class = "navbar",
            shiny::tags$div(
              class = "container-xl",
              topbar  # This will be the horizontalMenu content
            )
          )
        )
      )
    },

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

# Helper function for navbar-overlap layout
build_navbar_overlap_layout <- function(navbar, sidebar, body, footer) {
  shiny::tags$div(
    class = "page",
    # Navbar with overlap class and dark theme
    if (!is.null(navbar)) {
      shiny::tagAppendAttributes(navbar, 
        class = "navbar-overlap",
        `data-bs-theme` = "dark"
      )
    },

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

# Helper function for navbar-dark layout
build_navbar_dark_layout <- function(navbar, sidebar, body, footer) {
  shiny::tags$div(
    class = "page",
    # Navbar with dark theme
    if (!is.null(navbar)) {
      shiny::tagAppendAttributes(navbar, `data-bs-theme` = "dark")
    },

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

# Helper function for navbar-sticky layout
build_navbar_sticky_layout <- function(navbar, sidebar, body, footer) {
  shiny::tags$div(
    class = "page",
    # Sticky wrapper containing navbar
    if (!is.null(navbar)) {
      shiny::tags$div(
        class = "sticky-top",
        shiny::tagAppendAttributes(navbar, class = "sticky-top")
      )
    },

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

#' Create a Page Header
#'
#' Displays a prominent title and subtitle at the top of the page
#'
#' @param title Main page title
#' @param subtitle Optional subtitle or description
#' @param ... Additional header content (e.g., buttons, breadcrumbs)
#'
#' @export
tablerPageHeader <- function(title, subtitle = NULL, ...) {
  shiny::tags$div(
    class = "page-header d-print-none",
    shiny::tags$div(
      class = "container-xl",
      shiny::tags$div(
        class = "row g-2 align-items-center",
        shiny::tags$div(
          class = "col",
          shiny::tags$h2(class = "page-title", title),
          if (!is.null(subtitle)) {
            shiny::tags$div(
              class = "text-muted mt-1",
              subtitle
            )
          }
        ),
        if (length(list(...)) > 0) {
          shiny::tags$div(
            class = "col-auto ms-auto d-print-none",
            ...
          )
        }
      )
    )
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
#' @param ... Sidebar content (typically sidebarMenu)
#' @param title Brand title/text for sidebar header
#' @param brand_image URL or path to brand image
#' @param theme Sidebar theme: "light" or "dark"
#'
#' @export
tablerSidebar <- function(..., title = NULL, brand_image = NULL, theme = "dark") {
  # Build brand header if title or image provided
  brand_header <- if (!is.null(title) || !is.null(brand_image)) {
    shiny::tags$h1(
      class = "navbar-brand navbar-brand-autodark",
      if (!is.null(brand_image)) {
        shiny::tags$a(
          href = "#",
          shiny::tags$img(
            src = brand_image,
            alt = title %||% "Dashboard",
            width = 110,
            height = 32,
            class = "navbar-brand-image"
          )
        )
      } else if (!is.null(title)) {
        shiny::tags$a(href = "#", class = "navbar-brand", title)
      }
    )
  }
  
  shiny::tags$aside(
    class = "navbar navbar-vertical navbar-expand-lg",
    `data-bs-theme` = if (theme == "dark") "dark" else NULL,
    shiny::tags$div(
      class = "container-fluid",
      # Brand header
      brand_header,
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
#' @param ... Navigation items (menuItem)
#'
#' @export
sidebarMenu <- function(...) {
  items <- list(...)
  
  # Make first item active by default if it has a tabName
  if (length(items) > 0) {
    first_item <- items[[1]]
    if (inherits(first_item, "shiny.tag")) {
      # Find the anchor tag and add active class
      if (!is.null(first_item$children) && length(first_item$children) > 0) {
        anchor <- first_item$children[[1]]
        if (inherits(anchor, "shiny.tag") && anchor$name == "a") {
          items[[1]]$children[[1]] <- shiny::tagAppendAttributes(anchor, class = "active")
        }
      }
    }
  }
  
  shiny::tags$ul(
    class = "navbar-nav pt-lg-3",
    items
  )
}

#' Create a Horizontal Navigation Menu
#'
#' Container for navigation items in horizontal layout
#'
#' @param ... Navigation items (menuItem)
#'
#' @export
horizontalMenu <- function(...) {
  items <- list(...)
  
  # Make first item active by default if it has a tabName
  if (length(items) > 0) {
    first_item <- items[[1]]
    if (inherits(first_item, "shiny.tag")) {
      # Find the anchor tag and add active class
      if (!is.null(first_item$children) && length(first_item$children) > 0) {
        anchor <- first_item$children[[1]]
        if (inherits(anchor, "shiny.tag") && anchor$name == "a") {
          items[[1]]$children[[1]] <- shiny::tagAppendAttributes(anchor, class = "active")
        }
      }
    }
  }
  
  shiny::tags$ul(
    class = "navbar-nav",
    items
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

#' Create Tab Items Container
#'
#' Container for multiple tab panels in tabbed layouts
#'
#' @param ... Tab items created with tablerTabItem()
#'
#' @export
tablerTabItems <- function(...) {
  items <- list(...)
  
  # Make first item active by default
  if (length(items) > 0) {
    first_item <- items[[1]]
    if (inherits(first_item, "shiny.tag")) {
      # Add 'show active' classes to first tab
      items[[1]] <- shiny::tagAppendAttributes(first_item, class = "show active")
    }
  }
  
  shiny::tags$div(
    class = "tab-content",
    items
  )
}

#' Create a Tab Item
#'
#' Individual tab panel content
#'
#' @param tabName Unique identifier for the tab (must match menuItem tabName)
#' @param ... Content for this tab
#'
#' @export
tablerTabItem <- function(tabName, ...) {
  shiny::tags$div(
    class = "tab-pane fade",
    id = tabName,
    role = "tabpanel",
    `data-tab` = tabName,
    ...
  )
}
