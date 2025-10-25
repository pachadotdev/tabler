#' @title Create a Tabler Dashboard Page
#' @description Main function to create a complete dashboard page with Tabler theme
#' @param title App title
#' @param navbar Dashboard navbar/menu. Can be:
#'   - `sidebar_menu()` for a vertical sidebar
#'   - `horizontal_menu()` for a horizontal menu
#' @param body Dashboard body content
#' @param footer Dashboard footer (optional)
#' @param layout Layout type: "boxed", "combo", "condensed", "fluid",
#'   "fluid-vertical", "horizontal", "navbar-dark", "navbar-overlap",
#'   "navbar-sticky", "rtl", "vertical", "vertical-right", "vertical-transparent"
#' @param theme Default theme: "light" (default) or "dark".
#' @param color Color theme (optional): "blue" (default), "azure", "indigo", "purple", "pink",
#'   "red", "orange", "yellow", "lime", "green", "teal", "cyan".
#' @examples
#' ui <- tabler_page(
#'   title = "Combo Dashboard",
#'   layout = "combo",
#'   navbar = list(
#'     top = topbar(title = "My App"),
#'     side = sidebar_menu(
#'       menu_item("Dogs", icon = "dog"),
#'       menu_item("Cats", icon = "cat")
#'     )
#'   ),
#'   body = tabler_body("Welcome to Tabler!")
#' )
#'
#' server <- function(input, output, session) {}
#'
#' # shiny::shinyApp(ui, server)
#' @return HTML tag with dependencies attached
#' @rdname shiny-page
#' @export
tabler_page <- function(title = NULL, navbar = NULL, body = NULL, footer = NULL, layout = "boxed",
  theme = "light", color = "blue") {
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

  # Build page structure based on layout (forward theme/color)
  page_content <- get_layout_structure(layout, navbar, body, footer, theme = theme, color = color)

  # Build the HTML structure
  # For RTL, we need to set dir on html tag
  html_attrs <- if (layout == "rtl") list(dir = "rtl", lang = "en") else list(lang = "en")

  # Resolve theme selection and validate color
  allowed_themes <- c("light", "dark")
  if (!theme %in% allowed_themes) {
    stop("Invalid theme. Must be one of: ", paste(allowed_themes, collapse = ", "))
  }
  allowed_colors <- c("blue", "azure", "indigo", "purple", "pink", "red", "orange", "yellow", "lime", "green", "teal", "cyan")
  if (!is.null(color) && !color %in% allowed_colors) {
    stop("Invalid color. Must be one of: ", paste(allowed_colors, collapse = ", "))
  }

  # Prepare inline script to set Tabler theme keys in localStorage and
  # set the corresponding data attributes on <html> so CSS rules apply
  # immediately (avoid race with dependency JS).
  script_lines <- c(
    sprintf("try{localStorage.setItem('tabler-theme','%s');document.documentElement.setAttribute('data-bs-theme','%s')}catch(e){}", theme, theme)
  )
  if (!is.null(color)) {
    script_lines <- c(script_lines, sprintf("try{localStorage.setItem('tabler-theme-primary','%s');document.documentElement.setAttribute('data-bs-theme-primary','%s')}catch(e){}", color, color))
  } else {
    script_lines <- c(script_lines, "try{localStorage.removeItem('tabler-theme-primary');document.documentElement.removeAttribute('data-bs-theme-primary')}catch(e){}")
  }
  script_text <- paste(script_lines, collapse = ";")

  html_tag <- do.call(shiny::tags$html, c(
    html_attrs,
    list(
      shiny::tags$head(
        shiny::tags$meta(charset = "utf-8"),
        shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, viewport-fit=cover"),
        shiny::tags$meta(`http-equiv` = "X-UA-Compatible", content = "ie=edge"),
        if (!is.null(title)) shiny::tags$title(title),
        shiny::tags$script(shiny::HTML(script_text))
      ),
      do.call(shiny::tags$body, c(
        body_attrs,
        list(page_content)
      ))
    )
  ))

  add_deps(html_tag, layout = layout, theme = theme, color = color)
}

#' @title Create a Dashboard Body
#' @description Container for dashboard content
#' @param ... Content to include in the body
#' @param class Additional CSS classes
#' @return A Shiny tag representing the body
#' @rdname shiny-page
#' @export
tabler_body <- function(..., class = NULL) {
  shiny::tags$div(
    class = paste("container-xl", class),
    ...
  )
}

#' @title Create a Page Header
#' @description Displays a prominent title and subtitle at the top of the page
#' @param title Main page title
#' @param subtitle Optional subtitle or description
#' @param ... Additional header content (e.g., buttons, breadcrumbs)
#' @return A Shiny tag representing the page header
#' @rdname shiny-page
#' @export
tabler_page_header <- function(title, subtitle = NULL, ...) {
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

## Removed `tabler_navbar()` to allow passing `sidebar_menu()` and `horizontal_menu()`
## directly to `tabler_page()` via the `navbar` argument.

#' @title Create a Topbar Header
#' @description Helper to create a top navigation header. This replaces the
#' previous top-navbar behavior when users passed a header-like component.
#' @param title Brand title/text
#' @param brand_image URL or path to brand image
#' @param ... Additional header content (e.g., nav items)
#' @return A Shiny tag representing the top header
#' @rdname shiny-page
#' @export
topbar <- function(title = NULL, brand_image = NULL, ...) {
  extra <- list(...)

  # If the caller passed menu_item() elements (li tags), wrap them into a ul.navbar-nav
  nav_children <- NULL
  if (length(extra) > 0) {
    # collect li elements
    li_items <- lapply(extra, function(x) if (inherits(x, "shiny.tag") && x$name == "li") x else NULL)
    li_items <- Filter(Negate(is.null), li_items)
    if (length(li_items) > 0) {
      # Make first item active by default (add 'active' class to its anchor)
      first_li <- li_items[[1]]
      if (!is.null(first_li$children) && length(first_li$children) > 0) {
        anchor <- first_li$children[[1]]
        if (inherits(anchor, "shiny.tag") && anchor$name == "a") {
          li_items[[1]]$children[[1]] <- shiny::tagAppendAttributes(anchor, class = "active")
        }
      }

      nav_children <- shiny::tags$ul(class = "navbar-nav", li_items)
    }
  }

  shiny::tags$header(
    class = "navbar navbar-expand-md navbar-light d-print-none",
    shiny::tags$div(
      class = "container-xl",
      shiny::tags$h1(
        class = "navbar-brand navbar-brand-autodark d-none-navbar-horizontal pe-0 pe-md-3",
        if (!is.null(brand_image)) {
          shiny::tags$a(href = "#", shiny::tags$img(src = brand_image, alt = title %||% "Dashboard", class = "navbar-brand-image"))
        } else if (!is.null(title)) {
          shiny::tags$a(href = "#", title)
        }
      ),
      # place any raw passed elements (e.g., anchors, buttons) and the wrapped nav items
      shiny::tags$div(class = "navbar-nav flex-row order-md-last",
        lapply(extra, function(x) if (!(inherits(x, "shiny.tag") && x$name == "li")) x else NULL),
        nav_children
      )
    )
  )
}

#' @title Create a Dashboard Footer
#' @description Footer for the dashboard
#' @param left Left-aligned content
#' @param right Right-aligned content
#' @return A Shiny tag representing the footer
#' @rdname shiny-page
#' @export
tabler_footer <- function(left = NULL, right = NULL) {
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



#' @title Create a Navigation Menu
#' @description Container for navigation items in sidebar
#' @param ... Navigation items (menuItem)
#' @param title Optional brand for the sidebar; either a string (text title) or
#'   a named list with elements `text` and `img` (URL/path) to render a brand
#'   image and title. Example: `title = list(text = "My App", img = "logo.png")`.
#' @return A Shiny tag representing the sidebar menu
#' @rdname shiny-page
#' @export
sidebar_menu <- function(..., title = NULL) {
  items <- list(...)

  # Make first item active by default if it has a tab_name
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

  ul <- shiny::tags$ul(
    class = "navbar-nav pt-lg-3",
    items
  )

  # Attach title as attribute so layout builder can render a navbar-brand
  if (!is.null(title)) {
    ul$attribs$title <- title
  }

  ul
}


#' @title Create a Sidebar Brand
#' @description Helper to build a brand structure for `sidebar_menu(title = ...)`.
#' @param text Brand text/label
#' @param img Optional image URL/path for brand logo
#' @param href Optional link URL for the brand (defaults to "#")
#' @return A named list suitable to pass to `sidebar_menu(title = )`
#' @examples
#' sidebar_brand("My App", img = "logo.png")
#' @export
sidebar_brand <- function(text = NULL, img = NULL, href = "#") {
  res <- list()
  if (!is.null(text)) res$text <- text
  if (!is.null(img)) res$img <- img
  if (!is.null(href)) res$href <- href
  res
}

#' @title Create a Horizontal Navigation Menu
#' @description Container for navigation items in horizontal layout
#' @param ... Navigation items (menuItem)
#' @return A Shiny tag representing the horizontal menu
#' @rdname shiny-page
#' @export
horizontal_menu <- function(...) {
  items <- list(...)

  # Make first item active by default if it has a tab_name
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

#' @title Create a Navigation Item
#' @description Individual navigation item for sidebar
#' @param text Item text/label
#' @param tab_name Tab name for routing
#' @param icon Icon name (optional)
#' @param href Link URL (optional, alternative to tab_name)
#' @param badge Badge text (optional)
#' @return A Shiny tag representing the menu item
#' @rdname shiny-page
#' @export
menu_item <- function(text, tab_name = NULL, icon = NULL, href = NULL, badge = NULL) {
  # Icon element
  icon_el <- if (!is.null(icon)) {
    list(
      shiny::tags$span(
        class = "nav-link-icon d-md-none d-lg-inline-block",
        tabler_icon(icon)
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

  if (!is.null(tab_name)) {
    link_attrs[["data-toggle"]] <- "tab"
    link_attrs[["data-target"]] <- paste0("#", tab_name)
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

#' @title Create Tab Items Container
#' @description Container for multiple tab panels in tabbed layouts
#' @param ... Tab items created with tablerTabItem()
#' @return A Shiny tag representing the tab items container
#' @rdname shiny-page
#' @export
tabler_tab_items <- function(...) {
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

#' @title Create a Tab Item
#' @description Individual tab panel content
#' @param tab_name Unique identifier for the tab (must match menuItem tab_name)
#' @param ... Content for this tab
#' @return A Shiny tag representing the tab item
#' @rdname shiny-page
#' @export
tabler_tab_item <- function(tab_name, ...) {
  shiny::tags$div(
    class = "tab-pane fade",
    id = tab_name,
    role = "tabpanel",
    `data-tab` = tab_name,
    ...
  )
}

#' @title Get layout-specific body attributes
#' @description Determine body classes and attributes based on layout
#' @param layout Layout type
#' @return A list of body attributes
#' @keywords internal
get_layout_attributes <- function(layout) {
  base_class <- "antialiased"

  layout_class <- switch(layout,
    "boxed" = "layout-boxed",
    "fluid" = "layout-fluid",
    "fluid-vertical" = "layout-fluid",
    "condensed" = NULL, # condensed has no body class
    "navbar-overlap" = "layout-navbar-overlap",
    "navbar-sticky" = "layout-navbar-sticky",
    "vertical-transparent" = "layout-vertical-transparent",
    NULL
  )

  # Additional attributes for specific layouts
  attrs <- list(class = css_class(base_class, layout_class))

  attrs
}

#' @title Get layout-specific page structure
#' @description Build the HTML structure based on layout type
#' @param layout Layout type
#' @param navbar Navbar component
#' @param body Body content
#' @param footer Footer component
#' @return A Shiny tag representing the page structure
#' @keywords internal
get_layout_structure <- function(layout, navbar, body, footer, theme = "light", color = NULL) {
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
        # `ul` can represent either a vertical sidebar (sidebar_menu)
        # or a horizontal menu (horizontal_menu). We choose placement by
        # layout: for horizontal layouts treat as topbar; for vertical/combo
        # layouts treat as sidebar. The caller will pass a list for combo
        # layouts, or a single `ul` for simple vertical/horizontal pages.
        if (layout %in% c("horizontal")) {
          topbar <- navbar
        } else {
          sidebar <- navbar
        }
      }
    }
  }

  switch(layout,
    "combo" = build_combo_layout(top_nav, sidebar, body, footer, theme = theme, color = color),
  "vertical" = build_vertical_simple_layout(top_nav, sidebar, body, footer, layout = "vertical", theme = theme, color = color),
  "vertical-right" = build_vertical_simple_layout(top_nav, sidebar, body, footer, side = "right", layout = "vertical-right", theme = theme, color = color),
  "vertical-transparent" = build_vertical_simple_layout(top_nav, sidebar, body, footer, layout = "vertical-transparent", theme = theme, color = color),
    "horizontal" = build_horizontal_layout(top_nav, topbar, body, footer, theme = theme, color = color),
    "fluid-vertical" = build_fluid_vertical_layout(top_nav, sidebar, body, footer, theme = theme, color = color),
    "navbar-overlap" = build_navbar_overlap_layout(top_nav, sidebar, body, footer, theme = theme, color = color),
    "navbar-dark" = build_navbar_dark_layout(top_nav, sidebar, body, footer, theme = theme, color = color),
    "navbar-sticky" = build_navbar_sticky_layout(top_nav, sidebar, body, footer, theme = theme, color = color),
    "condensed" = build_condensed_layout(top_nav, sidebar, body, footer, theme = theme, color = color),
    build_default_layout(top_nav, sidebar, body, footer)
  )
}

#' @title Helper function for default/boxed/fluid layouts
#' @description Build standard page structure
#' @param navbar Navbar component
#' @param sidebar Sidebar component
#' @param body Body content
#' @param footer Footer component
#' @return A Shiny tag representing the page structure
#' @keywords internal
#' @noRd
build_default_layout <- function(navbar, sidebar, body, footer, theme = "light", color = NULL) {
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

#' @title Helper function for condensed layout (navbar without wrapper)
#' @description Build condensed page structure
#' @param navbar Navbar component
#' @param sidebar Sidebar component
#' @param body Body content
#' @param footer Footer component
#' @return A Shiny tag representing the page structure
#' @keywords internal
#' @noRd
build_condensed_layout <- function(navbar, sidebar, body, footer, theme = "light", color = NULL) {
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

#' @title Helper function for combo layout (sidebar + top navbar)
#' @description Build combo page structure
#' @param navbar Navbar component
#' @param sidebar Sidebar component
#' @param body Body content
#' @param footer Footer component
#' @return A Shiny tag representing the page structure
#' @keywords internal
#' @noRd
build_combo_layout <- function(navbar, sidebar, body, footer, theme = "light", color = NULL) {
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

#' @title Helper function for vertical layouts (sidebar only, no top navbar)
#' @description Build vertical page structure
#' @param navbar Navbar component
#' @param sidebar Sidebar component
#' @param body Body content
#' @param footer Footer component
#' @param side Sidebar side: "left" (default) or "right"
#' @return A Shiny tag representing the page structure
#' @keywords internal
#' @noRd
build_vertical_simple_layout <- function(navbar, sidebar, body, footer, side = "left", layout = NULL, theme = "light", color = NULL) {
  sidebar_class <- if (side == "right") {
    "navbar navbar-vertical navbar-expand-lg navbar-right"
  } else {
    "navbar navbar-vertical navbar-expand-lg"
  }

  # If sidebar is a raw <ul> (from sidebar_menu), wrap it in the
  # full <aside class="navbar navbar-vertical ..."> + container so
  # alignment and collapse behaviour match the official Tabler markup.
  if (!is.null(sidebar)) {
    if (inherits(sidebar, "shiny.tag") && sidebar$name == "ul") {
      # Prefer an explicit title attribute set by `sidebar_menu(title = ...)`.
      brand_node <- NULL
      if (!is.null(sidebar$attribs) && !is.null(sidebar$attribs$title)) {
        brand_node <- sidebar$attribs$title
      } else if (length(sidebar$children) > 0 && is.character(sidebar$children[[1]])) {
        # Fallback: if the first child of the UL is a plain text node, treat it as a
        # brand/title and move it into a .navbar-brand container so it appears
        # above the menu like the official example.
        brand_node <- trimws(sidebar$children[[1]])
        # remove the text node from ul children
        sidebar$children <- sidebar$children[-1]
      }

      # Build the aside wrapper and include brand if present
      aside_children <- list(
        # navbar toggler for collapse behaviour on smaller screens
        shiny::tags$button(
          class = "navbar-toggler",
          type = "button",
          `data-bs-toggle` = "collapse",
          `data-bs-target` = "#sidebar-menu",
          `aria-controls` = "sidebar-menu",
          `aria-expanded` = "false",
          `aria-label` = "Toggle navigation",
          shiny::tags$span(class = "navbar-toggler-icon")
        )
      )

      # Accept brand_node as a list (from sidebar_brand()), a single string,
      # or a character vector; ensure we only call nzchar on length-1 strings.
      brand_present <- FALSE
      if (is.list(brand_node)) {
        brand_present <- length(brand_node) > 0
      } else if (is.character(brand_node)) {
        brand_present <- length(brand_node) == 1 && nzchar(brand_node)
      } else if (!is.null(brand_node)) {
        # fallback truthiness for other types
        brand_present <- TRUE
      }

      if (brand_present) {
        # If title is a list with text and img, render an anchor with image
        if (is.list(brand_node) && !is.null(brand_node$img)) {
          brand_el <- shiny::tags$a(href = "#", class = "navbar-brand navbar-brand-autodark"," ",
            if (!is.null(brand_node$img)) shiny::tags$img(src = brand_node$img, alt = brand_node$text %||% "", class = "navbar-brand-image"),
            if (!is.null(brand_node$text)) brand_node$text
          )
        } else {
          brand_el <- shiny::tags$div(class = "navbar-brand navbar-brand-autodark", brand_node)
        }
        aside_children <- c(aside_children, list(brand_el))
      }

      aside_children <- c(aside_children, list(
        shiny::tags$div(class = "collapse navbar-collapse", id = "sidebar-menu", sidebar)
      ))

      # Decide aside theme. The official Tabler "vertical" example uses a
      # dark sidebar regardless of the page-level theme. To match that
      # behaviour: when layout == "vertical" force aside theme to "dark".
      aside_theme <- if (!is.null(layout) && layout == "vertical") "dark" else theme

      sidebar_attrs <- list(class = sidebar_class)
      sidebar_attrs[["data-bs-theme"]] <- aside_theme

      # Add helper class for coloration
      if (!is.null(aside_theme) && aside_theme == "dark") {
        sidebar_attrs[["class"]] <- paste(sidebar_attrs[["class"]], "navbar-dark")
      } else {
        sidebar_attrs[["class"]] <- paste(sidebar_attrs[["class"]], "navbar-light")
      }

      sidebar <- do.call(shiny::tags$aside, c(sidebar_attrs, list(shiny::tags$div(class = "container-fluid", aside_children))))
    } else {
      # If the sidebar is already an element like <aside>, append class and theme
      sidebar <- shiny::tagAppendAttributes(sidebar, class = sidebar_class)
      # respect the same aside_theme decision for prebuilt asides
      aside_theme <- if (!is.null(layout) && layout == "vertical") "dark" else theme
      sidebar <- shiny::tagAppendAttributes(sidebar, `data-bs-theme` = aside_theme)
      if (!is.null(aside_theme) && aside_theme == "dark") {
        sidebar <- shiny::tagAppendAttributes(sidebar, class = "navbar-dark")
      } else {
        sidebar <- shiny::tagAppendAttributes(sidebar, class = "navbar-light")
      }
    }
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

#' @title Helper function for fluid-vertical layout (sidebar + uses layout-fluid)
#' @description Build fluid-vertical page structure
#' @param navbar Navbar component
#' @param sidebar Sidebar component
#' @param body Body content
#' @param footer Footer component
#' @return A Shiny tag representing the page structure
#' @keywords internal
#' @noRd
build_fluid_vertical_layout <- function(navbar, sidebar, body, footer, theme = "light", color = NULL) {
  # Fluid-vertical is like vertical but with fluid body class
  build_vertical_simple_layout(navbar, sidebar, body, footer, theme = theme, color = color)
}

#' @title Helper function for horizontal layout (with horizontal menu)
#' @description Build horizontal page structure
#' @param navbar Navbar component
#' @param topbar Horizontal menu component
#' @param body Body content
#' @param footer Footer component
#' @return A Shiny tag representing the page structure
#' @keywords internal
#' @noRd
build_horizontal_layout <- function(navbar, topbar, body, footer, theme = "light", color = NULL) {
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
              topbar # This will be the horizontal_menu content
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

#' @title Helper function for navbar-overlap layout
#' @description Build navbar-overlap page structure
#' @param navbar Navbar component
#' @param sidebar Sidebar component
#' @param body Body content
#' @param footer Footer component
#' @return A Shiny tag representing the page structure
#' @keywords internal
#' @noRd
build_navbar_overlap_layout <- function(navbar, sidebar, body, footer, theme = "light", color = NULL) {
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

#' @title Helper function for navbar-dark layout
#' @description Build navbar-dark page structure
#' @param navbar Navbar component
#' @param sidebar Sidebar component
#' @param body Body content
#' @param footer Footer component
#' @return A Shiny tag representing the page structure
#' @keywords internal
#' @noRd
build_navbar_dark_layout <- function(navbar, sidebar, body, footer, theme = "light", color = NULL) {
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

#' @title Helper function for navbar-sticky layout
#' @description Build navbar-sticky page structure
#' @param navbar Navbar component
#' @param sidebar Sidebar component
#' @param body Body content
#' @param footer Footer component
#' @return A Shiny tag representing the page structure
#' @keywords internal
#' @noRd
build_navbar_sticky_layout <- function(navbar, sidebar, body, footer, theme = "light", color = NULL) {
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
