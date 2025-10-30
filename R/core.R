#' @title Create a Tabler Dashboard Page
#' @description Main function to create a complete dashboard page with Tabler theme
#' @param title App title
#' @param navbar Dashboard navbar/menu. Can be:
#'   - `sidebar_menu()` for a vertical sidebar
#'   - `horizontal_menu()` for a horizontal menu
#' @param body Dashboard body content
#' @param footer Dashboard footer (optional)
#' @param layout Layout type: "boxed"
#' @param theme Default theme: "light" (default) or "dark".
#' @param color Color theme (optional): "blue" (default), "azure", "indigo", "purple", "pink",
#'   "red", "orange", "yellow", "lime", "green", "teal", "cyan".
#' @param show_theme_button Whether to show the theme toggle buttons (default: `FALSE`).
#' @examples
#' ui <- page(
#'   title = "Combo Dashboard",
#'   layout = "combo",
#'   navbar = list(
#'     top = topbar(title = "My App"),
#'     side = sidebar_menu(
#'       menu_item("Dogs", icon = "dog"),
#'       menu_item("Cats", icon = "cat")
#'     )
#'   ),
#'   body = body("Welcome to Tabler!")
#' )
#'
#' server <- function(input, output, session) {}
#'
#' # shinyApp(ui, server)
#' @return HTML tag with dependencies attached
#' @rdname tabler-page
## NOTE: exported names intentionally short (no `tabler_` prefix)
#' @export
page <- function(
    title = NULL, navbar = NULL, body = NULL, footer = NULL, layout = "boxed",
    theme = "light", color = "blue", show_theme_button = FALSE) {
  # Validate layout
  valid_layouts <- c(
    "boxed",
    "combo",
    "condensed",
    "fluid",
    "fluid-vertical",
    "horizontal",
    "navbar-dark",
    "navbar-overlap",
    "navbar-sticky",
    "navbar-sticky-dark",
    "rtl",
    "vertical-right",
    "vertical-transparent",
    "vertical"
  )

  if (!layout %in% valid_layouts) {
    stop("Invalid layout. Must be one of: ", paste(valid_layouts, collapse = ", "))
  }

  # Body classes and attributes based on layout
  body_attrs <- get_layout_attributes(layout)

  # Build page structure based on layout (forward theme/color)
  page_content <- switch(layout,
    "boxed" = layout_boxed(navbar, NULL, body, footer, theme = theme, color = color, show_theme_button = show_theme_button),
    "combo" = layout_combo(navbar, NULL, body, footer, theme = theme, color = color, show_theme_button = show_theme_button),
    "condensed" = layout_condensed(navbar, NULL, body, footer, theme = theme, color = color, show_theme_button = show_theme_button),
    "fluid" = layout_fluid(navbar, NULL, body, footer, theme = theme, color = color, show_theme_button = show_theme_button),
    "fluid-vertical" = layout_fluid_vertical(navbar, NULL, body, footer, theme = theme, color = color, show_theme_button = show_theme_button),
    "horizontal" = layout_horizontal(navbar, NULL, body, footer, theme = theme, color = color, show_theme_button = show_theme_button),
    "vertical" = layout_vertical(navbar, NULL, body, footer, theme = theme, color = color, show_theme_button = show_theme_button),
    "vertical-right" = layout_vertical_right(navbar, NULL, body, footer, theme = theme, color = color, show_theme_button = show_theme_button),
    "vertical-transparent" = layout_vertical_transparent(navbar, NULL, body, footer, theme = theme, color = color, show_theme_button = show_theme_button),
    "navbar-dark" = layout_navbar_dark(navbar, NULL, body, footer, theme = theme, color = color, show_theme_button = show_theme_button),
    "navbar-overlap" = layout_navbar_overlap(navbar, NULL, body, footer, theme = theme, color = color, show_theme_button = show_theme_button),
    "navbar-sticky" = layout_navbar_sticky(navbar, NULL, body, footer, theme = theme, color = color, show_theme_button = show_theme_button),
    "navbar-sticky-dark" = layout_navbar_sticky_dark(navbar, NULL, body, footer, color = color, show_theme_button = show_theme_button),
    "rtl" = layout_rtl(navbar, NULL, body, footer, theme = theme, color = color, show_theme_button = show_theme_button),
    stop("Unsupported layout: ", layout)
  )

  # Build the HTML structure
  # Note: Do not build a full <html> tag here; Shiny provides that.

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
  # Small click handler so clicking theme links toggles theme without a hard reload.
  # Also optionally remove the theme buttons if show_theme_button is FALSE.
  script_lines <- c(
    script_lines,
    "(function(){try{document.addEventListener('click',function(e){var closest = e.target && e.target.closest; if(!closest) return; var a = closest.call(e.target, \"a[href^='?theme='], a[href*='?theme=']\"); if(!a) return; try{ e.preventDefault(); var href = a.getAttribute('href') || ''; var u = new URL(href, window.location.href); var t = u.searchParams.get('theme'); if(t){ localStorage.setItem('tabler-theme', t); document.documentElement.setAttribute('data-bs-theme', t); var sp = new URLSearchParams(window.location.search); sp.set('theme', t); history.replaceState(null, '', window.location.pathname + (sp.toString() ? ('?' + sp.toString()) : '') + window.location.hash); document.dispatchEvent(new Event('tabler:themechange')); } }catch(err){} }, false);}catch(e){} })()"
  )

  if (!isTRUE(show_theme_button)) {
    # remove theme anchors if requested
    script_lines <- c(script_lines, "try{document.querySelectorAll('a.hide-theme-dark,a.hide-theme-light').forEach(function(n){var p=n.parentElement;p&&p.classList.contains('nav-item')?p.remove():n.remove()})}catch(e){}")
  }
  script_text <- paste(script_lines, collapse = ";")

  # Build head and body separately and return a tagList. Returning a full
  # <html> tag would cause Shiny to nest html/body (Shiny already provides
  # those), which breaks rendering. Attach dependencies to the returned
  # tag list so resources are included in the final document.
  html_head <- head(
    meta(charset = "utf-8"),
    meta(name = "viewport", content = "width=device-width, initial-scale=1, viewport-fit=cover"),
    meta(`http-equiv` = "X-UA-Compatible", content = "ie=edge"),
    # Use an actual <title> tag for the document head. The local helper
    # `title()` renders an <h2> element intended for page body headers and
    # accidentally added that to the head when used here.
    if (!is.null(title)) htmltools::tags$title(title),
    script(HTML(script_text))
  )

  html_body <- do.call(body_tag, c(
    body_attrs,
    list(page_content)
  ))

  # Return head and body as a tag list; add_deps will attach dependencies so
  # CSS/JS are included in the Shiny page head.
  add_deps(tagList(html_head, html_body), layout = layout, theme = theme, color = color)
}

#' @title Create a Topbar Header
#' @description Helper to create a top navigation header. This replaces the
#' previous top-navbar behavior when users passed a header-like component.
#' @param title Brand title/text
#' @param brand_image URL or path to brand image
#' @param ... Additional header content (e.g., nav items)
#' @return An HTML tag representing the top header
#' @rdname tabler-page
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
          li_items[[1]]$children[[1]] <- tagAppendAttributes(anchor, class = "active")
        }
      }

      nav_children <- ul(class = "navbar-nav", li_items)
    }
  }

  header(
    class = "navbar navbar-expand-md navbar-light d-print-none",
    div(
      class = "container-xl",
      div(
        class = "navbar-brand navbar-brand-autodark d-none-navbar-horizontal pe-0 pe-md-3",
        if (!is.null(brand_image)) {
          a(href = "#", img(src = brand_image, alt = title %||% "Dashboard", class = "navbar-brand-image"))
        } else if (!is.null(title)) {
          a(href = "#", title)
        }
      ),
      # place any raw passed elements (e.g., anchors, buttons) and the wrapped nav items
      div(
        class = "navbar-nav flex-row order-md-last",
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
#' @return An HTML tag representing the footer
#' @rdname tabler-page
#' @export
footer <- function(left = NULL, right = NULL) {
  footer_tag(
    class = "footer footer-transparent d-print-none",
    div(
      class = "container-xl",
      div(
        class = "row text-center align-items-center flex-row-reverse",
        if (!is.null(right)) {
          div(
            class = "col-lg-auto ms-lg-auto",
            right
          )
        },
        if (!is.null(left)) {
          div(
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
#' @return An HTML tag representing the sidebar menu
#' @rdname tabler-page
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
          items[[1]]$children[[1]] <- tagAppendAttributes(anchor, class = "active")
        }
      }
    }
  }

  ul <- ul(
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
#' @return An HTML tag representing the horizontal menu
#' @rdname tabler-page
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
          items[[1]]$children[[1]] <- tagAppendAttributes(anchor, class = "active")
        }
      }
    }
  }

  ul(
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
#' @return An HTML tag representing the menu item
#' @rdname tabler-page
#' @export
menu_item <- function(text, tab_name = NULL, icon = NULL, href = NULL, badge = NULL) {
  # Icon element: render the wrapper and use icon (font/webfont) inside.
  # For static examples we include an HTML comment indicating where an SVG
  # could be placed (keeps markup similar to upstream examples without
  # inlining raw SVGs).
  icon_el <- if (!is.null(icon)) {
    list(
      span(
        class = "nav-link-icon d-md-none d-lg-inline-block",
        # comment placeholder for upstream SVG
        HTML(sprintf("<!-- Download SVG icon from http://tabler.io/icons/icon/%s -->", icon)),
        icon(icon)
      ),
      " "
    )
  }

  # Badge element
  badge_el <- if (!is.null(badge)) {
    span(
      class = "badge badge-sm bg-green-lt text-uppercase ms-auto",
      badge
    )
  }

  # Link attributes
  link_attrs <- list(
    class = "nav-link",
    href = href %||% "./"
  )

  if (!is.null(tab_name)) {
    link_attrs[["data-toggle"]] <- "tab"
    link_attrs[["data-target"]] <- paste0("#", tab_name)
  }

  li(
    class = "nav-item",
    do.call(a, c(
      link_attrs,
      list(
        icon_el,
        span(class = "nav-link-title", text),
        badge_el
      )
    ))
  )
}

#' @title Create a Dropdown Menu Item
#' @description Create a nav-item with a dropdown menu using columns like the example
#' @param text Title of the dropdown
#' @param icon Icon name (optional)
#' @param href Link for the dropdown toggle (optional)
#' @param items A list of character vectors or tags representing dropdown links; each element can be a character vector of length 2: c(text, href)
#' @return An HTML tag representing the dropdown nav item
#' @export
menu_dropdown <- function(text, icon = NULL, href = NULL, items = list()) {
  # build the anchor toggle
  toggle_attrs <- list(
    class = "nav-link dropdown-toggle",
    href = href %||% "#",
    `data-bs-toggle` = "dropdown",
    # default to header dropdown behavior (matches official header examples)
    `data-bs-auto-close` = "outside",
    role = "button",
    # default not expanded for header dropdowns
    `aria-expanded` = "false"
  )

  anchor <- do.call(a, c(toggle_attrs, list(
    if (!is.null(icon)) {
      span(
        class = "nav-link-icon d-md-none d-lg-inline-block",
        HTML(sprintf("<!-- Download SVG icon from http://tabler.io/icons/icon/%s -->", icon)),
        icon(icon)
      )
    },
    span(class = "nav-link-title", text)
  )))

  # build dropdown menu columns: split items into two roughly equal columns
  n <- length(items)
  if (n == 0) {
    menu_body <- div(class = "dropdown-menu")
  } else {
    half <- ceiling(n / 2)
    col1 <- items[1:half]
    col2 <- if (n > half) items[(half + 1):n] else list()

    make_links <- function(lst) {
      lapply(lst, function(it) {
        if (is.character(it) && length(it) >= 2) {
          a(class = "dropdown-item", href = it[2], it[1])
        } else if (inherits(it, "shiny.tag")) {
          it
        } else {
          a(class = "dropdown-item", href = "#", as.character(it))
        }
      })
    }

    menu_body <- div(
      class = "dropdown-menu",
      div(
        class = "dropdown-menu-columns",
        div(class = "dropdown-menu-column", make_links(col1)),
        div(class = "dropdown-menu-column", make_links(col2))
      )
    )
  }

  li(class = "nav-item active dropdown", anchor, menu_body)
}

#' @title Create Tab Items Container
#' @description Container for multiple tab panels in tabbed layouts
#' @param ... Tab items created with tablerTabItem()
#' @return An HTML tag representing the tab items container
#' @rdname tabler-page
#' @export
tab_items <- function(...) {
  items <- list(...)

  # Make first item active by default
  if (length(items) > 0) {
    first_item <- items[[1]]
    if (inherits(first_item, "shiny.tag")) {
      # Add 'show active' classes to first tab
      items[[1]] <- tagAppendAttributes(first_item, class = "show active")
    }
  }

  div(
    class = "tab-content",
    items
  )
}

#' @title Create a Tab Item
#' @description Individual tab panel content
#' @param tab_name Unique identifier for the tab (must match menuItem tab_name)
#' @param ... Content for this tab
#' @return An HTML tag representing the tab item
#' @rdname tabler-page
#' @export
tab_item <- function(tab_name, ...) {
  div(
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
  # Map layout name to the body class used in the official examples
  layout_class <- switch(layout,
    "boxed" = "layout-boxed",
    "combo" = "layout-combo",
    "condensed" = "layout-condensed",
    "fluid-vertical" = "layout-fluid-vertical",
    "horizontal" = "layout-horizontal",
    "navbar-dark" = "layout-navbar-dark",
    "navbar-overlap" = "layout-navbar-overlap",
    "navbar-sticky" = "layout-navbar-sticky",
    "navbar-sticky-dark" = "layout-navbar-sticky",
    "rtl" = "layout-rtl",
    "vertical" = "layout-vertical",
    "vertical-right" = "layout-vertical-right",
    "vertical-transparent" = "layout-vertical-transparent",
    "fluid" = "layout-fluid",
    NULL
  )

  attrs <- list()
  if (!is.null(layout_class)) attrs$class <- layout_class

  attrs
}

#' @title Build a standard navbar/header
#' @description Create the top header used in boxed layout. This function
#' accepts raw `li` items (created by `menu_item`) or a `sidebar_brand()`
#' attached as `title` attribute on a `ul`.
#' @param ... Additional nodes to include (typically `menu_item()` elements)
#' @param brand Optional brand for the navbar; either a string (text title),
#'  `sidebar_brand(...)` or a named list with elements `text` and `img`
#'  (URL/path).
#' @param show_theme_button Whether to show the theme toggle buttons (default: `FALSE``).
#' @return An HTML tag for the header
#' @export
navbar_menu <- function(..., brand = NULL, show_theme_button = FALSE) {
  items <- list(...)

  # Collect only <li> elements for the main menu
  li_items <- lapply(items, function(x) if (inherits(x, "shiny.tag") && x$name == "li") x else NULL)
  li_items <- Filter(Negate(is.null), li_items)

  # Brand block (simple link or image). Keep as a node to be included in the menu
  brand_tag <- NULL
  if (!is.null(brand)) {
    if (is.character(brand)) {
      brand_tag <- a(href = "./", brand)
    } else if (is.list(brand) && !is.null(brand$text)) {
      brand_tag <- a(
        href = brand$href %||% "./",
        if (!is.null(brand$img)) img(src = brand$img, class = "navbar-brand-image"),
        brand$text
      )
    }
  }

  # Theme toggles (anchors only; the caller wraps them into the proper li)
  theme_toggles <- NULL
  if (isTRUE(show_theme_button)) {
    theme_toggles <- list(
      a(
        href = "?theme=dark", class = "nav-link px-0 hide-theme-dark", title = "Enable dark mode", `data-bs-toggle` = "tooltip", `data-bs-placement` = "bottom",
        HTML('<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="icon icon-1"><path d="M12 3c.132 0 .263 0 .393 0a7.5 7.5 0 0 0 7.92 12.446a9 9 0 1 1 -8.313 -12.454z"/></svg>')
      ),
      a(
        href = "?theme=light", class = "nav-link px-0 hide-theme-light", title = "Enable light mode", `data-bs-toggle` = "tooltip", `data-bs-placement` = "bottom",
        HTML('<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="icon icon-1"><path d="M12 12m-4 0a4 4 0 1 0 8 0a4 4 0 1 0 -8 0"/><path d="M3 12h1m8 -9v1m8 8h1m-9 8v1m-6.4 -15.4l.7 .7m12.1 -.7l-.7 .7m0 11.4l.7 .7m-12.1 -.7l-.7 .7"/></svg>')
      )
    )
  }

  # If brand is a sidebar_brand-like list, render an <aside> suitable for
  # vertical layouts (sidebar). Otherwise render a standard header navbar.
  if (is.list(brand) && (!is.null(brand$img) || !is.null(brand$text))) {
    # For sidebar, modify theme toggles to have text labels and no px-0 class
    if (!is.null(theme_toggles)) {
      theme_toggles <- list(
        a(
          href = "?theme=dark", class = "nav-link hide-theme-dark", title = "Enable dark mode", `data-bs-toggle` = "tooltip", `data-bs-placement` = "bottom", `aria-label` = "Enable dark mode",
          span(
            class = "nav-link-icon d-md-none d-lg-inline-block",
            HTML('<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="icon icon-1"><path d="M12 3c.132 0 .263 0 .393 0a7.5 7.5 0 0 0 7.92 12.446a9 9 0 1 1 -8.313 -12.454z"/></svg>')
          ),
          span(class = "nav-link-title", "Dark theme")
        ),
        a(
          href = "?theme=light", class = "nav-link hide-theme-light", title = "Enable light mode", `data-bs-toggle` = "tooltip", `data-bs-placement` = "bottom", `aria-label` = "Enable light mode",
          span(
            class = "nav-link-icon d-md-none d-lg-inline-block",
            HTML('<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="icon icon-1"><path d="M12 12m-4 0a4 4 0 1 0 8 0a4 4 0 1 0 -8 0"/><path d="M3 12h1m8 -9v1m8 8h1m-9 8v1m-6.4 -15.4l.7 .7m12.1 -.7l-.7 .7m0 11.4l.7 .7m-12.1 -.7l-.7 .7"/></svg>')
          ),
          span(class = "nav-link-title", "Light theme")
        )
      )
    }

    # build collapse body for sidebar
    # For sidebar context, ensure dropdown anchors use sidebar behavior
    li_items <- lapply(li_items, function(li) {
      if (inherits(li, "shiny.tag") && li$name == "li" && length(li$children) > 0) {
        anchor <- li$children[[1]]
        if (inherits(anchor, "shiny.tag") && !is.null(anchor$attribs[["data-bs-toggle"]]) && anchor$attribs[["data-bs-toggle"]] == "dropdown") {
          # Overwrite dropdown attributes for sidebar context instead of appending
          # (tagAppendAttributes concatenates values, producing "outside false" etc.)
          anchor$attribs[["data-bs-auto-close"]] <- "false"
          anchor$attribs[["aria-expanded"]] <- "true"
          li$children[[1]] <- anchor
        }
      }
      li
    })

    aside(
      class = "navbar navbar-vertical",
      HTML("<!-- BEGIN SIDEBAR -->"),
      div(
        class = "container-fluid",
        # toggler
        button_tag(
          class = "navbar-toggler",
          type = "button",
          `data-bs-toggle` = "collapse",
          `data-bs-target` = "#sidebar-menu",
          `aria-controls` = "sidebar-menu",
          `aria-expanded` = "false",
          `aria-label` = "Toggle navigation",
          span(class = "navbar-toggler-icon")
        ),
        # brand block
        div(
          class = "navbar-brand navbar-brand-autodark",
          a(
            href = brand$href %||% "./",
            if (!is.null(brand$img)) img(src = brand$img, class = "navbar-brand-image"),
            brand$text %||% ""
          )
        ),
        # collapsible menu
        div(
          class = "collapse navbar-collapse", id = "sidebar-menu",
          ul(
            class = "navbar-nav pt-lg-3",
            li_items,
            # theme toggles appended as nav-item(s) - wrap plain anchors into li
            if (!is.null(theme_toggles)) li(class = "nav-item mt-auto", lapply(theme_toggles, function(x) x))
          )
        )
      ),
      HTML("<!-- END SIDEBAR -->")
    )
  } else {
    # standard header
    header(
      class = "navbar navbar-expand-md",
      HTML("<!-- BEGIN NAVBAR  -->"),
      div(
        class = "collapse navbar-collapse",
        id = "navbar-menu",
        div(
          class = "container-xl",
          div(
            class = "row flex-column flex-md-row flex-fill align-items-center",
            div(
              class = "col",
              ul(
                class = "navbar-nav",
                if (!is.null(brand_tag)) li(class = "nav-item", brand_tag),
                # ensure li_items dropdown anchors are normalized for header context
                lapply(li_items, function(it) {
                  if (inherits(it, "shiny.tag") && it$name == "li" && length(it$children) > 0) {
                    a <- it$children[[1]]
                    if (inherits(a, "shiny.tag") && !is.null(a$attribs[["data-bs-toggle"]]) && a$attribs[["data-bs-toggle"]] == "dropdown") {
                      a$attribs[["aria-expanded"]] <- "false"
                      it$children[[1]] <- a
                    }
                  }
                  it
                }),
                if (!is.null(theme_toggles)) {
                  # Render header theme toggles as a single nav-item with two anchors
                  li(
                    class = "nav-item ms-md-auto",
                    lapply(theme_toggles, function(a) {
                      # theme_toggles are anchor tags; ensure they keep their attributes
                      a
                    })
                  )
                }
              )
            )
          )
        )
      ),
      HTML("<!-- END NAVBAR  -->")
    )
  }
}
