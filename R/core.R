## Minimal core.R (diet): only the helpers and builders used by tests

## Layout helpers moved to dedicated files:
# see R/layout-boxed.R, R/layout-condensed.R

#' @title Helper function for condensed layout (navbar without wrapper)
#' @description Build condensed page structure
#' @param navbar Navbar component
#' @param sidebar Sidebar component
#' @param body Body content
#' @param footer Footer component
#' @return A Shiny tag representing the page structure
## Diet core: minimal helpers used by tests

`%||%` <- function(x, y) if (is.null(x)) y else x

# add_deps: inject minimal dependencies (CSS/JS/fonts) into generated HTML
# This is a small, safe implementation for preview generation that mirrors
# the essential assets used by the official previews (styles + theme script).
add_deps <- function(tag, layout = NULL, theme = NULL, color = NULL, title = NULL) {
  # only operate on an html tag
  if (!inherits(tag, "shiny.tag") || tag$name != "html") return(tag)

  # Build a minimal head similar to the preview pages
  head_children <- list(
    shiny::tags$meta(charset = "utf-8"),
    shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, viewport-fit=cover"),
    # Page title from tabler_page() when provided
    if (!is.null(title)) shiny::tags$title(title) else NULL,
    # Basic meta used by preview pages (non-identifying)
    shiny::tags$meta(name = "theme-color", content = "#066fd1"),
    shiny::tags$link(rel = "icon", href = "favicon.ico", type = "image/x-icon"),
    # Core tabler CSS (relative paths match preview/packaged assets)
    shiny::tags$link(href = "dist/css/tabler.min.css", rel = "stylesheet"),
    # Additional plugin/theme CSS commonly referenced in previews
    shiny::tags$link(href = "dist/css/tabler-flags.min.css", rel = "stylesheet"),
    shiny::tags$link(href = "dist/css/tabler-socials.min.css", rel = "stylesheet"),
    shiny::tags$link(href = "dist/css/tabler-payments.min.css", rel = "stylesheet"),
    shiny::tags$link(href = "dist/css/tabler-vendors.min.css", rel = "stylesheet"),
    shiny::tags$link(href = "dist/css/tabler-marketing.min.css", rel = "stylesheet"),
    shiny::tags$link(href = "dist/css/tabler-themes.min.css", rel = "stylesheet"),
    # Demo CSS used on preview pages (keeps layout/look similar)
    shiny::tags$link(href = "preview/css/demo.min.css", rel = "stylesheet"),
    # Optional small plugin styles can be added here if needed
    # Import the Inter font used in the official previews
    shiny::tags$style(shiny::HTML("@import url('https://rsms.me/inter/inter.css');"))
  )

  # Replace or populate the existing <head>
  if (!is.null(tag$children) && length(tag$children) >= 1 && inherits(tag$children[[1]], "shiny.tag") && tag$children[[1]]$name == "head") {
    # Merge with any existing head children but prefer our head_children ordering
    tag$children[[1]]$children <- Filter(Negate(is.null), head_children)
  } else {
    # Prepend a head tag if missing (unlikely)
    tag$children <- c(list(shiny::tags$head(Filter(Negate(is.null), head_children))), tag$children)
  }

  # Insert the theme script at the start of the body for parity with previews
  theme_script <- shiny::tags$script(src = "dist/js/tabler-theme.min.js")
  body_idx <- length(tag$children) # body is usually the last child
  if (!is.null(tag$children[[body_idx]]) && inherits(tag$children[[body_idx]], "shiny.tag") && tag$children[[body_idx]]$name == "body") {
    tag$children[[body_idx]]$children <- c(list(theme_script), tag$children[[body_idx]]$children)
  }

  tag
}

tabler_page <- function(title = NULL, navbar = NULL, body = NULL, footer = NULL,
                        layout = "boxed", theme = "light", color = "blue") {
  valid_layouts <- c("boxed", "vertical", "vertical-right", "horizontal", "fluid", "fluid-vertical", "combo", "condensed")
  if (!layout %in% valid_layouts) stop("Invalid layout. Must be one of: ", paste(valid_layouts, collapse = ", "))
  page_content <- get_layout_structure(layout, navbar, body, footer, theme = theme, color = color)
  html_attrs <- if (identical(layout, "rtl")) list(dir = "rtl", lang = "en") else list(lang = "en")
  # When using the boxed layout the official preview sets the body class to "layout-boxed"
  # Some previews use a body class for fluid layouts; include combo
  body_attrs <- if (identical(layout, "boxed")) list(class = "layout-boxed") else if (layout %in% c("fluid", "fluid-vertical", "condensed", "combo")) list(class = "layout-fluid") else list()
  html_tag <- do.call(shiny::tags$html, c(html_attrs, list(shiny::tags$head(), do.call(shiny::tags$body, c(body_attrs, list(page_content))))))
  # For boxed/combo layouts append modal and settings markup used by the
  # official preview. The helpers are provided in R/components.R (diet versions).
  if (layout %in% c("boxed", "combo") && exists("tabler_modal_report", mode = "function") && exists("tabler_settings", mode = "function")) {
    body_children <- html_tag$children[[length(html_tag$children)]]$children
    # append modal and settings to the body of the html tag (after page content)
    body_children <- c(body_children, list(tabler_modal_report(), tabler_settings()))
    html_tag$children[[length(html_tag$children)]]$children <- body_children
  }

  # Condensed layout uses simpler modal/settings placeholders (different helpers)
  if (identical(layout, "condensed")) {
    # Use fluid helpers if available
    if (exists("tabler_modal_report_fluid", mode = "function") && exists("tabler_settings_fluid", mode = "function")) {
      body_children <- html_tag$children[[length(html_tag$children)]]$children
      body_children <- c(body_children, list(tabler_modal_report_fluid(), tabler_settings_fluid()))
      html_tag$children[[length(html_tag$children)]]$children <- body_children
    } else if (exists("tabler_modal_report_condensed", mode = "function") && exists("tabler_settings_condensed", mode = "function")) {
      body_children <- html_tag$children[[length(html_tag$children)]]$children
      body_children <- c(body_children, list(tabler_modal_report_condensed(), tabler_settings_condensed()))
      html_tag$children[[length(html_tag$children)]]$children <- body_children
    }
  }

  # Combo layout reuses the fluid-vertical structure and should get combo placeholders
  if (identical(layout, "combo") && exists("tabler_modal_report_combo", mode = "function") && exists("tabler_settings_combo", mode = "function")) {
    body_children <- html_tag$children[[length(html_tag$children)]]$children
    body_children <- c(body_children, list(tabler_modal_report_combo(), tabler_settings_combo()))
    html_tag$children[[length(html_tag$children)]]$children <- body_children
  }

  add_deps(html_tag, layout = layout, theme = theme, color = color, title = title)
}

tabler_body <- function(..., class = NULL) {
  shiny::tags$div(class = paste("container-xl", class), ...)
}

tabler_page_header <- function(title, subtitle = NULL, ...) {
  extra <- list(...)
  right_col <- if (length(extra) > 0) shiny::tags$div(class = "col-auto ms-auto", extra) else NULL
  left <- list(shiny::tags$h2(class = "page-title", title), if (!is.null(subtitle)) shiny::tags$div(class = "text-muted mt-1", subtitle) else NULL)
  shiny::tags$div(class = "page-header d-print-none", shiny::tags$div(class = "container-xl d-flex align-items-center", shiny::tags$div(class = "col", left), right_col))
}

topbar <- function(title = NULL, brand_image = NULL, ...) {
  brand_title <- if (!is.null(title)) title else "Dashboard"
  img_tag <- if (!is.null(brand_image)) shiny::tags$img(src = brand_image, class = "navbar-brand-image", alt = brand_title) else NULL
  shiny::tags$header(class = "navbar navbar-expand-md navbar-light d-print-none", shiny::tags$div(class = "container-xl d-flex align-items-center", img_tag, if (!is.null(title)) shiny::tags$h1(class = "navbar-brand", title) else NULL, list(...)))
}

tabler_footer <- function(left = NULL, right = NULL) {
  shiny::tags$footer(class = "footer footer-transparent d-print-none", shiny::tags$div(class = "container-xl", shiny::tags$div(class = "d-flex w-100 justify-content-between", shiny::tags$div(left), shiny::tags$div(right))))
}

sidebar_menu <- function(..., title = NULL) {
  items <- list(...)
  if (length(items) > 0) {
    first_item <- items[[1]]
    if (inherits(first_item, "shiny.tag") && !is.null(first_item$children) && length(first_item$children) > 0) {
      a <- first_item$children[[1]]
      if (inherits(a, "shiny.tag") && a$name == "a") items[[1]]$children[[1]] <- shiny::tagAppendAttributes(a, class = "active", `aria-selected` = "true")
    }
  }
  ul <- shiny::tags$ul(class = "navbar-nav pt-lg-3", items)
  if (!is.null(title)) ul$attribs$title <- title
  ul
}

sidebar_brand <- function(text = NULL, img = NULL, href = "#") {
  res <- list()
  if (!is.null(text)) res$text <- text
  if (!is.null(img)) res$img <- img
  if (!is.null(href)) res$href <- href
  res
}

horizontal_menu <- function(...) {
  items <- list(...)
  if (length(items) > 0) {
    first_item <- items[[1]]
    if (inherits(first_item, "shiny.tag") && !is.null(first_item$children) && length(first_item$children) > 0) {
      anchor <- first_item$children[[1]]
      if (inherits(anchor, "shiny.tag") && anchor$name == "a") items[[1]]$children[[1]] <- shiny::tagAppendAttributes(anchor, class = "active")
    }
  }
  shiny::tags$ul(class = "navbar-nav", items)
}

menu_item <- function(text, tab_name = NULL, icon = NULL, href = NULL, badge = NULL) {
  icon_el <- if (!is.null(icon)) list(shiny::tags$span(class = "nav-link-icon d-md-none d-lg-inline-block", tabler_icon(icon)), " ") else NULL
  badge_el <- if (!is.null(badge)) shiny::tags$span(class = "badge badge-sm bg-green-lt text-uppercase ms-auto", badge) else NULL
  link_attrs <- list(class = "nav-link", href = href %||% "#")
  if (!is.null(tab_name)) {
    link_attrs[["data-bs-toggle"]] <- "tab"
    link_attrs[["data-bs-target"]] <- paste0("#", tab_name)
    link_attrs[["data-toggle"]] <- "tab"
    link_attrs[["data-target"]] <- paste0("#", tab_name)
    link_attrs[["href"]] <- paste0("#", tab_name)
    link_attrs[["id"]] <- paste0("tab-", tab_name)
    link_attrs[["role"]] <- "tab"
    link_attrs[["aria-controls"]] <- tab_name
    link_attrs[["aria-selected"]] <- "false"
  }
  shiny::tags$li(class = "nav-item", do.call(shiny::tags$a, c(link_attrs, list(icon_el, shiny::tags$span(class = "nav-link-title", text), badge_el))))
}

# Tab helpers used by tests
tabler_tab_items <- function(...) {
  items <- list(...)
  any_active <- FALSE
  for (it in items) {
    if (inherits(it, "shiny.tag")) {
      cls <- it$attribs$class %||% ""
      if (grepl("\\b(show|active)\\b", cls)) {
        any_active <- TRUE
        break
      }
      if (!is.null(it$attribs$`data-default`) && identical(as.character(it$attribs$`data-default`), "true")) {
        any_active <- TRUE
        break
      }
    }
  }
  if (!any_active && length(items) > 0) items[[1]] <- shiny::tagAppendAttributes(items[[1]], class = "show active")
  shiny::tags$div(class = "tab-content", items)
}

tabler_tab_item <- function(tab_name, ...) {
  pane_class <- "tab-pane fade pt-3"
  shiny::tags$div(class = pane_class, id = tab_name, role = "tabpanel", `data-tab` = tab_name, `aria-labelledby` = paste0("tab-", tab_name), ...)
}

get_layout_attributes <- function(layout) {
  list(class = "antialiased")
}

get_layout_structure <- function(layout, navbar, body, footer, theme = "light", color = NULL) {
  top_nav <- NULL
  sidebar <- NULL
  topbar <- NULL
  if (is.list(navbar) && !inherits(navbar, "shiny.tag")) {
    top_nav <- navbar$top
    sidebar <- navbar$side
  } else if (!is.null(navbar) && inherits(navbar, "shiny.tag")) {
    tag_name <- navbar$name
    if (tag_name == "aside") {
      sidebar <- navbar
    } else if (tag_name == "header") {
      top_nav <- navbar
    } else if (tag_name == "ul") {
      if (layout %in% c("horizontal")) topbar <- navbar else sidebar <- navbar
    }
  }
  switch(layout,
    "boxed" = build_boxed_layout(top_nav, sidebar, body, footer, theme = theme, color = color),
    "combo" = build_combo_layout(top_nav, sidebar, body, footer, theme = theme, color = color),
    "condensed" = build_condensed_layout(top_nav, sidebar, body, footer, theme = theme, color = color),
    "fluid" = build_fluid_layout(top_nav, sidebar, body, footer, theme = theme, color = color),
    "fluid-vertical" = build_fluid_vertical_layout(top_nav, sidebar, body, footer, theme = theme, color = color),
    "horizontal" = build_horizontal_layout(top_nav, topbar, body, footer, theme = theme, color = color),
    "navbar-dark" = build_navbar_dark_layout(top_nav, sidebar, body, footer, theme = theme, color = color),
    "navbar-overlap" = build_navbar_overlap_layout(top_nav, sidebar, body, footer, theme = theme, color = color),
    "navbar-sticky" = build_navbar_sticky_layout(top_nav, sidebar, body, footer, theme = theme, color = color),
    "rtl" = build_rtl_layout(top_nav, sidebar, body, footer, theme = theme, color = color),
    "vertical" = build_vertical_simple_layout(top_nav, sidebar, body, footer, layout = layout, theme = theme, color = color),
    "vertical-right" = build_vertical_simple_layout(top_nav, sidebar, body, footer, side = "right", layout = layout, theme = theme, color = color),
    "vertical-tranparent" = build_vertical_transparent_layout(top_nav, sidebar, body, footer, theme = theme, color = color),
    stop(
      "Invalid layout: ", layout,
      ".\nValid layouts are: boxed, combo, condensed, fluid, fluid-vertical,",
      "\nhorizontal, navbar-dark, navbar-overlap, navbar-sticky, vertical, vertical-right"
    )
  )
}
