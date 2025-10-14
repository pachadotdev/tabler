# Tabler Dashboard for Shiny

A modern dashboard framework for R Shiny using the beautiful Tabler Bootstrap theme.

## Overview

Uses Shiny

## Installation

```r
pak::pak("pachadotdev/tabler")
```

## Quick Start

```r
library(shiny)
library(tabler)

...

shinyApp(ui, server)
```

## Available Layouts

### 1. Default Layout

Basic dashboard with top navbar and main content area.

```r
ui <- tabler_page(
  title = "Default Layout",
  layout = "default",
  navbar = tabler_navbar(title = "My App"),
  body = tabler_body(
    # Your content here
  ),
  footer = tabler_footer(left = "© 2024", right = "Built with Tabler")
)
```

### 2. Boxed Layout (`layout-boxed.html`)

Constrained width layout with centered content.

```r
ui <- tabler_page(
  title = "Boxed Layout", 
  layout = "boxed",
  navbar = tabler_navbar(title = "My App"),
  body = tabler_body(
    # Your content here - constrained width
  )
)
```

### 3. Combo Layout (`layout-combo.html`)

Combines vertical sidebar navigation with top header.

```r
ui <- tabler_page(
  title = "Combo Layout",
  layout = "combo",
  navbar = tabler_navbar(title = "My App"),
  sidebar = tabler_sidebar(
    tabler_nav_menu(
      tabler_nav_item("Dashboard", tabName = "dashboard", icon = "home"),
      tabler_nav_item("Users", tabName = "users", icon = "users"),
      tabler_nav_item("Settings", tabName = "settings", icon = "settings")
    ),
    theme = "dark"
  ),
  body = tabler_body(
    # Your content here
  )
)
```

### 4. Condensed Layout (`layout-condensed.html`)

Compact layout with reduced spacing.

```r
ui <- tabler_page(
  title = "Condensed Layout",
  layout = "condensed",
  navbar = tabler_navbar(title = "My App"),
  body = tabler_body(
    # Your content here - condensed spacing
  )
)
```

### 5. Fluid Layout (`layout-fluid.html`)

Full-width layout without container constraints.

```r
ui <- tabler_page(
  title = "Fluid Layout",
  layout = "fluid",
  navbar = tabler_navbar(title = "My App"),
  body = tabler_body(
    # Your content here - full width
  )
)
```

### 6. Fluid Vertical Layout (`layout-fluid-vertical.html`)

Full-width layout with vertical sidebar.

```r
ui <- tabler_page(
  title = "Fluid Vertical Layout",
  layout = "fluid-vertical",
  sidebar = tabler_sidebar(
    tabler_nav_menu(
      tabler_nav_item("Dashboard", icon = "home"),
      tabler_nav_item("Users", icon = "users")
    )
  ),
  body = tabler_body(
    # Your content here - full width with sidebar
  )
)
```

### 7. Horizontal Layout (`layout-horizontal.html`)

Centered horizontal layout.

```r
ui <- tabler_page(
  title = "Horizontal Layout",
  layout = "horizontal",
  navbar = tabler_navbar(title = "My App"),
  body = tabler_body(
    # Your content here - centered horizontally
  )
)
```

### 8. Navbar Dark Layout (`layout-navbar-dark.html`)

Layout with dark navbar theme.

```r
ui <- tabler_page(
  title = "Dark Navbar Layout",
  layout = "navbar-dark",
  navbar = tabler_navbar(title = "My App"),
  body = tabler_body(
    # Your content here - with dark navbar
  )
)
```

### 9. Navbar Overlap Layout (`layout-navbar-overlap.html`)

Layout where content overlaps with navbar.

```r
ui <- tabler_page(
  title = "Navbar Overlap Layout",
  layout = "navbar-overlap",
  navbar = tabler_navbar(title = "My App"),
  body = tabler_body(
    # Your content here - overlapping navbar
  )
)
```

### 10. Navbar Sticky Layout (`layout-navbar-sticky.html`)

Layout with sticky/fixed navbar.

```r
ui <- tabler_page(
  title = "Sticky Navbar Layout",
  layout = "navbar-sticky",
  navbar = tabler_navbar(title = "My App"),
  body = tabler_body(
    # Your content here - sticky navbar
  )
)
```

### 11. RTL Layout (`layout-rtl.html`)

Right-to-left layout for Arabic/Hebrew languages.

```r
ui <- tabler_page(
  title = "RTL Layout",
  layout = "rtl",
  navbar = tabler_navbar(title = "שלום"),
  body = tabler_body(
    # Your content here - right-to-left
  )
)
```

### 12. Vertical Layout (`layout-vertical.html`)

Pure vertical sidebar layout without top navbar.

```r
ui <- tabler_page(
  title = "Vertical Layout",
  layout = "vertical",
  sidebar = tabler_sidebar(
    tabler_nav_menu(
      tabler_nav_item("Dashboard", icon = "home"),
      tabler_nav_item("Users", icon = "users"),
      tabler_nav_item("Settings", icon = "settings")
    )
  ),
  body = tabler_body(
    # Your content here - sidebar only
  )
)
```

### 13. Vertical Right Layout (`layout-vertical-right.html`)

Vertical sidebar positioned on the right side.

```r
ui <- tabler_page(
  title = "Vertical Right Layout",
  layout = "vertical-right",
  sidebar = tabler_sidebar(
    tabler_nav_menu(
      tabler_nav_item("Dashboard", icon = "home"),
      tabler_nav_item("Users", icon = "users")
    )
  ),
  body = tabler_body(
    # Your content here - right sidebar
  )
)
```

### 14. Vertical Transparent Layout (`layout-vertical-transparent.html`)

Vertical layout with transparent/semi-transparent sidebar.

```r
ui <- tabler_page(
  title = "Vertical Transparent Layout",
  layout = "vertical-transparent",
  sidebar = tabler_sidebar(
    tabler_nav_menu(
      tabler_nav_item("Dashboard", icon = "home"),
      tabler_nav_item("Users", icon = "users")
    ),
    theme = "light"
  ),
  body = tabler_body(
    # Your content here - transparent sidebar
  )
)
```

## Navigation Components

### Sidebar

```r
tabler_sidebar(
  tabler_nav_menu(
    tabler_nav_item("Item 1", tabName = "tab1", icon = "home"),
    tabler_nav_item("Item 2", tabName = "tab2", icon = "users", badge = "New")
  ),
  theme = "dark"  # or "light"
)
```

### Navigation Items

```r
tabler_nav_item(
  text = "Dashboard",
  tabName = "dashboard",  # For tab switching
  icon = "home",         # Icon name
  href = "/dashboard",   # Alternative to tabName  
  badge = "New"          # Optional badge
)
```

## Layout Structure

The package automatically handles the HTML structure for each layout type:

- **Default/Boxed/Fluid**: Simple page wrapper with navbar and content
- **Combo**: Sidebar + header + content area
- **Navbar Overlap**: Special styling for overlapping content

## CSS Classes Applied

- `layout-boxed`: Applied to body for boxed layout
- `layout-fluid`: Applied to body for fluid layout  
- `layout-navbar-overlap`: Applied to body for navbar overlap
- `data-bs-theme="dark"`: Applied to sidebar for dark theme

```r
tabler_page(
  layout = "combo",  # Choose your layout
  navbar = tabler_navbar(),
  sidebar = tabler_sidebar(),  # New sidebar support
  body = tabler_body()
)
```

## License

Apache License (>= 2)
