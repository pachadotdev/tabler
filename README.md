# Tabler for R

<!-- badges: start -->
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/pachadotdev/tabler/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pachadotdev/tabler/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/tabler)](https://CRAN.R-project.org/package=tabler)
[![Test
coverage](https://raw.githubusercontent.com/pachadotdev/tabler/coverage/badges/coverage.svg)](https://github.com/pachadotdev/tabler/actions/workflows/test-coverage.yaml)
[![BuyMeACoffee](https://raw.githubusercontent.com/pachadotdev/buymeacoffee-badges/main/bmc-blue.svg)](https://buymeacoffee.com/pacha)
<!-- badges: end -->

A modern dashboard framework for R using the beautiful Tabler Bootstrap theme. To render Tabler apps using a server, see [Tabler Server](https://github.com/pachadotdev/tabler-server).

<iframe width="560" height="315" src="https://www.youtube.com/embed/_PWVmmis-AE?si=wJYMvUQUpoZz_k3_" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen>

</iframe>

## Installation

Old version, depends on Shiny:

```r
install.packages("tabler", repos = "https://cran.r-project.org")
```

New version, does not use Shiny:

```r
# using the R-Universe
install.packages("tabler", repos = "https://pachadotdev.r-universe.dev")

# or using the remotes package
remotes::install_github("pachadotdev/tabler")
```

## Quick Start

### Single-script app

The following example uses the "combo" layout to recreate Shiny's geyser example. The theme options
can be adjusted from the code or the theme setting icon that can be hidden. See the example
[here](https://github.com/pachadotdev/tabler/blob/main/inst/extdata/app-template/combo-layout.R).

<figure>
<img src="screenshots/combo-layout-light.png" title="Light theme + teal colour + zinc base"
alt="layout-geyser" />
</figure>

<figure>
<img src="screenshots/combo-layout-dark.png" title="Dark theme + cyan colour + slate base"
alt="layout-geyser" />
</figure>

<figure>
<img src="screenshots/theme-selection.png" title="Theme selection"
alt="layout-geyser" />
</figure>

I added a UI-only example to cover the different input elements and their options [here](https://github.com/pachadotdev/tabler/blob/main/inst/extdata/app-template/boxed-layout-all-ui-elements.R).

<figure>
<img src="screenshots/select.png" title="Select & Multi-Select"
alt="select" />
</figure>

<figure>
<img src="screenshots/sliders.png" title="Sliders"
alt="sliders" />
</figure>

<figure>
<img src="screenshots/dates.png" title="Dates"
alt="dates" />
</figure>

<figure>
<img src="screenshots/radio-checkboxes.png" title="Radio & Checkboxes"
alt="radio-checkboxes" />
</figure>

<figure>
<img src="screenshots/text-numeric-buttons.png" title="Text, Numeric & buttons"
alt="text-numeric-buttons" />
</figure>

### Modular R package app

Create an R package with modular components:

```
library(tabler)

pkg_template("mydashboard")
```

See the package skeleton [here](https://github.com/pachadotdev/tabler/tree/main/inst/extdata/pkg-template). `pkg_template()` adds a `DESCRIPTION` and other components required for an R package to work.

I built a full dashboard that uses environment variables, SQL connections, caching, and D3 plots [here](https://github.com/pachadotdev/tradestatistics-dashboard).

## Login page

This R package provides a login page that you can connect to a database or another system. The
example [here](https://github.com/pachadotdev/tabler/blob/main/inst/extdata/app-template/combo-layout-with-login.R) shows the
dashboard after correctly typing the user "SpaceMariner" and password "IDDQD". There is an example using RSQLite
[here](https://github.com/pachadotdev/tabler/blob/main/inst/extdata/app-template/combo-layout-with-login-sqlite.R).

I was thinking about adding a Google/Outlook/GitHub account login but I have no idea how to. If
you know how and would like to contribute, please comment [here](https://github.com/pachadotdev/tabler/issues/2).

<figure>
<img src="screenshots/combo-layout-sign-in.png" title="Sign in"
alt="sign-in" />
</figure>

<figure>
<img src="screenshots/combo-layout-sign-out.png" title="Dashboard with sing out button"
alt="sign-out" />
</figure>

## Does it run Doom?

Yes. I tested the WebSocket compiling and running the Doom WASM version. See the code
[here](https://github.com/pachadotdev/tabler/blob/main/dev/doom.R).

<figure>
<img src="screenshots/doom.png" title="Doom"
alt="doom" />
</figure>

## Available Layouts

There are [additional examples](https://github.com/pachadotdev/tabler/tree/main/examples) for each of the following layouts:

- **Boxed (Default)**: Basic dashboard with top navbar and constrained
  width content area. This is the default layout.
- **Combo**: Combines vertical sidebar navigation with top header.
- **Condensed**: Compact layout with reduced padding/margins.
- **Fluid**: Full-width layout without container constraints.
- **Fluid Vertical**: Full-width layout with vertical sidebar.
- **Horizontal**: Layout with horizontal navigation menu.
- **Navbar Dark**: Layout with dark navbar theme.
- **Navbar Overlap**: Layout where content overlaps with navbar for a
  modern look.
- **Navbar Sticky**: Layout with sticky/fixed navbar that stays at the
  top when scrolling.
- **RTL**: Right-to-left layout for Hebrew/Arabic languages.
- **Vertical**: Vertical sidebar layout without top navbar.
- **Vertical Right**: Vertical sidebar positioned on the right side.
- **Vertical Transparent**: Vertical layout with transparent sidebar.

Note: `tabler` allows to pass `layout = "navbar"` and `layout = "navbar-sticky-dark"` which are wrappers
for a light theme navbar layout and a dark theme sticky navbar layour, respectively.

## Differences with Shiny

- Static plots (base, ggplot, tinyplot, etc.) render as SVG and can be downloaded with the right click button.
- URLs are of the form `my.site/myapp?year=2000&country=gbr` instead of `my.site/myapp?year=2000&country=%22gbr%22` 

## License

Apache License (\>= 2)
