# tabler 1.4.0 (development version)

* `tablerApp()` gains a `githubAuth` argument for GitHub OAuth login (server-side Authorization Code flow with CSRF state-token protection). Pass a list with `clientId`, `clientSecret`, and optionally `org` (restrict to GitHub org members) or `allowedUsers` (restrict to named GitHub users). Cannot be combined with `checkCredentials`.

# tabler 0.03

* Adds themes, colours, templated inputs, calendars and an almost comprehensive
  port of Tabler UI.
* Includes a simple login.

# tabler 0.2.0

* `selectInput()` gains a `searchable` argument (default `TRUE`) that overlays
  the dropdown with a type-to-filter text box matching anywhere in an
  option's label, instead of the browser's native jump-to-prefix behavior.
* Adds Tabler buttons and sliders.

# tabler 0.1.0

* First stable release.
* Initial CRAN submission.
