#' @title Build boxed (and combo) layout structure
#' @description Build the HTML structure for the boxed layout. This
#' accepts either a single navbar/sidebar tag, or a named list with
#' elements `top` and `side` for combo-style callers. The function
#' returns the full page tag structure (including sidebar, navbar,
#' page wrapper, body and footer).
#' @param navbar Navbar component (or list with top/side for combo)
#' @param sidebar Sidebar component (not typically used when passing list)
#' @param body Body content
#' @param footer Footer component
#' @keywords internal
layout_combo <- function(navbar, sidebar, body, footer, theme = "light", color = NULL, show_theme_button = TRUE) {
	top_nav <- NULL
	side_nav <- NULL

	# Support both list(navbar = list(top=..., side=...)) and direct tags
	if (is.list(navbar) && !inherits(navbar, "shiny.tag")) {
		top_nav <- navbar$top
		side_nav <- navbar$side
	} else if (!is.null(navbar)) {
		if (inherits(navbar, "shiny.tag")) {
			tag_name <- navbar$name
			if (tag_name == "header") {
				top_nav <- navbar
			} else if (tag_name == "aside") {
				side_nav <- navbar
			} else if (tag_name == "ul") {
				# treat ul as sidebar for combo layout when passed directly
				side_nav <- navbar
			}
		}
	}

	# Mirror the HTML structure in layout-combo.html: page contains aside (sidebar), header (top), then page-wrapper
	shiny::tags$div(
		class = "page",

		# Sidebar (if present)
		if (!is.null(side_nav)) side_nav,

		# Navbar (top) - show on wide viewports
		if (!is.null(top_nav)) top_nav,

		# Main content wrapper
		shiny::tags$div(
			class = "page-wrapper",

			# Page header / pretitle/title may be part of body; just include body here
			shiny::tags$div(
				class = "page-body",
				body
			),

			# Footer
			if (!is.null(footer)) footer
		)
	)
}
