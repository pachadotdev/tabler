#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
	# Read layout choice and HW flag from environment variables (with defaults)
	LAYOUT_TYPE <- Sys.getenv("TABLER_LAYOUT", "vertical")
	HW <- Sys.getenv("TABLER_HW", "no")

	# Common content for all layouts (kept small; most content comes from example_app.R)
	dashboardContent <- tablerBody(
		tablerPageHeader(
			title = "Dashboard Overview",
			subtitle = "Explore penguin and car datasets with interactive visualizations"
		),
		tablerTabItems(
			tablerTabItem(
				tabName = "penguins",
				fluidRow(
					tablerValueBox(value = "344", title = "Total Penguins", icon = "users", color = "primary", width = 3),
					tablerValueBox(value = "3", title = "Species", icon = "brand-github", color = "success", width = 3),
					tablerValueBox(value = "3", title = "Islands", icon = "map-pin", color = "warning", width = 3),
					tablerValueBox(value = "2007-2009", title = "Study Years", icon = "calendar", color = "info", width = 3)
				),
				fluidRow(column(12)),
				fluidRow(
					column(8, tablerCard(title = "Body Mass by Species", highchartOutput("mass_boxplot", height = "400px"), status = "primary")),
					column(4,
						tablerAlert(
							title = "Dataset Information",
							ifelse(HW != "yes",
										 paste("Palmer Penguins data with", LAYOUT_TYPE, "layout."),
										 paste("נתוני פינגווינים עם פריסת", LAYOUT_TYPE)
							),
							type = "info",
							dismissible = TRUE
						),
						tablerCard(title = "Species Distribution", highcharter::highchartOutput("species_pie", height = "300px"))
					)
				),
				fluidRow(
					column(6, tablerCard(title = "Flipper Length vs Body Mass", highchartOutput("scatter_plot", height = "400px"), status = "success")),
					column(6, tablerCard(title = "Bill Length by Island", highchartOutput("bill_column", height = "400px"), status = "warning"))
				),
				fluidRow(column(12, tablerCard(title = "Penguins Count by Year and Species", highcharter::highchartOutput("year_line", height = "350px"), status = "info")))
			),

			tablerTabItem(
				tabName = "mtcars",
				fluidRow(
					tablerValueBox(value = "32", title = "Total Cars", icon = "car", color = "blue", width = 3),
					tablerValueBox(value = "3-8", title = "Cylinder Range", icon = "engine", color = "red", width = 3),
					tablerValueBox(value = "10.4-33.9", title = "MPG Range", icon = "gauge", color = "green", width = 3),
					tablerValueBox(value = "1973-74", title = "Model Years", icon = "calendar", color = "purple", width = 3)
				),
				fluidRow(
					column(8, tablerCard(title = "MPG Distribution by Cylinders", highchartOutput("mpg_boxplot", height = "400px"), status = "blue")),
					column(4,
						tablerAlert(title = "Dataset Information", "Motor Trend Car Road Tests (1974 Motor Trend magazine).", type = "info", dismissible = TRUE),
						tablerCard(title = "Transmission Types", highcharter::highchartOutput("transmission_pie", height = "300px"))
					)
				),
				fluidRow(
					column(6, tablerCard(title = "Weight vs MPG", highchartOutput("weight_scatter", height = "400px"), status = "green")),
					column(6, tablerCard(title = "Horsepower by Cylinders", highchartOutput("hp_column", height = "400px"), status = "red"))
				),
				fluidRow(column(12, tablerCard(title = "Average MPG by Cylinders", highcharter::highchartOutput("mpg_column", height = "350px"), status = "purple")))
			)
		)
	)

	# Build UI based on selected layout
	ui <- switch(
		LAYOUT_TYPE,
		"boxed" = tablerPage(title = "Boxed Layout - Tabler Dashboard", layout = "boxed", navbar = tablerNavbar(title = "Boxed Layout"), body = dashboardContent, footer = tablerFooter(left = "\u00A9 2025 Pacha", right = "Built with Tabler - Boxed Layout")),
		"combo" = tablerPage(title = "Combo Layout - Tabler Dashboard", layout = "combo", navbar = list(top = tablerNavbar(title = "Combo Layout"), side = tablerSidebar(title = "Combo Layout", sidebarMenu(menuItem("Palmer penguins", tabName = "penguins", icon = "home"), menuItem("Motor Trend Cars", tabName = "mtcars", icon = "car", badge = "New")), theme = "dark")), body = dashboardContent, footer = tablerFooter(left = "\u00A9 2025 Pacha", right = "Built with Tabler - Combo Layout")),
		"vertical" = tablerPage(title = "Vertical Layout - Tabler Dashboard", layout = "vertical", navbar = tablerSidebar(title = "Vertical Layout", sidebarMenu(menuItem("Palmer penguins", tabName = "penguins", icon = "home"), menuItem("Motor Trend Cars", tabName = "mtcars", icon = "car")), theme = "dark"), body = dashboardContent, footer = tablerFooter(left = "\u00A9 2025 Pacha", right = "Built with Tabler - Vertical Layout")),
		"condensed" = tablerPage(title = "Condensed Layout - Tabler Dashboard", layout = "condensed", navbar = tablerNavbar(title = "Condensed Layout"), body = dashboardContent, footer = tablerFooter(left = "\u00A9 2025 Pacha", right = "Built with Tabler - Condensed Layout")),
		"navbar-dark" = tablerPage(title = "Dark Navbar Layout - Tabler Dashboard", layout = "navbar-dark", navbar = tablerNavbar(title = "Dark Navbar Layout"), body = dashboardContent, footer = tablerFooter(left = "\u00A9 2025 Pacha", right = "Built with Tabler - Dark Navbar Layout")),
		"rtl" = tablerPage(title = "RTL Layout - Tabler Dashboard", layout = "rtl", navbar = tablerNavbar(title = "פריסת RTL"), body = dashboardContent, footer = tablerFooter(left = "\u00A9 2024 החברה שלי", right = "נבנה עם Tabler")),
		"fluid" = tablerPage(title = "Fluid Layout - Tabler Dashboard", layout = "fluid", navbar = tablerNavbar(title = "Fluid Layout"), body = dashboardContent, footer = tablerFooter(left = "\u00A9 2025 Pacha", right = "Built with Tabler - Fluid Layout")),
		"fluid-vertical" = tablerPage(title = "Fluid Vertical Layout - Tabler Dashboard", layout = "fluid-vertical", navbar = tablerSidebar(title = "Fluid Vertical Layout", sidebarMenu(menuItem("Palmer penguins", tabName = "penguins", icon = "home"), menuItem("Motor Trend Cars", tabName = "mtcars", icon = "car")), theme = "dark"), body = dashboardContent, footer = tablerFooter(left = "\u00A9 2025 Pacha", right = "Built with Tabler - Fluid Vertical Layout")),
		"horizontal" = tablerPage(title = "Horizontal Layout - Tabler Dashboard", layout = "horizontal", navbar = horizontalMenu(menuItem("Palmer Penguins", tabName = "penguins", icon = "home"), menuItem("Motor Trend Cars", tabName = "mtcars", icon = "car")), body = dashboardContent, footer = tablerFooter(left = "\u00A9 2025 Pacha", right = "Built with Tabler - Horizontal Layout")),
		"navbar-overlap" = tablerPage(title = "Navbar Overlap Layout - Tabler Dashboard", layout = "navbar-overlap", navbar = tablerNavbar(title = "Navbar Overlap Layout"), body = dashboardContent, footer = tablerFooter(left = "\u00A9 2025 Pacha", right = "Built with Tabler - Navbar Overlap Layout")),
		"navbar-sticky" = tablerPage(title = "Navbar Sticky Layout - Tabler Dashboard", layout = "navbar-sticky", navbar = tablerNavbar(title = "Navbar Sticky Layout"), body = dashboardContent, footer = tablerFooter(left = "\u00A9 2025 Pacha", right = "Built with Tabler - Navbar Sticky Layout")),
		"vertical-right" = tablerPage(title = "Vertical Right Layout - Tabler Dashboard", layout = "vertical-right", navbar = tablerSidebar(title = "Vertical Right Layout", sidebarMenu(menuItem("Palmer penguins", tabName = "penguins", icon = "home"), menuItem("Motor Trend Cars", tabName = "mtcars", icon = "car")), theme = "dark"), body = dashboardContent, footer = tablerFooter(left = "\u00A9 2025 Pacha", right = "Built with Tabler - Vertical Right Layout")),
		"vertical-transparent" = tablerPage(title = "Vertical Transparent Layout - Tabler Dashboard", layout = "vertical-transparent", navbar = tablerSidebar(title = "Vertical Transparent Layout", sidebarMenu(menuItem("Palmer penguins", tabName = "penguins", icon = "home"), menuItem("Motor Trend Cars", tabName = "mtcars", icon = "car")), theme = "light"), body = dashboardContent, footer = tablerFooter(left = "\u00A9 2025 Pacha", right = "Built with Tabler - Vertical Transparent Layout")),
		# default
		tablerPage(title = "Tabler Demo", layout = "vertical", navbar = tablerSidebar(title = "Tabler Demo", sidebarMenu(menuItem("Palmer penguins", tabName = "penguins", icon = "home"), menuItem("Motor Trend Cars", tabName = "mtcars", icon = "car")), theme = "dark"), body = dashboardContent, footer = tablerFooter(left = "\u00A9 2025 Pacha", right = "Built with Tabler"))
	)

	tagList(
		# Leave this function for adding external resources
		golem_add_external_resources(),
		# Application UI
		ui
	)
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
	add_resource_path(
		"www",
		app_sys("app/www")
	)

	tags$head(
		favicon(),
		bundle_resources(
			path = app_sys("app/www"),
			app_title = "tablerdemo"
		)
		# Add here other external resources
		# for example, you can add shinyalert::useShinyalert()
	)
}
