## Shared fixtures for layout tests
aside_ul <- sidebar_menu(menu_item("Side", tab_name = "side"))
aside_fixture <- aside(
  class = "navbar navbar-vertical",
  div(
    class = "container-fluid",
    div(class = "collapse navbar-collapse", id = "sidebar-menu", aside_ul)
  )
)

top_fixture <- topbar(title = "TopTest")
brand_nav_fixture <- navbar_menu(menu_item("Home", tab_name = "home"), brand = sidebar_brand(text = "Brand"))
body_fixture <- body("BODY")
