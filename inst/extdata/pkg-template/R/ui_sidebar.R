#' @title Create a Side Bar (UI)
#' @noRd
sidebar_nav <- navbar_menu(
  brand = sidebar_brand(text = "", img = svg_data_uri, href = "./"),
  menu_item("Mtcars", tab_name = "mtcars", icon = "car"),
  menu_item("Iris", tab_name = "iris", icon = "flower"),
  menu_item("Airquality", tab_name = "airquality", icon = "cloud")
)
