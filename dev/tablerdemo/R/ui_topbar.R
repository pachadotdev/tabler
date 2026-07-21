#' @title Create a Top Bar (UI)
#' @noRd
topbar_nav <- navbar_menu(
  menu_item("Tabler docs", icon = NULL, href = "https://tabler.io/admin-template/preview"),
  menu_item("Tabler R docs", icon = NULL, href = "https://pacha.dev/tabler"),
  menu_item("Tabler Server docs", icon = NULL, href = "https://pacha.dev/tabler-server"),
  menu_dropdown(
    "Another button",
    icon = "layout-2",
    href = "./",
    items = list(
      c("Placeholder", "./")
    )
  )
)
