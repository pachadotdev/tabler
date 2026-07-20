#' @title Run the Dashboard Application
#' @export
run_app <- function() {
  tablerApp(
    ui = app_ui(),
    server = app_server
  )
}
