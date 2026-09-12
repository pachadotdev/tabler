#' @title Run the Dashboard Application
#' @export
run_app <- function() {
  tabler_app(
    ui = app_ui(),
    server = app_server
  )
}
