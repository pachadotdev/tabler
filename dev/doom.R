library(tabler)
library(httpserver)

doom_directory <- "./doom-wasm"

startServer("127.0.0.1", 1234, list(
  call = function(req) {
    file_path <- paste0(doom_directory, req$PATH_INFO)
    if (file.exists(file_path)) {
      return(list(
        status = 200,
        headers = list(
          "Content-Type" = "text/html"
        ),
        body = readBin(file_path, "raw", file.info(file_path)$size)
      ))
    } else {
      return(list(
        status = 404,
        headers = list(
          "Content-Type" = "text/html"
        ),
        body = "File not found"
      ))
    }
  }
))

ui <- tags$div(
  style = "text-align: center",
  tags$p("Space: fire; E: open door; up/down/left/right: move; 1-4: change weapon; Esc: release mouse/go to menu; F: full screen"),
  tags$iframe(style = "width:850px; height:640px; display:block; margin: 0 auto;", src = "http://127.0.0.1:1234/index.html")
)

server <- function(input, output, session) { }

# Run the Tabler app
tabler_app(ui, server, port = 4321)
