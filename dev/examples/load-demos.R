document()
load_all()

# source("inst/examples/boxed_layout_demo.R")
# source("inst/examples/combo_layout_demo.R")
# source("inst/examples/condensed_layout_demo.R")
# source("inst/examples/fluid_layout_demo.R")
# source("inst/examples/fluid_vertical_layout_demo.R")
source("inst/examples/horizontal_layout_demo.R")

shinyApp(ui = ui, server = server)
