# SHINY COMPONENTS####
## User interface ####
source("R/_ui_contri.R")

## Server ####
source("R/_server_contri.R")

# Run the application 
shinyApp(ui = ui_contri, server = server_contri)
