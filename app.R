# SHINY COMPONENTS####
## User interface ####
source("R/_ui.R")

## Server ####
source("R/_server.R")

# Run the application 
shinyApp(ui = ui, server = server)
