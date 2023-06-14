# SHINY COMPONENTS####
## User interface ####
source("R/_ui_conta.R")

## Server ####
source("R/_server_conta.R")

# Run the application 
shinyApp(ui = ui_conta, server = server_conta)
