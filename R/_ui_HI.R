# RAW DATA PRETREATMENT ####
load("data/processed/processed_data_eati_V2.RData")
load("data/processed/processed_data_eat2_V6_FR.RData")
rds_raw <- readRDS(file = "data/raw/FRA_adm1.rds")

# PACKAGES ####
source("R/packages.R")

# # FUNCTIONS ####
# source("R/functions.R")

# Define UI ####
ui_HI <- fluidPage(
  # titlePanel("Données EAT: visualisation"),
  
  navbarPage(
    "Données",
    tabPanel("EAT2",
             tabsetPanel(
               tabPanel("Identification des dangers",
                        fluidRow(
                          column(6,selectInput("eat2_food_4_identify_hazards", "Aliment",
                                               tapply(food_tab_eat2$food, food_tab_eat2$food_grp, function(x) x),
                                               multiple = FALSE))
                        ),
                        fluidRow(
                          column(6, actionButton("btn_eat2_identify_hazards", ">>> Liste des dangers", icon = icon("eye"), class = "btn-success"))
                        ),
                        fluidRow(
                          column(12, dataTableOutput("eat2_identify_hazards"))
                        )
               )
             ), # end tabsetPanel "Presence significative   
             width = 2) # end tabPanel "panel_eat2"
    
  )
  
)
