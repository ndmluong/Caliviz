# RAW DATA PRETREATMENT ####
load("data/processed/processed_data_eati_V2.RData")
load("data/processed/processed_data_eat2_V4.RData")
rds_raw <- readRDS(file = "data/raw/FRA_adm1.rds")

# PACKAGES ####
source("R/packages.R")

# # FUNCTIONS ####
# source("R/functions.R")

# Define UI ####
ui_contri <- fluidPage(
  # titlePanel("Données EAT: visualisation"),
  
  navbarPage(
    "Données",
    tabPanel("EAT2",
             tabsetPanel(
               # tabPanel("Aliments contributeurs",
                        fluidRow(
                          column(6, selectInput("eat2_single_subs_contri", "Choisir une substance",
                                                # tapply(subs_tab_eat2$subs, subs_tab_eat2$subs_grp, function(x) x),
                                                unique(df_eat2_ct$Substance),
                                                selected = "Argent",
                                                multiple = FALSE))
                        ),
                        fluidRow(
                          column(12, plotlyOutput("eat2_contri", height = "800px", width = "900px"))
                        )
               # ) # end tabPanel "Aliments contributeurs"
             ), # end tabsetPanel       
    width = 2), # end tabPanel "panel_eat2"
  ) # end navBar
)
