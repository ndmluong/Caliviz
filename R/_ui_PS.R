# RAW DATA PRETREATMENT ####
load("data/processed/processed_data_eati_V2.RData")
load("data/processed/processed_data_eat2_V3.RData")
rds_raw <- readRDS(file = "data/raw/FRA_adm1.rds")

# PACKAGES ####
source("R/packages.R")

# # FUNCTIONS ####
# source("R/functions.R")

# Define UI ####
ui_PS <- fluidPage(
  # titlePanel("Données EAT: visualisation"),
  
  navbarPage(
    "Données",
    tabPanel("EAT2",
             tabsetPanel(
               tabPanel("Présence significative",
                        fluidRow(
                          column(6,selectInput("eat2_multiple_food", "Aliment(s)",
                                               tapply(food_tab_eat2$food, food_tab_eat2$food_grp, function(x) x),
                                               multiple = TRUE)),
                          
                          column(6,
                                 selectInput("eat2_multiple_subs", "Substance(s)",
                                             tapply(subs_tab_eat2$subs, subs_tab_eat2$subs_grp, function(x) x),
                                             multiple = TRUE))
                        ),
                        fluidRow(
                          column(12, actionButton("btn_eat2_plot_check", "Visualiser", icon = icon("eye"), class = "btn-success"))
                        ),
                        fluidRow(
                          plotOutput("eat2_plot_check", height = "600px")
                        ),
                        fluidRow(
                          
                        )
               )
             ), # end tabsetPanel "Presence significative   
    width = 2), # end tabPanel "panel_eat2"
    
    
    tabPanel("EATi",
             tabsetPanel(
               tabPanel("Présence significative",
                        fluidRow(
                          column(6,selectInput("eati_multiple_food", "Choisir aliment(s)",
                                               tapply(food_tab_eati$aliment, food_tab_eati$groupe_aliment, function(x) x),
                                               multiple = TRUE)),
                          
                          column(6,
                                 selectInput("eati_multiple_subs", "Choisir substance(s)",
                                             tapply(subs_tab_eati$substance, subs_tab_eati$famille_substances, function(x) x),
                                             multiple = TRUE))
                        ),
                        fluidRow(
                          column(12, actionButton("btn_eati_plot_check", "Visualiser", icon = icon("eye"), class = "btn-success"))
                        ),
                        fluidRow(
                          plotOutput("eati_plot_check", height = "600px")
                        )
               ),
               
               # tabPanel("Niveau de contamination",
               #          fluidRow(
               #            column(6, selectInput("eati_single_subs", "Choisir une substance",
               #                                  tapply(subs_tab_eati$substance, subs_tab_eati$famille_substances, function(x) x),
               #                                  multiple = FALSE)),
               #            column(6, sliderInput("eati_ndisplay", "Nombre d'aliments (affichage)",
               #                                  min = 20, value = 25, max = 50))
               #          ),
               #          fluidRow(
               #            column(6, actionButton("btn_eati_order", "Visualiser", icon = icon("chart-bar"), class = "btn-primary"))
               #          ),
               #          fluidRow(
               #            column(12, plotOutput("eati_conta_order", height = "720px"))
               #          )
               # ) # end tabPanel "Niveau de contamination EATi"
               
             )
    ) ## end tabPanel "panel_eati
    
  )
  
)
