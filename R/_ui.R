# RAW DATA PRETREATMENT ####
load("data/processed/processed_data_eat2.RData")
load("data/processed/processed_data_eati.RData")

# PACKAGES ####
source("R/packages.R")

# # FUNCTIONS ####
# source("R/functions.R")

# Define UI ####
ui <- fluidPage(
  titlePanel("Données EAT: visualisation"),
  
  navbarPage(
    "Jeu de données",
    tabPanel("EAT2 (data.gouv.fr)",
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
                          plotOutput("eat2_plot_check", height = "1080px")
                        )
               ),
               
               tabPanel("Données - Couple aliment/substance",
                        fluidRow(
                          column(6, selectInput("eat2_single_food", "Choisir un aliment",
                                                tapply(food_tab_eat2$food, food_tab_eat2$food_grp, function(x) x),
                                                multiple = FALSE)),
                          
                          column(6, selectInput("eat2_single_subs", "Choisir une substance",
                                                tapply(subs_tab_eat2$subs, subs_tab_eat2$subs_grp, function(x) x),
                                                multiple = FALSE))
                        ),
                        fluidRow(
                          column(12, actionButton("btn_eat2_extract_data", "Extraire", icon = icon("file"), class = "btn-success"))
                        ),
                        fluidRow(
                          column(6, dataTableOutput("eat2_extracted_data")),
                          column(6, plotOutput("eat2_carto_conta", height = "720px"))
                        )
               ) # end tabPanel "Donnees couples aliments"
             ), # end tabsetPanel (3 onglets de recherche eat2)       
    width = 2), # end tabPanel "panel_eat2"
    
    
    tabPanel("EATi (data.gouv.fr)",
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
                          plotOutput("eati_plot_check", height = "1080px")
                        )
               ),
               
               tabPanel("Niveau de contamination",
                        fluidRow(
                          column(6, selectInput("eati_single_subs", "Choisir une substance",
                                                tapply(subs_tab_eati$substance, subs_tab_eati$famille_substances, function(x) x),
                                                multiple = FALSE))
                        ),
                        fluidRow(
                          column(6, actionButton("btn_eati_order", "Visualiser", icon = icon("chart-bar"), class = "btn-primary"))
                        ),
                        fluidRow(
                          column(12, plotOutput("eati_conta_order", height = "720px"))
                        )
               ) # end tabPanel "Niveau de contamination EATi"
               
             )
    ) ## end tabPanel "panel_eati
    
  )
  
  
  

)
