# RAW DATA PRETREATMENT ####
load("data/processed/processed_data_eat2_V6_EN.RData")
load("data/processed/processed_data_eati_V2.RData")
rds_raw <- readRDS(file = "data/raw/FRA_adm1.rds")

# PACKAGES ####
source("R/packages.R")

# # FUNCTIONS ####
# source("R/functions.R")

# Define UI ####
ui_conta <- fluidPage(
  # titlePanel("Données EAT: Niveau de contamination"),
  
  navbarPage(
    "Dataset",
    tabPanel("TDS 2",
             # fluidRow(
             #   em("- Pour visualiser la contamination moyenne pour les différents groupes d'aliments: sélectionner une hypothèse de censure à considérer; sélectionner une subtance dans la liste (sélectionner avec le menu déroulant ou entrer le nom de la substance); puis cliquer sur 'Contamination des différents groupes'."),
             #   p("\n"),
             #   em('- Pour visualiser la contamination moyenne pour les différents aliments dans un groupe spécifique: sélectionner celui-ci dans la liste proposée (sélectionner avec le menu déroulant ou entrer le nom de la substance); puis cliquer sur "Contamination des différents aliments du groupe".'),
             #   p("\n")
             # ),
             fluidRow(
               column(12, radioButtons("eat2_conta_hyp", label = "Substitution methods",
                                       choices = c("Lower-bound" = "LB",
                                                   "Medium-bound" = "MB",
                                                   "Upper-bound (UB)" = "UB"),
                                       selected = "LB"))
             ),
             
             fluidRow(
               column(6, selectInput("eat2_single_subs_hyp", "Substance",
                                     tapply(subs_tab_eat2$subs, subs_tab_eat2$subs_grp, function(x) x),
                                     selected = "Cd",
                                     multiple = FALSE)),
               
               column(6, selectInput("eat2_single_food_grp", "Food groups",
                                     unique(food_tab_eat2$food_grp),
                                     selected = "Meat",
                                     multiple = FALSE)),
             ),
             
             fluidRow(
               column(7, actionButton("btn_eat2_show_conta_allgrp", "Contamination of the different food groups", icon = icon("chart-simple"), class = "btn-success")),
               column(5, actionButton("btn_eat2_show_conta_onegrp", "Contamination of the different food items", icon = icon("chart-simple"), class = "btn-success")),
             ),
             
             fluidRow(
               column(7, plotlyOutput("eat2_show_conta_bygrp", height = "550px")),
               column(5, plotlyOutput("eat2_show_conta_byfood", height = "200px"))
             ),
             
             fluidRow(
               p("\n"),
               em("NB: The contamination data for Mycotoxins et Phytoestrogens (LB/MB/UB) will be integrated in the upcoming weeks")
             ),
             
             width = 2), # end tabPanel "panel_eat2"
    
    
    # tabPanel("EATi",
    #          tabsetPanel(
    #            tabPanel("Contamination level",
    #                     fluidRow(
    #                       column(6, selectInput("eati_single_subs", "Substance",
    #                                             tapply(subs_tab_eati$substance, subs_tab_eati$famille_substances, function(x) x),
    #                                             multiple = FALSE)),
    #                       column(6, sliderInput("eati_ndisplay", "Nombre d'aliments (affichage)",
    #                                             min = 20, value = 25, max = 50))
    #                     ),
    #                     fluidRow(
    #                       column(6, actionButton("btn_eati_order", "Visualiser", icon = icon("chart-bar"), class = "btn-primary"))
    #                     ),
    #                     fluidRow(
    #                       column(12, plotOutput("eati_conta_order", height = "720px"))
    #                     )
    #            ) # end tabPanel "Niveau de contamination EATi"
    #          )
    # ) ## end tabPanel "panel_eati

  )
  
)
