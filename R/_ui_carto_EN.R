# RAW DATA PRETREATMENT ####
load("data/processed/processed_data_eati_V2.RData")
load("data/processed/processed_data_eat2_V6_EN.RData")
rds_raw <- readRDS(file = "data/raw/FRA_adm1.rds")

# PACKAGES ####
source("R/packages.R")

# # FUNCTIONS ####
# source("R/functions.R")

# Define UI ####
ui_carto <- fluidPage(
  # titlePanel("Données EAT: visualisation"),
  
  navbarPage(
    "Dataset",
    tabPanel("TDS 2",
             tabsetPanel(
               tabPanel("Cartography by Food/Substance pairs",
                        fluidRow(
                          column(6, selectInput("eat2_single_food", "Food item",
                                                tapply(food_tab_eat2$food, food_tab_eat2$food_grp, function(x) x),
                                                selected = "veau",
                                                multiple = FALSE)),

                          column(6, selectInput("eat2_single_subs", "Substance",
                                                tapply(subs_tab_eat2$subs, subs_tab_eat2$subs_grp, function(x) x),
                                                selected = "Bpa",
                                                multiple = FALSE))
                        ),
                        fluidRow(
                          column(12, actionButton("btn_eat2_extract_data", "Show",
                                                  icon = icon("map"), class = "btn-success"))
                        ),
                        fluidRow(
                          column(7, dataTableOutput("eat2_extracted_data")),
                          column(5, plotOutput("eat2_carto_conta", height = "720px"))
                        ),
                        fluidRow(
                          em("Region code:
                              (1) Bretagne, Pays de la Loire, Poitou-Charentes;
                              (2) Basse-Normandie, Haute-Normandie, Nord-Pas-de-Calais;
                              (5) Alsace, Champagne-Ardenne, Lorraine;
                              (6) Franche-Comté, Rhône-Alpes;
                              (7) Languedoc-Roussillon, Provence-Alpes-Côte d'Azur;
                              (8) Aquitaine, Midi-Pyrénées;
                              (9) Auvergne, Bourgogne, Centre, Limousin,
                              (34) Ile-de-France
                              (99) National")
                        )
               ) # end tabPanel "Cartographie couples aliments substance"
             ), # end tabsetPanel       
    width = 2), # end tabPanel "panel_eat2"
  ) # end navBar
)
