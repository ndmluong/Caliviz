# RAW DATA PRETREATMENT ####
load("data/processed/processed_data_eati_V2.RData")
load("data/processed/processed_data_eat2_V6_EN.RData")
rds_raw <- readRDS(file = "data/raw/FRA_adm1.rds")

# PACKAGES ####
source("R/packages.R")

# # FUNCTIONS ####
# source("R/functions.R")

# Define UI ####
ui_contri <- fluidPage(
  # titlePanel("Données EAT: visualisation"),
  
  navbarPage(
    "Dataset",
    tabPanel("TDS 2",
             fluidRow(
               column(6, selectInput("eat2_single_subs_contri", "Substance",
                                     tapply(subs_tab_eat2_ct$Substance, subs_tab_eat2_ct$Famille, function(x) x),
                                     # unique(df_eat2_ct$Substance),
                                     selected = "Silver",
                                     multiple = FALSE))
             ),
             
             tabsetPanel(
               tabPanel("Pie chart",
                        fluidRow(
                          column(12, plotlyOutput("eat2_contri_pie", height = "800px", width = "900px"))
                        )
               ) # end tabPanel "Pie chart"
               ,
               tabPanel("Bar plot",
                        radioButtons("eat2_contri_bar_hyp", label = "Substition method",
                                     choices = c("Lower-bound" = "LB",
                                                 "Medium-bound" = "MB",
                                                 "Upper-bound" = "UB"),
                                     selected = "LB"),
                        fluidRow(
                          column(12, plotOutput("eat2_contri_bar", height = "800px", width = "1200px"))
                        )
               )
             ), # end tabsetPanel       
             width = 2), # end tabPanel "panel_eat2"
  ) # end navBar
)