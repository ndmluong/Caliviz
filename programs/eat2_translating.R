# LOADING DATASET (FRENCH VERSION) ####
load("data/processed/processed_data_eat2_V5.RData")

# PACKAGES ####
source("R/packages.R")

gdata::keep(subs_tab_eat2, food_tab_eat2,
            sure = TRUE)

# Dictionary data frame ####
## Create table with original french terms
eat2_dico <- data.frame(var_category = c(rep("subs_grp", length(unique(subs_tab_eat2$subs_grp))),
                                         rep("subs", length(unique(subs_tab_eat2$subs))),
                                         rep("food_grp", length(unique(food_tab_eat2$food_grp))),
                                         rep("food", length(unique(food_tab_eat2$food)))),
                        FR = c(unique(subs_tab_eat2$subs_grp),
                               unique(subs_tab_eat2$subs),
                               unique(food_tab_eat2$food_grp),
                               unique(food_tab_eat2$food)))

eat2_dico %>%
  dplyr::mutate(., EN = NA) -> eat2_dico

openxlsx::write.xlsx(x = eat2_dico, file = "data/raw/eat2_dico.xlsx", sheetName = "extracted", overwrite = FALSE)



openxlsx::write.xlsx(x = arrange(food_tab_eat2, food_grp, food), file = "data/raw/food_tabs_eat2.xlsx", sheetName = "raw", overwrite = FALSE)

## /!\ translating terms in Excel ####

## 
