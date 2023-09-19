# LOADING DATASET (FRENCH VERSION) ####
load("data/processed/processed_data_eat2_V6_FR.RData")

# PACKAGES ####
source("R/packages.R")

# Dictionary data frame ####
## Create table with original french terms ####
# eat2_dico <- data.frame(var_category = c(rep("subs_grp", length(unique(subs_tab_eat2$subs_grp))),
#                                          rep("subs", length(unique(subs_tab_eat2$subs))),
#                                          rep("food_grp", length(unique(food_tab_eat2$food_grp))),
#                                          rep("food", length(unique(food_tab_eat2$food))),
#                                          rep("subs_names", length(unique(df_eat2_ct$Substance)))),
#                         FR = c(unique(subs_tab_eat2$subs_grp),
#                                unique(subs_tab_eat2$subs),
#                                unique(food_tab_eat2$food_grp),
#                                unique(food_tab_eat2$food),
#                                unique(df_eat2_ct$Substance)))
# 
# eat2_dico %>%
#   dplyr::mutate(., EN = NA) -> eat2_dico

## Exporting the list of french terms to Excel ####
# openxlsx::write.xlsx(x = eat2_dico, file = "data/raw/eat2_dico.xlsx", sheetName = "extracted", overwrite = FALSE)


## /!\ Translating french terms in Excel ####
# from the Excel eat_dico.xlsx to eat2_dico_translated.xlsx


## Extracting the list of English terms from Excel ####
eat2_dico <- read_excel(path = "data/raw/eat2_dico_translated.xlsx", sheet = "translated")

## Subsetting dictionnary ####
dico_subsgrp <- subset(eat2_dico, var_category == "subs_grp")
dico_subs <- subset(eat2_dico, var_category == "subs")
dico_foodgrp <- subset(eat2_dico, var_category == "food_grp")
dico_food <- subset(eat2_dico, var_category == "food")
dico_subsnames <- subset(eat2_dico, var_category == "subs_names")


# FR-EN TRANSLATION ####
## >> df_eat2 ####
# translating the names of the list elements (substance families)
for (j in 1:nrow(dico_subsgrp)) {
  names(df_eat2)[names(df_eat2) == dico_subsgrp$FR[j]] <- dico_subsgrp$EN[j]
}

lapply(df_eat2, function(st) { ## for each element (substance family)
  ## translating the names of the substances
  for (j in 1:nrow(dico_subs)) {
    names(st)[names(st) == dico_subs$FR[j]] <- dico_subs$EN[j] 
  }
  
  ## translating the names of the food items
  for (j in 1:nrow(dico_food)) {
    st$`Libellé`[st$`Libellé` == dico_food$FR[j]] <- dico_food$EN[j] 
  }
  
  ## translating the names of the food group
  for (j in 1:nrow(dico_foodgrp)) {
    st$`Groupe de la nomenclature INCA 2`[st$`Groupe de la nomenclature INCA 2` == dico_foodgrp$FR[j]] <- dico_foodgrp$EN[j]
  }

  return(st)
}) -> df_eat2




## >> df_eat2_censored ####
# translating the names of the list elements (substance families)
for (j in 1:nrow(dico_subsgrp)) {
  names(df_eat2_censored)[names(df_eat2_censored) == dico_subsgrp$FR[j]] <- dico_subsgrp$EN[j]
}

lapply(df_eat2_censored, function(st) { ## for each element (substance family)
  ## translating the names of the substance family
  for (j in 1:nrow(dico_subsgrp)) {
    st$`Famille de substances`[st$`Famille de substances` == dico_subsgrp$FR[j]] <- dico_subsgrp$EN[j]
  }
  
  ## translating the names of the substances
  for (j in 1:nrow(dico_subs)) {
    st$`Substance`[st$`Substance` == dico_subs$FR[j]] <- dico_subs$EN[j]
  }
  
  ## translating the names of the food items
  for (j in 1:nrow(dico_food)) {
    st$`Libellé`[st$`Libellé` == dico_food$FR[j]] <- dico_food$EN[j] 
  }
  
  ## translating the names of the food group
  for (j in 1:nrow(dico_foodgrp)) {
    st$`Groupe de la nomenclature INCA 2`[st$`Groupe de la nomenclature INCA 2` == dico_foodgrp$FR[j]] <- dico_foodgrp$EN[j]
  }
  
  return(st)
}) -> df_eat2_censored



## >> df_eat2_ct & df_eat2_ct_expanded ####
# translating the names of the substances
for (j in 1:nrow(dico_subsnames)) {
  df_eat2_ct$Substance[df_eat2_ct$Substance == dico_subsnames$FR[j]] <- dico_subsnames$EN[j]
  df_eat2_ct_expanded$Substance[df_eat2_ct_expanded$Substance == dico_subsnames$FR[j]] <- dico_subsnames$EN[j]
}

# translating the names of the substance families
for (j in 1:nrow(dico_subsgrp)) {
  df_eat2_ct$Famille[df_eat2_ct$Famille == dico_subsgrp$FR[j]] <- dico_subsgrp$EN[j]
  df_eat2_ct_expanded$Famille[df_eat2_ct_expanded$Famille == dico_subsgrp$FR[j]] <- dico_subsgrp$EN[j]
}

# translating the names of the food groups and food items
for (j in 1:nrow(dico_foodgrp)) {
  df_eat2_ct$`Groupe d'aliments`[df_eat2_ct$`Groupe d'aliments` == dico_foodgrp$FR[j]] <- dico_foodgrp$EN[j]
  df_eat2_ct_expanded$`Groupe d'aliments`[df_eat2_ct_expanded$`Groupe d'aliments` == dico_foodgrp$FR[j]] <- dico_foodgrp$EN[j]
}

for (j in 1:nrow(dico_food)) {
  df_eat2_ct$`Groupe d'aliments`[df_eat2_ct$`Groupe d'aliments` == dico_food$FR[j]] <- dico_food$EN[j]
  df_eat2_ct_expanded$`Groupe d'aliments`[df_eat2_ct_expanded$`Groupe d'aliments` == dico_food$FR[j]] <- dico_food$EN[j]
}





## >> df_mat_presence_eat2 ####
# translating the names of the food items
for (j in 1:nrow(dico_food)) {
  df_mat_presence_eat2$`Libellé`[df_mat_presence_eat2$`Libellé` == dico_food$FR[j]] <- dico_food$EN[j]
}

# translating the names of the substances (columns)
for (j in 1:nrow(dico_subs)) {
  names(df_mat_presence_eat2)[names(df_mat_presence_eat2) == dico_subs$FR[j]] <- dico_subs$EN[j]
}



## >> food_tab_eat2 ####
for (j in 1:nrow(dico_food)) {
  food_tab_eat2$food[food_tab_eat2$food == dico_food$FR[j]] <- dico_food$EN[j]
}
for (j in 1:nrow(dico_foodgrp)) {
  food_tab_eat2$food_grp[food_tab_eat2$food_grp == dico_foodgrp$FR[j]] <- dico_foodgrp$EN[j]
}


## >> subs_tab_eat2 ####
for (j in 1:nrow(dico_subs)) {
  subs_tab_eat2$subs[subs_tab_eat2$subs == dico_subs$FR[j]] <- dico_subs$EN[j]
}
for (j in 1:nrow(dico_subsgrp)) {
  subs_tab_eat2$subs_grp[subs_tab_eat2$subs_grp == dico_subsgrp$FR[j]] <- dico_subsgrp$EN[j]
}


## >> subs_tab_eat2_ct ####
for (j in 1:nrow(dico_subsnames)) {
  subs_tab_eat2_ct$Substance[subs_tab_eat2_ct$Substance == dico_subsnames$FR[j]] <- dico_subsnames$EN[j]
}

for (j in 1:nrow(dico_subsgrp)) {
  subs_tab_eat2_ct$Famille[subs_tab_eat2_ct$Famille == dico_subsgrp$FR[j]] <- dico_subsgrp$EN[j]
}




# Clear environment
rm(dico_food, dico_foodgrp, dico_subs, dico_subsgrp, dico_subsnames, eat2_dico)
rm(j)


save.image("data/processed/processed_data_eat2_V6_EN.RData")
