# PACKAGES ####
source("R/packages.R")

# FUNCTIONS ####
source("R/functions_eati.R")

# Import the raw data ####
packaging_tab_eati <- read_excel(path = "data/raw/donnees-eati.xlsx", sheet = "Liste emballages (packaging)")
foodgrp_tab_eati <- read_excel(path = "data/raw/donnees-eati.xlsx", sheet = "Liste groupes aliments")
subsgrp_tab_eati <- read_excel(path = "data/raw/donnees-eati.xlsx", sheet = "Liste familles substances")

## Results pesticides and other ####
df_eati_P_raw <- read_excel(path = "data/raw/donnees-eati.xlsx", sheet = "Résultats résidus de pesticides")
df_eati_nP_raw <- read_excel(path = "data/raw/donnees-eati.xlsx", sheet = "Résultats hors pesticides")




## Terms harmonization ####
### Results Pesticide residues ####
df_eati_P_raw %>%
  dplyr::mutate(groupe_aliment = str_to_sentence(groupe_aliment),
                aliment = str_to_sentence(aliment)) %>%
  dplyr::mutate(aliment = str_replace(aliment, "œ", replacement = "oe")) %>%
  dplyr::mutate(famille_substances = "Résidus de pesticides", .after = substance) %>%
  dplyr::mutate(substance_group = "Pesticide residues", .after = famille_substances) %>%
  dplyr::mutate(lor = NA, .after = lq) -> df_eati_P


### Other substances (excepting Pesticide residues) ####
df_eati_nP <- df_eati_nP_raw
df_eati_nP$aliment[df_eati_nP$aliment == "Poires"] <- "Poire"
df_eati_nP %>%
  dplyr::mutate(groupe_aliment = str_to_sentence(groupe_aliment),
                aliment = str_to_sentence(aliment)) %>%
  dplyr::mutate(aliment = str_replace(aliment, "œ", replacement = "oe")) %>%
  dplyr::mutate(., Substance_prioritaire = NA, .after = substance_group) -> df_eati_nP





## Full list of food products combined from both categories ####
### Results Pesticide residues ####
df_eati_P %>%
  dplyr::select(., code, groupe_aliment, aliment, type_alimentation) %>%
  dplyr::distinct(.) -> food_tab_eati_P

df_eati_nP %>%
  dplyr::select(., code, groupe_aliment, aliment, type_alimentation) %>%
  dplyr::distinct(.) -> food_tab_eati_nP

rbind(food_tab_eati_P, food_tab_eati_nP) %>%
  dplyr::distinct(.) %>%
  dplyr::arrange(., groupe_aliment, aliment) -> food_tab_eati

rm(food_tab_eati_P, food_tab_eati_nP)




## Full list of food products combined from both categories ####
df_eati_nP %>%
  dplyr::select(., substance_group, substance) %>%
  dplyr::distinct(.) %>%
  dplyr::arrange(., substance_group, substance) %>%
  dplyr::mutate(., Substance_prioritaire = NA) -> subs_nP_tab_eati

df_eati_P %>%
  dplyr::select(., substance, Substance_prioritaire) %>%
  dplyr::distinct(.) %>%
  dplyr::mutate(., substance_group = "Pesticide residues") %>%
  dplyr::arrange(., desc(Substance_prioritaire)) -> subs_P_tab_eati
subs_P_tab_eati$Substance_prioritaire[subs_P_tab_eati$Substance_prioritaire == "OUI"] <- "Prioritaire"
subs_P_tab_eati$Substance_prioritaire[subs_P_tab_eati$Substance_prioritaire == "NON"] <- "Non prioritaire"

rbind(subs_nP_tab_eati, subs_P_tab_eati) %>%
  mutate(., famille_substances = NA, .before = substance) -> subs_tab_eati
rm(subs_nP_tab_eati, subs_P_tab_eati)

for (gr in subsgrp_tab_eati$substance_group) {
  subs_tab_eati$famille_substances[subs_tab_eati$substance_group == gr] <- subsgrp_tab_eati$famille_substances[subsgrp_tab_eati$substance_group == gr]
}
rm(gr, subsgrp_tab_eati)

## Combine the two categories
rbind(df_eati_nP, df_eati_P) -> df_eati

rm(df_eati_nP, df_eati_P)
rm(df_eati_P_raw, df_eati_nP_raw)


lapply(food_tab_eati$aliment, function(fi) {
  sapply(subs_tab_eati$substance, function(sb) {
    res <- subset(df_eati, aliment == fi & substance == sb)$type_res
    if (length(res) < 1) return(NA) # s'il y a aucune valeur : NA
    else if (length(res) == 1) { # s'il y a une seule valeur : 
      if (res == "Non déterminé") return(NA) 
      # else if (res == "VAL") return(1)
      else if (res %in% c("VAL", "<LQ")) return(1)
      else return(0)
    }
    else { # s'il y a plusieur valeurs
      # if ("VAL" %in% res) return(1)
      if ("VAL" %in% res || "<LQ" %in% res) return(1)
      else return(0)
      }
  }) %>%
    as.vector()
}) %>%
  do.call(rbind, .) %>%
  as.matrix() %>%
  as.data.frame() -> df_mat_presence_eati

colnames(df_mat_presence_eati) <- subs_tab_eati$substance

df_mat_presence_eati <- data.frame(`Libellé` = food_tab_eati$aliment,
                                   df_mat_presence_eati)



# f_eat2_plot_check(df_mp = df_mat_presence_eati,
#                   food_input = c("Beurre - papier", "Beurre doux", "Biscuit sec nature", "Biscuit sec au chocolat"),
#                   subs_input = c("E262", "E304", "HCDD_123678"))

df_eati$concentration <- as.numeric(df_eati$concentration)


# SAVE IMAGE ####
save.image("data/processed/processed_data_eati_V2.RData")






