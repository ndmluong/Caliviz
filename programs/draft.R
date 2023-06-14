

# Palette de couleurs
colfunc<-colorRampPalette(c("white","red"))
colors <- (colfunc(5))

## selection du couple aliment/substance
eat2_single_food <- "veau"
eat2_single_subs <- "Cd"

eat2_single_food <- "soupe poulet vermicelle" ## uniquement regionale et 1 seule region
eat2_single_subs <- "Cd"

eat2_single_food <- "croissant" ## uniquement nationale
eat2_single_subs <- "Cd"

eat2_single_food <- sample(food_tab_eat2$food, 1)
eat2_single_subs <- sample(subs_tab_eat2$subs, 1)


f_eat2_carto_conta(d = df_eat2,
                   food_input = eat2_single_food,
                   subs_input = eat2_single_subs,
                   food_table = food_tab_eat2,
                   subs_table = subs_tab_eat2,
                   vars = com_var_eat2,
                   frsf = sf_FRA1)



f_eat2_plot_check(df_mp = df_mat_presence,
                  food_input = c("veau", "croissant", "soupe poulet vermicelle", "cidre"),
                  subs_input = c("Cu", "Cd", "Li", "PCB_126", "PCB_169", "Sn", "Sb", "Te")) 






# Affichage





plot(readRDS("data/raw/FRA_adm1.rds"))


# 
# sapply(reg_conta, function(conta) {
#   frsf$conta[EAT_conta == names(conta)]
# })
# 
# frsf2 <- cbind(frsf, conta = NA)
# 
# frsf2$conta[frsf2$EAT2_region %in% names(reg_conta)]
# 
# frsf2[frsf2$EAT2_region %in% names(reg_conta), ]



unique(ext_data$Unité)










# draft classement eati
subs_input <- "Cd"

f_eati_order(df = df_eati, subs_input = "Cd", ndisplay = 25)


f_eati_order(df = df_eati, subs_input = "E262")







dc <- df_eat2_censored$`Contaminants inorg et minéraux`


f_eat2_plot_conta_bygrp(d = df_eat2_censored,
                        subs_input = "Cd",
                        hyp = "LB",
                        food_table = food_tab_eat2,
                        subs_table = subs_tab_eat2,
                        vars = com_var_eat2)

f_eat2_plot_conta_byfood(d = df_eat2_censored,
                         subs_input = "Cd",
                         food_grp_input = "Viande",
                         hyp = "UB",
                         food_table = food_tab_eat2,
                         subs_table = subs_tab_eat2,
                         vars = com_var_eat2)








