# Functions EAT2 ####

f_eat2_rename_df <- function(df) {
  colnames(df) <- gsub("   ", " ", colnames(df))
  colnames(df) <- gsub("  ", " ", colnames(df))
  return(df)
}

f_eat2_char2num_col <- function(
  input_string,
  excluded_terms = c("ND|NQ|NR"),
  allowed_chars = c(as.character(0:9), ".")
) {
  # sapply(input_string, function(x) {
  #   if (str_detect(x, excluded_terms) %in% c(NA, TRUE)) { # s'il y a des termes exclus
  #     return(NA)
  #   } else {
  #     return(x)
  #   }
  # })  %>% 
  #   as.vector() -> output_string
  
  sapply(input_string, function(x) {
    if (is.na(x)|(str_detect(x, "NR"))) return(NA)
    else if (str_detect(x, "ND|<")) return("ND")
    else if (str_detect(x, "NQ")) return("NQ")
    else return(x)
  }) %>%
    as.vector() -> output_string
  
  return(output_string)
}

f_eat2_char2num_df <- function(
  input_df,
  vars,
  allowed_chars = c(as.character(0:9), ".")
) {
  
  ddf <- dplyr::select(input_df, !any_of(vars))
  
  apply(ddf, 2, function(x) f_eat2_char2num_col(x)) %>% as.data.frame() -> ddf_out
  
  output <- cbind(dplyr::select(input_df, any_of(vars)),
                  ddf_out)

  return(output)
}



f_eat2_check_by_food <- function(
    d,
    food_list,
    food_table, ## data frame with the all the names of food and corresponding food groups
    subs_table, ## data frame with the all the names of substances and corresponding element of list in d
    vars
) {
  
  lapply(d, function(dst) { ## pour chaque famille de substances

    subs_list <- names(dst)[!names(dst) %in% vars] ## extraire la liste des substances concernees
    print(subs_list)
    # dst_food <- subset(dst, `Libellé` %in% food_list) ## extraire les donnees correspondant aux food input
    mat_out <- matrix(data = NA, nrow = length(food_list), ncol = length(subs_list))

    lapply(food_list, function(fi) { # pour chaque aliment en input
      sapply(subs_list, function(sb) { # pour chaque substance a regarder dans la liste
        if (all(is.na(dst[dst$`Libellé`== fi ,sb]))) { #s'il y a aucune analyse faite (que des NA dans la colonne), retourner NA
          return(NA)
        } else { ## sinon
          
          # ## Règle V1: matrice de presence significative = 1 s'il y a au moins une donnee nationales ou deux donnees regionales quantifiees
          # 
          # dst %>%
          #   dplyr::select(., `Type`, `Libellé`, sb) %>%
          #   filter(., `Libellé` == fi) -> ext_dst ## extraire des donnees correspondant a la substance sb et l'aliment fi
          # 
          # # enlever les lignes avec donnees non quantifiees
          # ext_dst[[sb]] <- suppressWarnings(as.numeric(ext_dst[[sb]])) # conversion string en valeur numerique, si ND/NQ/NR ça devient NA
          # ext_dst <- ext_dst[!is.na(ext_dst[[sb]]), ]
          # 
          # # compter le nombre de donnees quantifiees nationales et regionales
          # ndq_R <- length(ext_dst$Type[ext_dst$Type == "R"])
          # ndq_N <- length(ext_dst$Type[ext_dst$Type == "N"])
          # 
          # if (ndq_N >= 1 || ndq_R >= 2) { # s'il y a au moins une donnees nationales ou deux donnees regionales
          #   return(1) # matrice de presence significative = 1
          # } else {
          #   return(0) # matrice de presence significative = 0
          # }
          
          ## Regle V2: matrice de presence significative = 1 s'il y a au moins une donnees NQ ou chiffree
          dst %>%
            dplyr::select(., `Type`, `Libellé`, sb) %>%
            dplyr::filter(., `Libellé` == fi) -> ext_dst ## extraire des donnees correspondant a la substance sb et l'aliment fi
          
          # compter le nombre de NQ
          ndNQ <- sum(ext_dst[[sb]] == "NQ", na.rm = T)
          
          # compter le nombre de donnees quantifiees
          vec_dq <- suppressWarnings(as.numeric(ext_dst[[sb]])) # conversion string en valeur numerique, si ND/NQ/NR ça devient NA
          vec_dq <- vec_dq[!is.na(vec_dq)] # enlever les NA nouvellement crees
          ndq <- length(vec_dq)
          
          # regle
          if (ndNQ >= 1 || ndq >= 1) { # s'il y a au moins une NQ ou au moins une donnee quantifiee
            return(1) # matrice de presence significative = 1
          } else {
            return(0) # matrice de presence significative = 0
          }
        }
      }) %>%
        as.vector() # end sapply(subs_list)
    }) %>%
      do.call(rbind, .) %>%
      as.matrix() -> mat_out
    
    return(mat_out)
    
  }) -> list_mat_out

  return(list_mat_out)
}


# f_eat2_plot_check_by_food <- function(
#     d,
#     food_input,
#     subsgrp_input,
#     food_table,
#     subs_table,
#     vars,
#     frac_gr = 50
# ) {
#   
#   f_eat2_check_by_food(d = d,
#                          food_input = food_input,
#                          food_table = food_tab,
#                          subs_table = subs_tab,
#                          vars = vars) -> mat_out
#   
#   NCOL <- ncol(mat_out[[subsgrp_input]])
#   
#   
#   if (NCOL > frac_gr) {
#     ngraph <- ceiling(NCOL/frac_gr)
# 
#     par(mfrow = c(ngraph, 1))
# 
#     for (gr in 1:ngraph) {
#       corrplot::corrplot(mat_out[[subsgrp_input]][, seq(from = (gr-1)*frac_gr+1, to = min(gr*frac_gr, NCOL))],
#                          method = "circle", is.corr = F, cl.pos = "n",
#                          tl.col="black", tl.cex=0.8, tl.srt = 45)
#     }
#   } else {
#     par(mfrow = c(1,1))
#     corrplot::corrplot(mat_out[[subsgrp_input]],
#                        method = "circle", is.corr = F, cl.pos = "n",
#                        tl.col="black", tl.cex=0.8, tl.srt = 45)
#   }
# 
#   par(mfrow = c(1,1))
# 
# }










# f_eat2_check_by_subs <- function(
#     d,
#     subs_input,
#     food_table, ## data frame with the all the names of food and corresponding food groups
#     subs_table, ## data frame with the all the names of substances and corresponding element of list in d
#     vars
# ) {
#   
#   tapply(food_table$food, food_table$food_grp, function(x) {
#     mat_food <- matrix(data = NA, nrow = length(x), ncol = length(subs_input))
#     rownames(mat_food) <- x
#     colnames(mat_food) <- subs_input
#     
#     for (fi in x) {
#       for (sb in subs_input) {
#         ## recherche la famille de la substance dans le catalogue des substances
#         sbgr <- subset(subs_table, subs == sb)$subs_grp
#         
#         data_sb_fi <- subset(d[[sbgr]], `Libellé` == fi)[, sb]
#         
#         if (all(is.na(data_sb_fi))) {
#           mat_food[fi, sb] <- 0
#         } else {
#           mat_food[fi, sb] <- 1
#         }
#       }
#     }
#     
#     return(mat_food)
#   }) -> output
#   
#   return(output)
# }



# f_eat2_plot_check_by_subs <- function(
#     d,
#     subs_input,
#     foodgrp_input, ## only for visualisation
#     food_table,
#     subs_table,
#     vars,
#     frac_gr = 50
# ) {
#   
#   f_eat2_check_by_subs(d = d,
#                          subs_input = subs_input,
#                          food_table = food_table,
#                          subs_table = subs_table,
#                          vars = vars) -> mat_out
#   
#   par(mfrow = c(1,1))
#   corrplot::corrplot(mat_out[[foodgrp_input]],
#                      method = "circle", is.corr = T, cl.pos = "n",
#                      tl.col="black", tl.cex=0.8, tl.srt = 45)
#   
# 
# }





f_eat2_plot_check <- function(
  df_mp, # presence matrix (pre-calculated by the function f_eat2_check_by_food)
  food_input,
  subs_input
) {
  
  # recuperer les valeurs correspondant aux aliments selectionnes
  df_mp %>%
    subset(., `Libellé` %in% food_input) -> df_mp_input 
  
  # recuperer les noms dans le bon ordre
  RN <- df_mp_input$`Libellé` 

  # selectionner seulement les substances interessees et convertir en matrice
  df_mp_input %>%
    dplyr::select(., any_of(subs_input)) %>%
    as.matrix() -> mat_presence_input

  # renommer les lignes de la matrice avec les noms dans le bon ordre
  rownames(mat_presence_input) <- RN
  
  # convertir en valeur fictive pour graphique
  mat_presence_input[mat_presence_input == 0] <- 0.25
  mat_presence_input[is.na(mat_presence_input)] <- 0

  corrplot::corrplot(mat_presence_input,
                     method = "circle", is.corr = T, cl.pos = "n",
                     tl.col="black", tl.cex=0.8, tl.srt = 45)
 
  # return(mat_presence_input)
   
}




f_eat2_extract_data <- function(
  d, ## list of data frames
  food_input,
  subs_input,
  food_table, ## data frame with the all the names of food and corresponding food groups
  subs_table, ## data frame with the all the names of substances and corresponding element of list in d
  vars, ## additional variables to extract (ex. food names, food groups names, regions, year...)
  lang = "FR"
) {
  
  subsgrp <- subs_table$subs_grp[subs_table$subs %in% subs_input]
  
  d_subsgrp <- d[[unique(subsgrp)]]
  
  d_output <- d_subsgrp[d_subsgrp$`Libellé` %in% food_input, c(vars, subs_input)]
  
  return(d_output)
}





f_eat2_carto_conta <- function(
    d,
    food_input,
    subs_input,
    food_table,
    subs_table,
    vars,
    frsf,
    rds_raw,
    lang = "FR"
) {
  ## extraction des donnees correspondant au couple selectionne  
  f_eat2_extract_data(d = d,
                      food_input = food_input,
                      subs_input = subs_input,
                      food_table = food_table,
                      subs_table = subs_table,
                      vars = vars) -> ext_data
  
  ## transformer en numérique
  ext_data[[subs_input]] <- suppressWarnings(as.numeric(ext_data[[subs_input]]))
  
  ## enlever les lignes avec NA
  ext_data <- ext_data[!is.na(ext_data[, subs_input]), ]
  
  frsf_out <- cbind(frsf, conta = NA)
  
  if (nrow(ext_data) > 0) { ## s'il y a des donnees dans le tableau
    
    if ("N" %in% ext_data$Type) { ## s'il y a des donnees nationales,
      ## alors, par defaut assigner cette valeur nationale pour toutes les regions (sauf la Corse)
      frsf_out$conta <- mean(subset(ext_data, Type == "N")[, subs_input])
      frsf_out$conta[frsf_out$NAME_1 == "Corse"] <- NA 
    }
    
    if ("R" %in% ext_data$Type) { ## s'il y a des donnees regionales
      ## recuperer les valeurs moyennes par regions
      obs_conta <- tapply(ext_data[[subs_input]], ext_data$`Région`, mean, na.rm = TRUE)
      
      ## assigner region par region
      for (rg in names(obs_conta)) {
        frsf_out$conta[frsf_out$EAT2_region == rg] <- obs_conta[[rg]]
      }
    }
    
    if (length(unique(ext_data$`Région`)) == 1) { ## s'il y a uniquement une region/uniquement nationale
      plot(rds_raw)
      
      if (unique(ext_data$`Région`) == 99) { ## plotter des donnees nationales
        plot(st_geometry(frsf_out[frsf_out$NAME_1 != "Corse",]),
             border = "navyblue",lwd=2,legend=T, add=T)
        
        legendN <- ifelse(lang == "FR",
                          paste("Aliment: ", food_input, " --- ",
                                "Substance: ", subs_input, "\n",
                                "Moyenne nationale: ", mean(subset(ext_data, Type == "N")[, subs_input]), " (", unique(ext_data$`Unité`), ")",
                                sep = ""),
                          paste("Food item: ", food_input, " --- ",
                                "Substance: ", subs_input, "\n",
                                "Average contamination (national): ", mean(subset(ext_data, Type == "N")[, subs_input]), " (", unique(ext_data$`Unité`), ")",
                                sep = ""))
           
        legend("bottomleft", legend = legendN, lwd = 2, col = "navyblue", bty = "n", cex = 0.8)
        
      } else { ## plotter des donnees regionales
        
        region_ID <- reg_code$ID_1[reg_code$EAT2_Region == unique(ext_data$`Région`)]
        plot(st_geometry(frsf_out[frsf_out$ID_1 %in% region_ID,]),
             border = "darkred",lwd = 3, legend=T, add=T)
        
        legendR <- ifelse(lang == "FR",
                          paste("Aliment: ", food_input, " --- ",
                                "Substance: ", subs_input, "\n",
                                "Moyenne régionale: ", mean(subset(ext_data, Type == "R")[, subs_input]), " (", unique(ext_data$`Unité`), ")",
                                sep = ""),
                          paste("Food item: ", food_input, " --- ",
                                "Substance: ", subs_input, "\n",
                                "Average contamination (regional): ", mean(subset(ext_data, Type == "R")[, subs_input]), " (", unique(ext_data$`Unité`), ")",
                                sep = ""))
          
        legend("bottomleft", legend = legendR, lwd = 3, col = "darkred", bty = "n", cex = 0.8)
        
      }
      
    } else {
      choroLayer(frsf_out,
                 var = "conta",
                 method = "quantile",
                 legend.values.rnd = 6,
                 legend.pos = "bottomleft",
                 legend.title.txt = ifelse(lang == "FR",
                                           paste("Aliment: ", food_input, "\n",
                                                 "Substance: ", subs_input, " (", unique(ext_data$`Unité`), ")", sep = ""),
                                           paste("Food item: ", food_input, "\n",
                                                 "Substance: ", subs_input, " (", unique(ext_data$`Unité`), ")", sep = "")),
                 legend.nodata = ifelse(lang == "FR",
                                        "Analyses non réalisées \nSubstances non détectées",
                                        "Analyses not done \nSubstances not detected"),
                 nclass = min(5, length(unique(ext_data$`Région`))))
    }
    
  } else { # sinon, s'il n'y a aucune donnees, plotter la carte vierge
    plot(rds_raw)
  }
  
}






f_eat2_plot_conta_bygrp <- function(
    d, ## list of data frames
    subs_input,
    hyp,
    food_table, ## data frame with the all the names of food and corresponding food groups
    subs_table, ## data frame with the all the names of substances and corresponding element of list in d
    vars,
    lang = "FR"
) {
  
  subsgrp <- subs_table$subs_grp[subs_table$subs %in% subs_input]
  
  d_sub <- d[[unique(subsgrp)]]
  
  d_sub %>%
    dplyr::filter(., Substance == subs_input) %>%
    dplyr::select(., c(vars, "Contamination rapportée", hyp)) -> d_sub
  
  d_sub[[hyp]] <- as.numeric(d_sub[[hyp]])
  
  ## Calcul des moyennes regionales (moyenne des deux vagues) pour l'ensemble des aliments
  d_sub %>%
    group_by(`Groupe de la nomenclature INCA 2`, `Libellé`, `Région`, `Unité`) %>%
    summarise(ContaminationReg = mean(get(hyp), na.rm = TRUE),
              .groups = 'drop') %>%
    group_by(`Groupe de la nomenclature INCA 2`, `Libellé`, `Unité`) %>%
    summarise(ContaminationNatFood = mean(ContaminationReg, na.rm = TRUE),
              .groups = 'drop') %>%
    group_by(`Groupe de la nomenclature INCA 2`, `Unité`) %>%
    summarise(ContaminationNatGrp = mean(ContaminationNatFood, na.rm = TRUE),
              .groups = 'drop') %>%
    arrange(., ContaminationNatGrp) %>%
    rename(., `Groupe d'aliments` = `Groupe de la nomenclature INCA 2`,
           `Contamination moyenne (nationale)` = `ContaminationNatGrp`) -> d_nat_grp
  
  d_nat_grp$`Groupe d'aliments` <- factor(d_nat_grp$`Groupe d'aliments`, levels = unique(d_nat_grp$`Groupe d'aliments`))
  
  d_nat_grp %>%
    mutate(`Contamination moyenne (nationale)` = round(`Contamination moyenne (nationale)`, 4)) -> d_nat_grp
  
  ## translate the variable names (FR -> EN)
  if (lang == "EN") {
    d_nat_grp %>%
      dplyr::rename(`Food group` = `Groupe d'aliments`,
                    `Average contamination (national scale)` = `Contamination moyenne (nationale)`) -> d_nat_grp
  }
  
  if (lang == "FR") {
    g_grp <- ggplot(data = d_nat_grp) +
      geom_col(fill = "#5770BE",
               mapping = aes(x = `Groupe d'aliments`,
                             y = `Contamination moyenne (nationale)`))
  } else {
    g_grp <- ggplot(data = d_nat_grp) +
      geom_col(fill = "#5770BE",
               mapping = aes(x = `Food group`,
                             y = `Average contamination (national scale)`))
  }
  
  g_grp <- g_grp +
    theme(axis.ticks = element_blank(),
          legend.position = "right",
          # axis.text.y = element_blank(),
          axis.text.x = element_text(size=8),
          plot.title = element_text(face="bold", size=16),
          panel.background = element_rect(fill="white"),
          # axis.text = element_text(size=16),
          panel.grid.major.x = element_line(colour = "lightgray"),
          panel.grid.minor.x = element_line(colour = "lightgray")
    ) +
    xlab("") +
    ylab(ifelse(lang == "FR",
                paste("Substance: ", subs_input,
                      "\nContamination (", unique(d_nat_grp$`Unité`), ") - Hypothèse ", hyp, sep = ""),
                paste("Substance: ", subs_input,
                      "\nContamination (", unique(d_nat_grp$`Unité`), ") - Hypothesis ", hyp, sep = ""))) +
    coord_flip()
  
  plotly::ggplotly(g_grp)
  
}


f_eat2_plot_conta_byfood <- function(
    d, ## list of data frames
    subs_input,
    food_grp_input, ## food_grp as input
    hyp,
    food_table, ## data frame with the all the names of food and corresponding food groups
    subs_table, ## data frame with the all the names of substances and corresponding element of list in d
    vars,
    lang = "FR"
) {
  
  subsgrp <- subs_table$subs_grp[subs_table$subs %in% subs_input]
  
  d_sub <- d[[unique(subsgrp)]]
  
  d_sub %>%
    dplyr::filter(., Substance == subs_input) %>%
    dplyr::filter(., `Groupe de la nomenclature INCA 2` == food_grp_input) %>%
    dplyr::select(., c(vars, "Contamination rapportée", hyp)) -> d_sub
  
  d_sub[[hyp]] <- as.numeric(d_sub[[hyp]])
  
  ## Calcul des moyennes regionales (moyenne des deux vagues) pour l'ensemble des aliments
  d_sub %>%
    group_by(`Libellé`, `Région`, `Unité`) %>%
    summarise(ContaminationReg = mean(get(hyp)),
              .groups = 'drop') %>%
    group_by(`Libellé`, `Unité`) %>%
    summarise(ContaminationNatFood = mean(ContaminationReg),
              .groups = 'drop') %>%
    arrange(., ContaminationNatFood) %>%
    rename(., `Aliments` = `Libellé`,
           `Contamination moyenne (nationale)` = `ContaminationNatFood`) -> d_nat_food
  
  d_nat_food$`Aliments` <- factor(d_nat_food$`Aliments`, levels = unique(d_nat_food$`Aliments`))
  
  d_nat_food %>%
    mutate(`Contamination moyenne (nationale)` = round(`Contamination moyenne (nationale)`, 4)) -> d_nat_food
  
  ## translate the variable names (FR -> EN)
  if (lang == "EN") {
    d_nat_food %>%
      dplyr::rename(`Food items` = `Aliments`,
                    `Average contamination (national scale)` = `Contamination moyenne (nationale)`) -> d_nat_food
  }
  
  if (lang == "FR") {
    g_food <- ggplot(data = d_nat_food) +
      geom_col(fill = "#5770BE", alpha = 0.5,
               mapping = aes(x = `Aliments`,
                             y = `Contamination moyenne (nationale)`))
  } else {
    g_food <- ggplot(data = d_nat_food) +
      geom_col(fill = "#5770BE", alpha = 0.5,
               mapping = aes(x = `Food items`,
                             y = `Average contamination (national scale)`))
  }
  
  g_food <- g_food +
    theme(axis.ticks = element_blank(),
          legend.position = "right",
          # axis.text.y = element_blank(),
          axis.text.x = element_text(size=8),
          plot.title = element_text(face="bold", size=16),
          panel.background = element_rect(fill="white"),
          # axis.text = element_text(size=16),
          panel.grid.major.x = element_line(colour = "lightgray"),
          panel.grid.minor.x = element_line(colour = "lightgray")
    ) +
    xlab("") + 
    ylab(ifelse(lang == "FR",
                paste("Substance: ", subs_input,
                      "\nContamination (", unique(d_nat_food$`Unité`), ") - Hypothèse ", hyp, sep = ""),
                paste("Substance: ", subs_input,
                      "\nContamination (", unique(d_nat_food$`Unité`), ") - Hypothesis ", hyp, sep = ""))) +
    coord_flip()
  
  plotly::ggplotly(g_food)
  
}




f_eat2_plot_contri_pie <- function(
  df, ## data frame (contribution 2 populations)
  subs_input, ## string: name of the substance
  lang = "FR"
) {
  
  df_pie <- subset(df, Substance == subs_input)
  
  mypiecols <- viridisLite::turbo(n = 41, alpha = 0.7, begin = 0, end = 0.9, direction = 1)
  set.seed(408)
  mypiecols <- sample(mypiecols)
  
  if (sum(is.na(df_pie$Contribution_MB)) == 0) { ## si c'est le cas de MB uniquement
    plot_output <- plotly::plot_ly() %>%
      add_pie(data = subset(df_pie, Population == "Adultes"),
              labels = ~ `Groupe d'aliments`,
              values = ~ `Contribution_MB`, ## MB
              textposition = ifelse(subset(df_pie, Population == "Adultes")$`Contribution_MB` >= 5, "inside","none"),
              textinfo = 'label',
              hoverinfo = "label+percent",
              hole = 0.3,
              marker = list(colors = mypiecols),
              name = ifelse(lang == "FR", "Adultes - MB", "Adults - MB"), domain = list(y = c(0.5,1)),
              showlegend = F) %>%
      add_pie(data = subset(df_pie, Population == "Enfants"),
              labels = ~ `Groupe d'aliments`,
              values = ~ `Contribution_MB`, ## MB
              textposition = ifelse(subset(df_pie, Population == "Enfants")$`Contribution_MB` >= 5, "inside","none"),
              textinfo = 'label',
              hoverinfo = "label+percent",
              hole = 0.3,
              marker = list(colors = mypiecols),
              name = ifelse(lang == "FR", "Enfants - MB", "Children - MB"), domain = list(y = c(0,0.5)),
              showlegend = F) -> plot_output 
    
      if (lang == "FR") {
        plot_output %>%
          layout(title = list(text = paste("Substance: ", subs_input, sep = ""),
                              font = list(family = "Arial",
                                          size = 24, color = "black")),
                 annotations = list(text = c("Adultes/MB", "Enfants/MB"),
                                    font = list(family = "Arial",
                                                size = 14, color = "black"),
                                    xref = "paper", yref = "paper",
                                    yanchor = "top", xanchor = "center",
                                    align = "center",
                                    x = c(0.5, 0.5),
                                    y = c(0.76, 0.26),
                                    showarrow = F),
                 margin = list(l = 10, r = 10, b = 10, t = 80, pad = 4)) -> plot_output
      } else {
        plot_output %>%
          layout(title = list(text = paste("Substance: ", subs_input, sep = ""),
                              font = list(family = "Arial",
                                          size = 24, color = "black")),
                 annotations = list(text = c("Adults/MB", "Children/MB"),
                                    font = list(family = "Arial",
                                                size = 14, color = "black"),
                                    xref = "paper", yref = "paper",
                                    yanchor = "top", xanchor = "center",
                                    align = "center",
                                    x = c(0.5, 0.5),
                                    y = c(0.76, 0.26),
                                    showarrow = F),
                 margin = list(l = 10, r = 10, b = 10, t = 80, pad = 4)) -> plot_output
      }
    
  } else {
    plotly::plot_ly() %>%
      add_pie(data = subset(df_pie, Population == "Adultes"),
              labels = ~ `Groupe d'aliments`,
              values = ~ `Contribution_LB`,
              textposition = ifelse(subset(df_pie, Population == "Adultes")$`Contribution_LB` >= 5, "inside","none"),
              textinfo = 'label',
              hoverinfo = "label+percent",
              hole = 0.3,
              marker = list(colors = mypiecols),
              name = ifelse(lang == "FR", "Adultes - LB", "Adults - LB"), domain = list(x = c(0,0.5), y = c(0.5,1)),
              showlegend = F) %>%
      add_pie(data = subset(df_pie, Population == "Adultes"),
              labels = ~ `Groupe d'aliments`,
              values = ~ `Contribution_UB`,
              textposition = ifelse(subset(df_pie, Population == "Adultes")$`Contribution_UB` >= 5, "inside","none"),
              textinfo = 'label',
              hoverinfo = "label+percent",
              hole = 0.3,
              marker = list(colors = mypiecols),
              name = ifelse(lang == "FR", "Adultes - UB", "Adults - UB"), domain = list(x = c(0.5, 1), y = c(0.5,1)),
              showlegend = F) %>%
      add_pie(data = subset(df_pie, Population == "Enfants"),
              labels = ~ `Groupe d'aliments`,
              values = ~ `Contribution_LB`,
              textposition = ifelse(subset(df_pie, Population == "Enfants")$`Contribution_LB` >= 5, "inside","none"),
              textinfo = 'label',
              hoverinfo = "label+percent",
              hole = 0.3,
              marker = list(colors = mypiecols),
              name = ifelse(lang == "FR", "Enfants - LB", "Children - LB"), domain = list(x = c(0, 0.5), y = c(0,0.5)),
              showlegend = F) %>%
      add_pie(data = subset(df_pie, Population == "Enfants"),
              labels = ~ `Groupe d'aliments`,
              values = ~ `Contribution_UB`,
              textposition = ifelse(subset(df_pie, Population == "Enfants")$`Contribution_UB` >= 5, "inside","none"),
              textinfo = 'label',
              hoverinfo = "label+percent",
              hole = 0.3,
              marker = list(colors = mypiecols),
              name = ifelse(lang == "FR", "Enfants - UB", "Children - UB"), domain = list(x = c(0.5, 1), y = c(0,0.5)),
              showlegend = F) -> plot_output
    
    if (lang == "FR") {
      plot_output %>%
        layout(title = list(text = paste("Substance: ", subs_input, sep = ""),
                            font = list(family = "Arial",
                                        size = 24, color = "black")),
               annotations = list(text = c("Adultes/LB","Adultes/UB", "Enfants/LB", "Enfants/UB"),
                                  font = list(family = "Arial",
                                              size = 14, color = "black"),
                                  xref = "paper", yref = "paper",
                                  yanchor = "top", xanchor = "center",
                                  align = "center",
                                  x = c(.25, .75, .25, .75),
                                  y = c(.76, .76, .26, .26),
                                  showarrow = F),
               margin = list(l = 10, r = 10, b = 10, t = 80, pad = 4)) -> plot_output
    } else {
      plot_output %>%
        layout(title = list(text = paste("Substance: ", subs_input, sep = ""),
                            font = list(family = "Arial",
                                        size = 24, color = "black")),
               annotations = list(text = c("Adults/LB","Adults/UB", "Children/LB", "Children/UB"),
                                  font = list(family = "Arial",
                                              size = 14, color = "black"),
                                  xref = "paper", yref = "paper",
                                  yanchor = "top", xanchor = "center",
                                  align = "center",
                                  x = c(.25, .75, .25, .75),
                                  y = c(.76, .76, .26, .26),
                                  showarrow = F),
               margin = list(l = 10, r = 10, b = 10, t = 80, pad = 4)) -> plot_output
    }
      
  }
  
  return(plot_output)
  
}













f_eat2_plot_contri_bar <- function(
    df, ## data frame (contribution 2 populations with the column "Hypothesis" (expanded table))
    subs_input,
    hyp_input,
    lang = "FR"
) {
  
  ## Adultes 
  df %>%
    dplyr::filter(., Substance == subs_input, Hypothesis == hyp_input, Population == "Adultes") %>%
    dplyr::arrange(., Contribution) -> subdf_A
  
  subdf_A$`Groupe d'aliments` <- factor(subdf_A$`Groupe d'aliments`, levels = unique(subdf_A$`Groupe d'aliments`))
  
  if (sum(is.na(subdf_A$`Contribution`)) == 0) {
    g_A <- ggplot(data = subdf_A,
                  mapping = aes(x = `Groupe d'aliments`, y = `Contribution`)) +
      geom_col(fill = "#FF9940", alpha = 0.7) +
      geom_text(aes(label = paste(`Groupe d'aliments`, " (", round(`Contribution`, 2), "%)", sep = "")),
                y = 0, size = 3, hjust = 0) +
      xlab("") + ylab("") + 
      labs(title = ifelse(lang == "FR",
                          "Population: Adultes",
                          "Population: Adults")) +
      theme(axis.ticks = element_blank(),
            legend.position = "right",
            axis.text.y = element_blank(),
            axis.text.x = element_text(size=8),
            plot.title = element_text(face="bold", size=12),
            panel.background = element_rect(fill="white"),
            axis.text = element_text(size=16),
            panel.grid.major.x = element_line(colour = "lightgray"),
            panel.grid.minor.x = element_line(colour = "lightgray")
      ) +
      coord_flip()
  }
  
  ## Enfants 
  df %>%
    dplyr::filter(., Substance == subs_input, Hypothesis == hyp_input, Population == "Enfants") %>%
    dplyr::arrange(., Contribution) -> subdf_E
  
  subdf_E$`Groupe d'aliments` <- factor(subdf_E$`Groupe d'aliments`, levels = unique(subdf_E$`Groupe d'aliments`))
  
  if (sum(is.na(subdf_E$`Contribution`)) == 0) {
    g_E <- ggplot(data = subdf_E,
                  mapping = aes(x = `Groupe d'aliments`, y = `Contribution`)) +
      geom_col(fill = "#00AC8C", alpha = 0.7) +
      geom_text(aes(label = paste(`Groupe d'aliments`, " (", round(`Contribution`, 2), "%)", sep = "")),
                y = 0, size = 3, hjust = 0) +
      xlab("") + ylab("") + 
      labs(title = ifelse(lang == "FR",
                          "Population: Enfants",
                          "Population: Children")) +
      theme(axis.ticks = element_blank(),
            legend.position = "right",
            axis.text.y = element_blank(),
            axis.text.x = element_text(size=8),
            plot.title = element_text(face="bold", size=12),
            panel.background = element_rect(fill="white"),
            axis.text = element_text(size=16),
            panel.grid.major.x = element_line(colour = "lightgray"),
            panel.grid.minor.x = element_line(colour = "lightgray")
      ) +
      coord_flip()
  }
  
  if (sum(is.na(subdf_E$`Contribution`)) > 0) {
    ggplot() +
      theme(axis.title = element_blank(),
            panel.background = element_rect(fill="white")) +
      labs(subtitle = ifelse(lang == "FR",
                             ">>> Les contributions relatives n'ont pas été estimées pour l'hypothèse sélectionnée",
                             ">>> Relative contributions were not estimated with the selected hypothesis in this study"))
  } else {
    gridExtra::grid.arrange(g_A, g_E, nrow = 1,
                            top = ifelse(lang == "FR",
                                         paste("Substance: ", subs_input, " - Hypothèse: ", hyp_input, sep =""),
                                         paste("Substance: ", subs_input, " - Hypothesis: ", hyp_input, sep ="")),
                            left = ifelse(lang == "FR", "Groupe d'aliments", "Food groups"),
                            bottom = "Contribution (%)")
  }
  
}










f_eat2_identify_hazards <- function(
    d,
    fi, ## food_input
    food_table,
    subs_table,
    vars,
    lang = "FR"
) {
  lapply(d, function(dst) { ## pour chaque famille de substances
    
    subs_list <- names(dst)[!names(dst) %in% vars] ## extraire la liste des substances concernees
    
    sapply(subs_list, function(sb) {
      if (all(is.na(dst[dst$`Libellé` == fi ,sb]))) {
        return(NA)
      } else {
        ## Regle V2: matrice de presence significative = 1 s'il y a au moins une donnees NQ ou chiffree
        dst %>%
          dplyr::select(., `Type`, `Libellé`, sb) %>%
          dplyr::filter(., `Libellé` == fi) -> ext_dst ## extraire des donnees correspondant a la substance sb et l'aliment fi
        
        # compter le nombre de NQ
        ndNQ <- sum(ext_dst[[sb]] == "NQ", na.rm = T)
        
        # compter le nombre de donnees quantifiees
        vec_dq <- suppressWarnings(as.numeric(ext_dst[[sb]])) # conversion string en valeur numerique, si ND/NQ/NR ça devient NA
        vec_dq <- vec_dq[!is.na(vec_dq)] # enlever les NA nouvellement crees
        ndq <- length(vec_dq)
        
        # regle
        if (ndNQ >= 1 || ndq >= 1) { # s'il y a au moins une NQ ou au moins une donnee quantifiee
          return(sb) # matrice de presence significative = 1
        } else {
          return(NA)}
      }
    }) -> subs_vec
    
    subs_vec <- unname(subs_vec[!is.na(subs_vec)])
    
    subs_vec <- paste("(",length(subs_vec),")  ",
                      stringr::str_c(subs_vec, collapse = ", "),
                      sep = "")
    
    return(subs_vec)
    
  }) %>%
    as.data.frame() -> hazards_output
  
  names(hazards_output) <- names(df_eat2)
  hazards_output <- t(hazards_output)
  
  hazards_output <- data.frame(rownames(hazards_output),
                               hazards_output)
  
  if (lang == "FR") {
    colnames(hazards_output) <- c("Famille de substances", "Dangers identifiés")
  } else {
    colnames(hazards_output) <- c("Substances family", "Identified hazards")
  }
  
  rownames(hazards_output) <- 1:nrow(hazards_output)
  
  return(hazards_output)
  
}

























