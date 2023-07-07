f_eati_order <- function(
    df,
    subs_input,
    ndisplay = 25,
    type_separation = TRUE
) {
  
  df %>%
    dplyr::filter(., type_res == "VAL") %>%
    dplyr::filter(., substance == subs_input) %>%
    dplyr::arrange(., concentration) -> subdf
  
  subdf$aliment <- factor(subdf$aliment, levels = unique(subdf$aliment))
  
  ## Subset: aliments courants
  subdf %>%
    dplyr::filter(., type_alimentation == "Courant") %>%
    dplyr::arrange(., concentration) -> subdf_common
  
  subdf_common %>%
    dplyr::slice_tail(., n = min(ndisplay, nrow(subdf_common))) -> subdf_common
  
  ## Subset: aliments infantiles
  subdf %>%
    dplyr::filter(., type_alimentation == "Infantile") %>%
    dplyr::arrange(., concentration) -> subdf_infantile
  
  subdf_infantile %>%
    dplyr::slice_tail(., n = min(ndisplay, nrow(subdf_infantile))) -> subdf_infantile
  
  
  
  ## Plots
  ## Aliments courants 
  g1 <- ggplot(data = subdf_common,
               mapping = aes(x = aliment, y = concentration)) +
    geom_col(fill = "#FF9940", alpha = 0.5) +
    geom_text(aes(label = paste(aliment, " (", round(concentration, 3), ")", sep = "")),
              y = 0, size = 3, hjust = 0) +
    xlab("Aliments") + ylab(paste("Concentration (", unique(subdf_common$unite), ")", sep = "")) +
    labs(title = ">>> Aliments courants") +
    theme(axis.ticks = element_blank(),
          legend.position = "right",
          axis.text.y = element_blank(),
          axis.text.x = element_text(size=8),
          plot.title = element_text(face="bold", size=16),
          panel.background = element_rect(fill="white"),
          axis.text = element_text(size=16),
          panel.grid.major.x = element_line(colour = "lightgray"),
          panel.grid.minor.x = element_line(colour = "lightgray")
    ) +
    coord_flip()
  if (nrow(subdf_common) == 0) {
    g1 <- g1 +
      theme(axis.title = element_blank()) +
      labs(subtitle = ">>>>> Pas de données quantifiées")
  }
  
  
  ## Aliments infantiles
  g2 <- ggplot(data = subdf_infantile,
               mapping = aes(x = aliment, y = concentration)) +
    geom_col(fill = "#00AC8C", alpha = 0.5) +
    geom_text(aes(label = paste(aliment, " (", round(concentration, 3), ")", sep = "")),
              y = 0, size = 3, hjust = 0) +
    xlab("Aliments") + ylab(paste("Concentration (", unique(subdf_common$unite), ")", sep = "")) +
    labs(title = ">>> Aliments infantiles") +
    theme(axis.ticks = element_blank(),
          legend.position = "right",
          axis.text.y = element_blank(),
          axis.text.x = element_text(size=8),
          plot.title = element_text(face="bold", size=16),
          panel.background = element_rect(fill="white"),
          axis.text = element_text(size=16),
          panel.grid.major.x = element_line(colour = "lightgray"),
          panel.grid.minor.x = element_line(colour = "lightgray")
    ) +
    coord_flip()
  if (nrow(subdf_infantile) == 0) {
    g2 <- g2 +
      theme(axis.title = element_blank()) +
      labs(subtitle = ">>>>> Pas de données quantifiées")
  }
  
  
  ggpubr::ggarrange(g1, g2)
}




