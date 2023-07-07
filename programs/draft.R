# PACKAGES ####
source("R/packages.R")

# Données de contribution
df_eat2_ctA <- read_excel(path = "data/raw/eat2_contrib_grp.xlsx", sheet = "Adultes")
df_eat2_ctE <- read_excel(path = "data/raw/eat2_contrib_grp.xlsx", sheet = "Enfants")

df_eat2_ctA <- mutate(df_eat2_ctA, Population = "Adultes", .after = Substance)
df_eat2_ctE <- mutate(df_eat2_ctE, Population = "Enfants", .after = Substance)

df_eat2_ct <- rbind(df_eat2_ctA, df_eat2_ctE)
rm(df_eat2_ctA, df_eat2_ctE)

substance <- "Mercure"
substance <- "Arsenic"

df_pie <- subset(df_eat2_ct, Substance == substance)

# mypiecols <- randomcoloR::randomColor(count = 41, luminosity = "dark")
# mypiecols <- rainbow(41, alpha = 0.9, start = 0.1, end = 0.8)
mypiecols <- viridisLite::turbo(n = 41, alpha = 0.7, begin = 0, end = 0.9, direction = 1)
set.seed(408)
mypiecols <- sample(mypiecols)

font_subplot <- list(
  family = "Courier New, monospace",
  size = 18, color = "black")

g_ctA_LB <- plot_ly(data = subset(df_pie, Population == "Adultes"),
                    labels = ~ `Groupe d'aliments`,
                    values = ~ `Contribution_LB`,
                    type = "pie",
                    textposition = "inside",
                    textinfo = 'label',
                    hoverinfo = "label+percent",
                    hole = 0.3,
                    marker = list(colors = mypiecols),
                    showlegend = F) %>%
  layout(annotations = list(text = "Adultes - LB", font = font_subplot,
                            xref = "paper", yref = "paper",
                            yanchor = "bottom", xanchor = "center",
                            align = "center",
                            # x = 0.5, y = 1,
                            showarrow = F))
g_ctA_UB <- plot_ly(data = subset(df_pie, Population == "Adultes"),
                    labels = ~ `Groupe d'aliments`,
                    values = ~ `Contribution_UB`,
                    type = "pie",
                    textposition = "inside",
                    textinfo = 'label',
                    hoverinfo = "label+percent",
                    hole = 0.3,
                    marker = list(colors = mypiecols),
                    showlegend = F) %>%
  layout(annotations = list(text = "Adultes - UB", font = font_subplot,
                            xref = "paper", yref = "paper",
                            yanchor = "bottom", xanchor = "center",
                            align = "center",
                            # x = 0.5, y = 1,
                            showarrow = F))
g_ctA_LB
g_ctA_UB

p <- subplot(g_ctA_LB, g_ctA_UB, nrows = 1) %>%
  layout(showlegend = FALSE)
p


p1 <- plot_ly(economics, x = ~date, y = ~uempmed)
p2 <- plot_ly(economics, x = ~date, y = ~unemploy)
subplot(p1, p2, p1, p2, nrows = 2, margin = 0.05)


plotly::plot_ly() %>%
  add_pie(data = subset(df_pie, Population == "Adultes"),
          labels = ~ `Groupe d'aliments`,
          values = ~ `Contribution_LB`,
          textposition = "inside",
          textinfo = 'label',
          hoverinfo = "label+percent",
          hole = 0.3,
          marker = list(colors = mypiecols),
          name = "Adultes - LB", domain = list(x = c(0,0.5), y = c(0.5,1)),
          showlegend = F) %>%
  add_pie(data = subset(df_pie, Population == "Adultes"),
          labels = ~ `Groupe d'aliments`,
          values = ~ `Contribution_UB`,
          textposition = "inside",
          textinfo = 'label',
          hoverinfo = "label+percent",
          hole = 0.3,
          marker = list(colors = mypiecols),
          name = "Adultes - UB", domain = list(x = c(0.5, 1), y = c(0.5,1)),
          showlegend = F) %>%
  add_pie(data = subset(df_pie, Population == "Enfants"),
          labels = ~ `Groupe d'aliments`,
          values = ~ `Contribution_LB`,
          textposition = "inside",
          textinfo = 'label',
          hoverinfo = "label+percent",
          hole = 0.3,
          marker = list(colors = mypiecols),
          name = "Enfants - LB", domain = list(x = c(0, 0.5), y = c(0,0.5)),
          showlegend = F) %>%
  add_pie(data = subset(df_pie, Population == "Enfants"),
          labels = ~ `Groupe d'aliments`,
          values = ~ `Contribution_UB`,
          textposition = "inside",
          textinfo = 'label',
          hoverinfo = "label+percent",
          hole = 0.3,
          marker = list(colors = mypiecols),
          name = "Enfants - UB", domain = list(x = c(0.5, 1), y = c(0,0.5)),
          showlegend = F) %>%
  layout(title = list(text = paste("Substance: ", substance, sep = ""),
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
         margin = list(l = 10, r = 10, b = 10, t = 80, pad = 4))

sum(is.na(df_pie$Contribution_LB))




f_eat2_plot_contri(df = df_eat2_ct, subs_input = "Mercure")
f_eat2_plot_contri(df = df_eat2_ct, subs_input = "Plomb")
f_eat2_plot_contri(df = df_eat2_ct, subs_input = "Calcium")


f_eat2_plot_contri_bar <- function(
  df, ## data frame (contribution 2 populations with the column "Hypothesis" (expanded table))
  subs_input,
  hyp_input
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
      xlab("") + ylab("") + labs(title = "Population: Adultes") +
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
      xlab("") + ylab("") + labs(title = "Population: Enfants") +
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
      labs(subtitle = ">>> Les contributions relatives n'ont pas été estimées pour l'hypothèse sélectionnée")
  } else {
    gridExtra::grid.arrange(g_A, g_E, nrow = 1,
                            top = paste("Substance: ", subs_input, " - Hypothèse: ", hyp_input, sep =""),
                            left = "Groupe d'aliments",
                            bottom = "Contribution (%)")
  }
  
}

f_eat2_plot_contri_bar(df = df_eat2_ct_expanded, subs_input = "Cadmium", hyp_input = "LB")
f_eat2_plot_contri_bar(df = df_eat2_ct_expanded, subs_input = "Cadmium", hyp_input = "MB")
f_eat2_plot_contri_bar(df = df_eat2_ct_expanded, subs_input = "Cadmium", hyp_input = "UB")

f_eat2_plot_contri_bar(df = df_eat2_ct_expanded, subs_input = "Arsenic inorganique", hyp_input = "UB")
f_eat2_plot_contri_bar(df = df_eat2_ct_expanded, subs_input = "Arsenic inorganique", hyp_input = "MB")
