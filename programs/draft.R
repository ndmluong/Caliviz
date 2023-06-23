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

