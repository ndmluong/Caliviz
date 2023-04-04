# PACKAGES ####
source("R/packages.R")

# FUNCTIONS ####
source("R/functions_eat2.R")

# Import the raw data ####
df_eat2_raw <- list(`Acrylamide` = read_excel(path = "data/raw/PASER2006sa0361-An02.xlsx", sheet = "Acrylamide"),
                    `HAP` = read_excel(path = "data/raw/PASER2006sa0361-An02.xlsx", sheet = "HAP"),
                    `Dioxines, PCB` = read_excel(path = "data/raw/PASER2006sa0361-An02.xlsx", sheet = "Dioxines, PCB"),
                    `Perfluorés` = read_excel(path = "data/raw/PASER2006sa0361-An02.xlsx", sheet = "Perfluorés"),
                    `Bromés` = read_excel(path = "data/raw/PASER2006sa0361-An02.xlsx", sheet = "Bromés"),
                    `Contaminants inorg et minéraux` = read_excel(path = "data/raw/PASER2006sa0361-An02.xlsx", sheet = "Contaminants inorg et minéraux"),
                    `Phytoestrogènes` = read_excel(path = "data/raw/PASER2006sa0361-An02.xlsx", sheet = "Phytoestrogènes"),
                    `Mycotoxines` = read_excel(path = "data/raw/PASER2006sa0361-An02.xlsx", sheet = "Mycotoxines"),
                    `Additifs` = read_excel(path = "data/raw/PASER2006sa0361-An02.xlsx", sheet = "Additifs"),
                    `Pesticides` = read_excel(path = "data/raw/PASER2006sa0361-An02.xlsx", sheet = "Pesticides"),
                    `BPA` = read_excel(path = "data/raw/donnees-bpa-eat2-lhn.xlsx", sheet = "BPA"))

df_eat2_raw$`Dioxines, PCB` %>% select(., - `Lipides (%)`) -> df_eat2_raw$`Dioxines, PCB`
df_eat2_raw$`BPA` %>% select(., - `LOD/LOQ`) -> df_eat2_raw$`BPA`

# Common variables ####
com_var_eat2 <- c("Groupe de la nomenclature INCA 2",
                  "Libellé",
                  "Type",
                  "Date",
                  "Région",
                  "Vague",
                  "Unité")

# Cleaning column names (check function f_rename_df)
df_eat2_raw <- lapply(df_eat2_raw, function(x) f_eat2_rename_df(x))

# Substances list ####
sapply(df_eat2_raw, function(x) {
  colnames(x)[!colnames(x) %in% com_var_eat2]
}) -> substances_list
subs_tab_eat2 <- data.frame(subs_grp = character(), subs = character())
for (x in 1:length(substances_list)) {
  subs_tab_eat2 <- rbind(subs_tab_eat2,
                    data.frame(subs_grp = rep(names(substances_list)[x], length(substances_list[[x]])),
                               subs = substances_list[[x]]))
}
rm(x, substances_list)

# Pre-treatment - change all characters to numeric ####
lapply(df_eat2_raw, function(x) {
  f_eat2_char2num_df(input_df = x,
                vars = com_var_eat2)
}) -> df_eat2


# Food list ####
food_tab_eat2 <- data.frame(food_grp = character(), food = character())
for (x in 1:length(df_eat2)) {
  food_tab_eat2 <- rbind(food_tab_eat2,
                data.frame(food_grp = df_eat2[[x]]$`Groupe de la nomenclature INCA 2`,
                           food = df_eat2[[x]]$`Libellé`))
}

food_tab_eat2 <- food_tab_eat2[!duplicated(food_tab_eat2), ]
food_tab_eat2 <- arrange(food_tab_eat2, food_grp)
rm(x)

# Check: Significative presence of substances in food ####
f_eat2_check_by_food(d = df_eat2,
                     food_list = food_tab_eat2$food,
                     food_table = food_tab_eat2,
                     subs_table = subs_tab_eat2,
                     vars = com_var_eat2) %>%
  do.call(cbind, .) %>%
  as.data.frame() %>%
  mutate(., `Libellé` = food_tab_eat2$food, .before = 1) -> df_mat_presence_eat2


# Cartography ####
## Cartography data
frsf <- sf::st_as_sf(readRDS("data/raw/FRA_adm1.rds"), "sf")

## Region code correspondence ####
reg_code <- read_excel(path = "data/raw/reg_code.xlsx", sheet = "reg_code")

frsf <- mutate(frsf, EAT2_region = as.character(reg_code$EAT2_Region), .after = NAME_1)

rm(df_eat2_raw)

save.image("data/processed/processed_data_eat2.RData")
