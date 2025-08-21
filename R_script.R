# Charger la bibliothèque nécessaire
library(tidyr)  # Pour transformer les données

# Charger le fichier Cap.csv
Cap <- read.csv2("Cap.csv", header = F, stringsAsFactors = FALSE)
head(Cap)
# Supposons que votre dataset s'appelle "data"
# Les colonnes de V2 à V19 correspondent aux années 1980 à 1997
colnames(Cap) <- c("Country", paste0("Year_", 1980:1997))  # Renommer les colonnes

# Transposer en format long
cap_long <- pivot_longer(
  Cap,
  cols = starts_with("Year_"),  # Toutes les colonnes qui commencent par "Year_"
  names_to = "Year",            # Le nom des colonnes devient une colonne "Year"
  values_to = "Value"           # Les valeurs sont regroupées sous la colonne "Value"
)

# Nettoyer la colonne "Year" pour enlever le préfixe "Year_"
cap_long$Year <- as.integer(sub("Year_", "", cap_long$Year))

# Vérification du résultat
head(cap_long)
cap_long$Value <- gsub(",", ".", cap_long$Value)  # Remplace les virgules par des points
cap_long$Value <- as.numeric(cap_long$Value)
head(cap_long)
#------------------------------------------------------------------------------------------------------------
import <- read.csv2("import.csv", header = F, stringsAsFactors = FALSE)
head(import)
# Supposons que votre dataset s'appelle "data"
# Les colonnes de V2 à V19 correspondent aux années 1980 à 1997
colnames(import) <- c("Country", paste0("Year_", 1980:1997))  # Renommer les colonnes

import[, 2:ncol(import)] <- lapply(import[, 2:ncol(import)], function(x) {
  x <- gsub(",", ".", x)  # Remplacer les virgules par des points
  as.numeric(as.character(x))  # Convertir en numérique
})


# Transposer en format long
import_long <- pivot_longer(
  import,
  cols = starts_with("Year_"),  # Toutes les colonnes qui commencent par "Year_"
  names_to = "Year",            # Le nom des colonnes devient une colonne "Year"
  values_to = "Value"           # Les valeurs sont regroupées sous la colonne "Value"
)

# Nettoyer la colonne "Year" pour enlever le préfixe "Year_"
import_long$Year <- as.integer(sub("Year_", "", import_long$Year))

# Vérification du résultat
head(import_long)
import_long$Value <- gsub(",", ".", import_long$Value)  # Remplace les virgules par des points
import_long$Value <- as.numeric(import_long$Value)

#-----------------------------------------------------------------------------------------------------------
inv <- read.csv2("inv.csv", header = F, stringsAsFactors = FALSE)
head(inv)

colnames(inv) <- c("Country", paste0("Year_", 1980:1997))

inv[, 2:ncol(inv)] <- lapply(inv[, 2:ncol(inv)], function(x) {
  x <- gsub(",", ".", x)
  as.numeric(as.character(x))
})


inv_long <- pivot_longer(
  inv,
  cols = starts_with("Year_"),
  names_to = "Year",
  values_to = "Value"
)

inv_long$Year <- as.integer(sub("Year_", "", inv_long$Year))

head(inv_long)
#----------------------------------------------------------------------------------------------------------------
labor <- read.csv2("labor.csv", header = F, stringsAsFactors = FALSE)
head(labor)

colnames(labor) <- c("Country", paste0("Year_", 1980:1997))

labor[, 2:ncol(labor)] <- lapply(labor[, 2:ncol(labor)], function(x) {
  x <- gsub(",", ".", x)
  as.numeric(as.character(x))
})

labor_long <- pivot_longer(
  labor,
  cols = starts_with("Year_"),
  names_to = "Year",
  values_to = "Value"
)

labor_long$Year <- as.integer(sub("Year_", "", labor_long$Year))

head(labor_long)
labor_long$Value <- gsub(",", ".", labor_long$Value)
labor_long$Value <- as.numeric(labor_long$Value)
#----------------------------------------------------------------------------------------------------------------
pib <- read.csv2("pib.csv", header = F, stringsAsFactors = FALSE)
head(pib)
colnames(pib) <- c("Country", paste0("Year_", 1980:1997))

pib[, 2:ncol(pib)] <- lapply(pib[, 2:ncol(pib)], function(x) {
  x <- gsub(",", ".", x)
  as.numeric(as.character(x))
})

pib_long <- pivot_longer(
  pib,
  cols = starts_with("Year_"),
  names_to = "Year",
  values_to = "Value"
)

pib_long$Year <- as.integer(sub("Year_", "", pib_long$Year))

head(pib_long)
pib_long$Value <- gsub(",", ".", pib_long$Value)
pib_long$Value <- as.numeric(pib_long$Value)
#----------------------------------------------------------------------------------------------------------------

X <- read.csv2("X.csv", header = F, stringsAsFactors = FALSE)
head(X)

colnames(X) <- c("Country", paste0("Year_", 1980:1997))

X[, 2:ncol(X)] <- lapply(X[, 2:ncol(X)], function(x) {
  x <- gsub(",", ".", x)
  as.numeric(as.character(x))
})

X_long <- pivot_longer(
  X,
  cols = starts_with("Year_"),
  names_to = "Year",
  values_to = "Value"
)

X_long$Year <- as.integer(sub("Year_", "", X_long$Year))

head(X_long)
X_long$Value <- gsub(",", ".", X_long$Value)
X_long$Value <- as.numeric(X_long$Value)
#-----------------------------------------------------------------------------------------------------------------------------
ide <- read.csv2("ide.csv", header = F, stringsAsFactors = FALSE)
head(ide)
colnames(ide) <- c("Country", paste0("Year_", 1980:1997))

ide[, 2:ncol(ide)] <- lapply(ide[, 2:ncol(ide)], function(x) {
  x <- gsub(",", ".", x)
  as.numeric(as.character(x))
})

ide_long <- pivot_longer(
  ide,
  cols = starts_with("Year_"),
  names_to = "Year",
  values_to = "Value"
)

ide_long$Year <- as.integer(sub("Year_", "", ide_long$Year))

head(ide_long)
ide_long$Value <- gsub(",", ".", ide_long$Value)
ide_long$Value <- as.numeric(ide_long$Value)
#-----------------------------------------------------------------------------------------------------------------------------
# Imputation

library('dplyr')
variables_long <- list(ide_long, X_long, pib_long, labor_long, import_long, cap_long, inv_long)

# Fonction pour remplacer les NA par la moyenne par pays
impute_by_country <- function(data) {
  data %>%
    group_by(Country) %>%
    mutate(
      mean_value = mean(Value, na.rm = TRUE),   # Calculer la moyenne pour chaque pays
      Value = ifelse(is.na(Value), mean_value, Value)  # Remplacer les NA par la moyenne
    ) %>%
    select(-mean_value)  # Supprimer la colonne intermédiaire
}

# Appliquer la fonction à chaque dataset
variables_long <- lapply(variables_long, impute_by_country)

# Réassigner les datasets modifiés à leurs noms respectifs
ide_long <- variables_long[[1]]
X_long <- variables_long[[2]]
pib_long <- variables_long[[3]]
labor_long <- variables_long[[4]]
import_long <- variables_long[[5]]
cap_long <- variables_long[[6]]
inv_long <- variables_long[[7]]

#Imputation pour les phillipines par Boostraping

if (!require(boot)) install.packages("boot")
library(boot)

# Récupérer les données de l'Indonésie (en supposant qu'il y a des valeurs disponibles)
indonesia_values <- inv_long %>%
  filter(Country == "Indonesia") %>%
  select(Value)

# Créer une fonction de bootstrap pour générer des valeurs simulées
bootstrap_imputation <- function(values, n = length(values)) {
  sample(values, size = n, replace = TRUE)
}
if (any(is.na(inv_long$Value[inv_long$Country == "Philippines"]))) {
  # Nombre de valeurs manquantes pour les Philippines
  n_missing <- sum(is.na(inv_long$Value[inv_long$Country == "Philippines"]))
  
  # Effectuer la simulation bootstrap en tirant des échantillons depuis la distribution de l'Indonésie
  simulated_values <- bootstrap_imputation(indonesia_values$Value, n_missing)
  
  # Remplacer les valeurs manquantes pour les Philippines par les valeurs simulées
  inv_long <- inv_long %>%
    mutate(Value = ifelse(
      Country == "Philippines" & is.na(Value),
      simulated_values,  # Remplacer par les valeurs simulées
      Value
    ))
}

labor_long <- labor_long %>%
  mutate(Country = replace(Country, Country == "Algerie", "Algeria"))

# Joindre les datasets `inv_long`, `labor_long`, et `pib_long` sur les colonnes `Country` et `Year`
pgf_data <- inv_long %>%
  rename(Investment = Value) %>% # Renommer la colonne pour clarté
  left_join(labor_long %>% rename(Labor = Value), by = c("Country", "Year")) %>%
  left_join(pib_long %>% rename(GDP = Value), by = c("Country", "Year"))



# Calculer la Productivité Globale des Facteurs (PGF)
pgf_data <- pgf_data %>%
  mutate(PGF = GDP / (Investment^0.4 * Labor^0.6))

PGF = read.csv2("pgf.csv", header = T, stringsAsFactors = FALSE)
head(PGF)

# Remplacer les valeurs des premières années par la moyenne des taux
PGF <- PGF %>%
  arrange(Country, Year) %>% # Trier par pays et année
  group_by(Country) %>% # Regrouper par entité spécifique (pays)
  mutate(
    Tx_PGF = ifelse(row_number() == 1, mean(Tx_PGF, na.rm = TRUE), Tx_PGF) # Remplacer pour la première année
  ) %>%
  ungroup() # Supprimer le regroupement

# Vérification des résultats
head(PGF)

















#----------------------------------------------------------------------------------------------
#transformation en taux de croissance
# Calculer le taux de croissance et remplacer la première année par la moyenne des autres taux
ide_growth <- ide_long %>%
  arrange(Country, Year) %>% # Trier par pays et année
  group_by(Country) %>% # Regrouper par pays
  mutate(
    Growth = (Value - lag(Value)) / lag(Value), # Calcul du taux de croissance
    Growth = ifelse(row_number() == 1, mean(Growth, na.rm = TRUE), Growth) # Remplacer le taux de la première année
  ) %>%
  ungroup() # Enlever le regroupement

# Remplacer les valeurs infinies et NaN par 0
ide_growth <- ide_growth %>%
  mutate(
    Growth = ifelse(is.infinite(Growth) | is.nan(Growth), 0, Growth)
  )

# Vérifier les résultats

X_growth <- X_long %>%
  arrange(Country, Year) %>% # Trier par pays et année
  group_by(Country) %>% # Regrouper par pays
  mutate(
    Growth = (Value - lag(Value)) / lag(Value), # Calcul du taux de croissance
    Growth = ifelse(row_number() == 1, mean(Growth, na.rm = TRUE), Growth) # Remplacer le taux de la première année
  ) %>%
  ungroup() # Enlever le regroupement


# Vérification des résultats

X_growth <- X_growth %>%
  mutate(
    Growth = ifelse(is.infinite(Growth) | is.nan(Growth), 0, Growth)
)


import_growth <- import_long %>%
  arrange(Country, Year) %>% # Trier par pays et année
  group_by(Country) %>% # Regrouper par pays
  mutate(
    Growth = (Value - lag(Value)) / lag(Value), # Calcul du taux de croissance
    Growth = ifelse(row_number() == 1, mean(Growth, na.rm = TRUE), Growth) # Remplacer le taux de la première année
  ) %>%
  ungroup() # Enlever le regroupement

# Vérification des résultats

import_growth <- import_growth %>%
  mutate(
    Growth = ifelse(is.infinite(Growth) | is.nan(Growth), 0, Growth) # Gérer les valeurs infinies ou NaN
  )

cap_growth <- cap_long %>%
  arrange(Country, Year) %>% # Trier par pays et année
  group_by(Country) %>% # Regrouper par pays
  mutate(
    Growth = (Value - lag(Value)) / lag(Value), # Calcul du taux de croissance
    Growth = ifelse(row_number() == 1, mean(Growth, na.rm = TRUE), Growth) # Remplacer le taux de la première année
  ) %>%
  ungroup() # Enlever le regroupement

# Vérification des résultats

cap_growth <- cap_growth %>%
  mutate(
    Growth = ifelse(is.infinite(Growth) | is.nan(Growth), 0, Growth) # Gérer les valeurs infinies ou NaN
  )

colnames(PGF)
# Préparer les datasets nécessaires

# Liste des datasets de taux de croissance
datasets_growth <- list(
  ide = ide_growth %>% select(Country, Year, Growth) %>% rename(TX_ide = Growth),
  import = import_growth %>% select(Country, Year, Growth) %>% rename(TX_imp = Growth),
  X = X_growth %>% select(Country, Year, Growth) %>% rename(TX_x = Growth),
  cap = cap_growth %>% select(Country, Year, Growth) %>% rename(TX_cap = Growth)
)

# Initialiser le panel final avec PGF
panel <- PGF %>% select(Country, Year, Tx_PGF)

# Jointures successives pour ajouter chaque dataset de taux de croissance
for (dataset_name in names(datasets_growth)) {
  panel <- panel %>%
    left_join(datasets_growth[[dataset_name]], by = c("Country", "Year"))
}
dataset = panel
View(panel)
head(panel)

panel <- panel %>%
  rename(TX_pgf = Tx_PGF)

head(panel)










#-----------------------------------------------------------------------------------------------------------------------
# Estimation du modele

if (!requireNamespace("plm")) install.packages("plm")
if (!requireNamespace("lme4")) install.packages("lme4")

# Charger les bibliothèques
library(plm)  # Analyse de données de panel
library(lme4) # Modèles mixtes linéaires


# Convertir les données en un format de panel
panel_data <- pdata.frame(dataset, index = c("Country", "Year"))
View(panel_data)

# Spécification du modèle
formula <- TX_pgf ~ TX_x + TX_imp + TX_cap + TX_ide + I(TX_imp * TX_cap)

# Estimation du modèle à effets aléatoires
modele_random <- plm(formula, data = panel_data, model = "random")
modele_fixed <- plm(formula, data = panel_data, model = "within")
# Résumé des résultats
summary(modele_random)
summary(modele_fixed)













