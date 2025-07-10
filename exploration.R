library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)

rm(list = ls())

filename <- "https://www.data.gouv.fr/fr/datasets/r/1c3f80c4-ace5-41ef-b6e1-983a8ccb5c69"
libraires <- read.csv(filename, sep = ";")

libraires_dates <- libraires %>%
  filter(str_detect(Type.de.brevet, regex("libraire", ignore_case = TRUE))) %>%
  filter(Date.de.début.du.brevet != "", Date.de.fin.du.brevet != "")

# PREMIERE PHASE D'EXTRACTION ----

# Extraction des jours
extraire_nombres <- function(modalite) {
  nombres <- stringr::str_extract_all(modalite, "\\b\\d{1,2}(?:er|nd|e|ème)?\\b") %>%
    unlist() %>%
    stringr::str_remove_all("(er|nd|e|ème)") %>%
    as.integer() %>%
    sprintf("%02d", .) %>%
    paste(collapse = " ")
  if (nombres == "") NA else nombres
}

# Extraction des mois
extraire_mois <- function(modalite) {
  mois <- str_extract_all(modalite, "(?i)\\b(janvier|février|mars|avril|mai|juin|juillet|août|septembre|octobre|novembre|décembre|janvier|fevrier|aout)\\b") %>%
    unlist() %>%
    paste(collapse = " ")
  if (mois == "") NA else mois
}

# Extraction des années
extraire_annees <- function(modalite) {
  annees <- str_extract_all(modalite, "(?<=\\s|^)[[:digit:]]{4}(?=\\s|$|[^[:digit:]])") %>%
    unlist() %>%
    paste(collapse = " ")
  if (annees == "") NA else annees
}

# Extraction des autres caractères (précisions sur les brevets quand il y a plusieurs dates)
extraire_autres <- function(modalite) {
  autres <- str_remove_all(modalite, "[[:digit:]]+|[[:space:]]+|(?i)janvier|février|mars|avril|mai|juin|juillet|août|septembre|octobre|novembre|décembre") %>%
    str_trim()
  if (autres == "") NA else autres
}

# Création des nouvelles colonnes en conséquence
libraires_dates <- libraires_dates %>%
  mutate(
    nombres = sapply(Date.de.début.du.brevet, extraire_nombres),
    mois = sapply(Date.de.début.du.brevet, extraire_mois),
    annees = sapply(Date.de.début.du.brevet, extraire_annees),
    autres = sapply(Date.de.début.du.brevet, extraire_autres),
    nombres_fin = sapply(Date.de.fin.du.brevet, extraire_nombres),
    mois_fin = sapply(Date.de.fin.du.brevet, extraire_mois),
    annees_fin = sapply(Date.de.fin.du.brevet, extraire_annees),
    autres_fin = sapply(Date.de.fin.du.brevet, extraire_autres),
  )

# DEUXIEME PHASE D'EXTRACTION ----

# Extraction des nombres s'il y en a plusieurs dans l'extraction précédente
extraire_huit_nombres <- function(modalites) {
  extraire_nombres_vectorise <- function(texte) {
    nombres <- stringr::str_extract_all(texte, "\\b\\d{1,2}(?:er|nd|e|ème)?\\b") %>%
      unlist() %>%
      as.integer() %>%
      sprintf("%02d", .)
    # Compléter avec des NA s'il y a moins de 8 valeurs
    length(nombres) <- 8
    return(nombres)
  }
  
  # Appliquer la fonction à chaque modalité et créer un data.frame
  donnees <- t(sapply(modalites, extraire_nombres_vectorise))
  donnees_df <- as.data.frame(donnees, stringsAsFactors = FALSE)
  
  # Renommer les colonnes
  colnames(donnees_df) <- paste0("n", 1:8)
  
  return(donnees_df)
}

# Extraction des mois s'il y en a plusieurs dans l'extraction précédente
extraire_huit_mois <- function(modalites) {
  # Liste des mois français en minuscules
  mois_fr <- c("janvier", "février", "mars", "avril", "mai", "juin",
               "juillet", "août", "septembre", "octobre", "novembre", "décembre")
  
  extraire_mois_vectorise <- function(texte) {
    # Mise en minuscules pour une détection insensible à la casse
    texte_min <- tolower(texte)
    
    # Extraire tous les mois présents dans le texte
    mois_extraits <- stringr::str_extract_all(texte_min, paste0("\\b(", paste(mois_fr, collapse = "|"), ")\\b")) %>%
      unlist()
    
    # Compléter avec NA s'il y a moins de 8 mois
    length(mois_extraits) <- 8
    return(mois_extraits)
  }
  
  # Appliquer à chaque modalité
  donnees <- t(sapply(modalites, extraire_mois_vectorise))
  donnees_df <- as.data.frame(donnees, stringsAsFactors = FALSE)
  colnames(donnees_df) <- paste0("mois", 1:8)
  
  return(donnees_df)
}

# Extraction des années s'il y en a plusieurs dans l'extraction précédente
extraire_huit_annees <- function(modalites) {
  extraire_annees_vectorise <- function(texte) {
    annees <- stringr::str_extract_all(texte, "\\b\\d{4}\\b") %>%
      unlist()
    
    # Compléter avec NA si < 8 années
    length(annees) <- 8
    return(annees)
  }
  
  donnees <- t(sapply(modalites, extraire_annees_vectorise))
  donnees_df <- as.data.frame(donnees, stringsAsFactors = FALSE)
  colnames(donnees_df) <- paste0("an", 1:8)
  
  return(donnees_df)
}

# On applique les fonctions aux colonnes précédemment créées
nombres_extraits <- extraire_huit_nombres(libraires_dates$nombres)
mois_extraits <- extraire_huit_mois(libraires_dates$mois)
annees_extraits <- extraire_huit_annees(libraires_dates$annees)
nombres_extraits_fin <- extraire_huit_nombres(libraires_dates$nombres_fin)
mois_extraits_fin <- extraire_huit_mois(libraires_dates$mois_fin)
annees_extraits_fin <- extraire_huit_annees(libraires_dates$annees_fin)

# On fusionne avec le dataframe
libraires_dates <- cbind(libraires_dates, nombres_extraits, mois_extraits, annees_extraits, nombres_extraits_fin, mois_extraits_fin, annees_extraits_fin)

# CREER LA COLONNE DES DATES DE DEBUT

creer_dates_debut <- function(df) {
  # Mois français → numéro de mois
  mois_fr <- c("janvier"=1, "février"=2, "mars"=3, "avril"=4, "mai"=5, "juin"=6,
               "juillet"=7, "août"=8, "septembre"=9, "octobre"=10,
               "novembre"=11, "décembre"=12)
  
  # Pour chaque ensemble (n1/mois1/an1 ... n8/mois8/an8)
  for (i in 1:8) {
    # Création du nom de la nouvelle colonne date
    nom_col <- paste0("date", i)
    
    # Récupération des colonnes jour, mois et année
    jour <- as.integer(df[[paste0("n", i)]])
    mois_nom <- tolower(df[[paste0("mois", i)]])
    mois <- mois_fr[mois_nom]
    annee <- as.integer(df[[paste0("an", i)]])
    
    # Création des dates avec vérification
    df[[nom_col]] <- as.Date(NA)  # initialise
    valide <- !is.na(jour) & !is.na(mois) & !is.na(annee)
    
    df[[nom_col]][valide] <- as.Date(sprintf("%04d-%02d-%02d", annee[valide], mois[valide], jour[valide]), format = "%Y-%m-%d")
  }
  
  return(df)
}

creer_dates_fin <- function(df) {
  # Mois français → numéro de mois
  mois_fr <- c("janvier"=1, "février"=2, "mars"=3, "avril"=4, "mai"=5, "juin"=6,
               "juillet"=7, "août"=8, "septembre"=9, "octobre"=10,
               "novembre"=11, "décembre"=12)
  
  # Pour chaque ensemble (n1/mois1/an1 ... n8/mois8/an8)
  for (i in 1:8) {
    # Création du nom de la nouvelle colonne date
    nom_col <- paste0("date", i".1")
    
    # Récupération des colonnes jour, mois et année
    jour <- as.integer(df[[paste0("n", i,".1")]])
    mois_nom <- tolower(df[[paste0("mois", i,".1")]])
    mois <- mois_fr[mois_nom]
    annee <- as.integer(df[[paste0("an", i,".1")]])
    
    # Création des dates avec vérification
    df[[nom_col]] <- as.Date(NA)  # initialise
    valide <- !is.na(jour) & !is.na(mois) & !is.na(annee)
    
    df[[nom_col]][valide] <- as.Date(sprintf("%04d-%02d-%02d", annee[valide], mois[valide], jour[valide]), format = "%Y-%m-%d")
  }
  
  return(df)
}

libraires_dates <- creer_dates_debut(libraires_dates)
libraires_dates <- creer_dates_fin(libraires_dates)

ggplot(libraires_dates[libraires_dates$an1 >= 1810 & libraires_dates$an1 <= 1881, ], aes(x = as.integer(an1))) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  scale_x_continuous(breaks = seq(1810, 1881, by = 5)) +
  labs(title = "Histogramme des années (an1)", x = "Année", y = "Fréquence") +
  theme_minimal()

# TRAVAIL EXPLORATOIRE SUR LES AUTRES DATES
libraires_dates_exemples <- libraires_dates %>%
  select(Type.de.brevet, Date.de.début.du.brevet, Date.de.fin.du.brevet, matches("^date\\d")) %>%
  filter(!is.na(date2))
# Parfois il y a deux dates car il y a deux professions
# Parfois il y a deux dates mais on ne sais pas pq