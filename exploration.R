library(tidyverse)

filename <- "https://www.data.gouv.fr/fr/datasets/r/1c3f80c4-ace5-41ef-b6e1-983a8ccb5c69"
libraires <- read.csv(filename, sep = ";")

table(libraires$Date.de.début.du.brevet)

libraires_dates_exemple <- libraires %>%
  select(Type.de.brevet, Date.de.début.du.brevet, Date.de.fin.du.brevet) %>%
  filter(str_detect(Type.de.brevet, regex("libraire", ignore_case = TRUE)))

library(dplyr)
library(stringr)

# Fonction pour extraire les nombres à un ou deux chiffres, incluant les suffixes
extraire_nombres <- function(modalite) {
  nombres <- str_extract_all(modalite, "\\b\\d{1,2}(?:er|nd|e|ème|ème|ème|er|e)?\\b") %>%
    unlist() %>%
    str_remove_all("er|nd|e|ème") %>%
    paste(collapse = " ")
  if (nombres == "") NA else nombres
}

# Fonction pour extraire les mois
extraire_mois <- function(modalite) {
  mois <- str_extract_all(modalite, "(?i)\\b(janvier|février|mars|avril|mai|juin|juillet|août|septembre|octobre|novembre|décembre|janvier|fevrier|aout)\\b") %>%
    unlist() %>%
    paste(collapse = " ")
  if (mois == "") NA else mois
}

# Fonction pour extraire les années
extraire_annees <- function(modalite) {
  annees <- str_extract_all(modalite, "(?<=\\s|^)[[:digit:]]{4}(?=\\s|$|[^[:digit:]])") %>%
    unlist() %>%
    paste(collapse = " ")
  if (annees == "") NA else annees
}

# Fonction pour extraire les autres caractères
extraire_autres <- function(modalite) {
  autres <- str_remove_all(modalite, "[[:digit:]]+|[[:space:]]+|(?i)janvier|février|mars|avril|mai|juin|juillet|août|septembre|octobre|novembre|décembre") %>%
    str_trim()
  if (autres == "") NA else autres
}

libraires_dates <- libraires_dates_exemple %>%
  mutate(
    nombres = sapply(Date.de.début.du.brevet, extraire_nombres),
    mois = sapply(Date.de.début.du.brevet, extraire_mois),
    annees = sapply(Date.de.début.du.brevet, extraire_annees),
    autres = sapply(Date.de.début.du.brevet, extraire_autres)
  )

table(libraires_dates$mois, useNA = "always")
