# Libraries -------------------------------------------------------------------------------------------------------
library(openxlsx)
library(tidyverse)
library(stringr)


# LOADING ---------------------------------------------------------------------------------------------------------
care <- read.csv("dataframe_abces_dentaire_20221228.csv", encoding = "UTF-8")

rouen_doc <- read.xlsx("Extraction.xlsx", sheet = 1)
rouen_sej_pmsi <- read.xlsx("Extraction.xlsx", sheet = 2)
rouen_prescriptions_atb <- read.xlsx("Extraction.xlsx", sheet = 3)
rouen_codes_pmsi <- read.xlsx("Extraction.xlsx", sheet = 4)
rouen_codes_ccam <- read.xlsx("Extraction.xlsx", sheet = 5)
rouen_concepts_long <- read.xlsx("Extraction.xlsx", sheet = 6)
rouen_concepts_wide <- read.xlsx("Extraction.xlsx", sheet = 7)
rouen_nomenclature_ccam <- read.xlsx("Extraction.xlsx", sheet = 9)


# write.xlsx(care, "care_plateforme.xlsx")


# Data management CARE platform -------------------------------------------------------------------------------------------------
# Checking how many unique IDs exist. Useful to verify next step
care %>%
  select(contains("id")) %>%
  summarise(
    consultation_id = n_distinct(consultation_id),
    patient_id = n_distinct(patient_id),
    physician_id = n_distinct(physician_id)
  )

# Replacing encrypted IDs with more manageable numbers
care <- care %>% mutate(
  consultation_id = as.numeric(as.factor(care$consultation_id)),
  patient_id = as.numeric(as.factor(care$patient_id)),
  physician_id = as.numeric(as.factor(care$physician_id))
)

# Verifying how many new IDs have been created.
care %>%
  select(contains("id")) %>%
  apply(2, max)

# Dropping non useful variables
care <- care %>% select(-contains(c("estimated_start_at", "day", "hour", "ticked", "offer", "sickleave")))



# Data management Rouen -------------------------------------------------------------------------------------------
# * PMSI codes ----------------------------------------------------------------------------------------------------
# Properly formatting to wide format
rouen_codes_pmsi <-
  rouen_codes_pmsi %>%
  group_by(PATID, EVTID) %>%
  summarise(PMSI = str_flatten(PMSI))
# Widening
rouen_codes_pmsi <- rouen_codes_pmsi %>%
  mutate(PMSI = str_split_fixed(PMSI, "\\s", n = 50)) %>%
  apply(2, str_remove, "\\:.+$")
# Getting rid of redundancy (code:code)
rouen_codes_pmsi <- rouen_codes_pmsi %>%
  apply(2, str_remove, "\\:.+$") %>%
  as.data.frame()
# Pivoting back to long format to comfortably erase duplicates then back to wide format
rouen_codes_pmsi <- pivot_longer(rouen_codes_pmsi, cols = 3:52, values_to = "pmsi") %>%
  select(-name) %>% # dropping name column
  distinct() %>% # dropping duplicates
  group_by(PATID, EVTID) %>%
  summarise(pmsi = str_flatten(pmsi, collapse = ",")) %>%
  mutate(pmsi = str_remove(pmsi, "\\,$"))
# Adding PMSI labels
case_when(str_detect(rouen_codes_pmsi$pmsi, "K047") ~ "Abces periapical",
          str_detect(rouen_codes_pmsi$pmsi, "K046") ~ "Abces periapical avec fistule",
          str_detect(rouen_codes_pmsi$pmsi, "K052") ~ "Periodontite aigue",
          str_detect(rouen_codes_pmsi$pmsi, "K053") ~ "Periodondite",
          str_detect(rouen_codes_pmsi$pmsi, "K02.") ~ "Carie",
          str_detect(rouen_codes_pmsi$pmsi, "K088") ~ "Autres affections precisees des dents et du parodonte",
          str_detect(rouen_codes_pmsi$pmsi, "K089") ~ "Affection des dents et du parodonte, sans precision",
          str_detect(rouen_codes_pmsi$pmsi, "K040|K045|K048") ~ "Maladies de la pulpe et des tissu périapicaux, autres et sans précision",
          str_detect(rouen_codes_pmsi$pmsi, "K050|K056") ~ "Maladie périodontale, sans précision ",
          str_detect(rouen_codes_pmsi$pmsi, "K068") ~ "Affection de la gencive et de la crête alvéolaire édentée, sans précision")

# CCAM codes ------------------------------------------------------------------------------------------------------
# Adding labels to acts
rouen_codes_ccam <- left_join(rouen_codes_ccam,rouen_nomenclature_ccam[c(1,2)], by = c("CODEACTE" = "Code"))
