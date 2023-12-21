# Replication archive for: "Delaying Carbon Debt Drawdown Fails Younger Generations"

# Contact for clarifications: [ANONYMISED]       

# Script contents: Pre-process AR6 scenarios database

# LOAD PACKAGES ----------------------------------------------------------------

#install.packages("pacman")
library(pacman)

# processing
p_load(dplyr, tidyr, readr, readxl, writexl, purrr, ggplot2, forcats, stringr, 
       patchwork, ggrepel)

# misc
p_load(here, countrycode, zoo)

# options
options(scipen = 999)

# READ IN FULL AR6 R10 DATABASE ------------------------------------------------

ar6_full <- read_csv(here("Data", "pathways", "ar6_all", "AR6_Scenarios_Database_R10_regions_v1.1.csv"))
ar6_indicators <- read_xlsx(here("Data", "pathways", "ar6_all", "AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx"),
                            sheet = 2)

ar6_full <- ar6_full %>% 
  filter(Variable %in% c("Emissions|CO2|Energy and Industrial Processes",
                         "Population"))

ar6_indicators <- ar6_indicators %>% 
  select(Model, Scenario, Category)

# Write to file
ar6_full <- left_join(ar6_full, ar6_indicators) %>% 
  write_csv(here("Data", "pathways", "ar6_all", "ar6_all_co2ffi.csv"))
