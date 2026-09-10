# Replication archive for: "Using net-zero carbon debt to track climate overshoot responsibility"

# Contact for clarifications: Setu Pelz (pelz@iiasa.ac.at)    

# Script contents: Determine remaining carbon budgets for countries and regions.

# LOAD PACKAGES ----------------------------------------------------------------

#install.packages("pacman")
library(pacman)

# processing
p_load(dplyr, tidyr, readr, readxl, writexl, purrr, ggplot2, forcats, stringr, 
       patchwork, countrycode)

# misc
p_load(here, countrycode, zoo)

# options
options(scipen = 999)

# COUNTRY NAMES AND REGIONAL GROUPING ------------------------------------------

# Determine country-years for analysis
iso3c_tbl <- read_csv(here("Data", "countrygroups", "iso3c_region_mapping_20240319.csv"), show_col_types = FALSE) %>% 
  mutate(r10 = r10_iamc) %>% 
  select(iso3c, r10) %>% 
  group_by(iso3c, r10) %>% 
  expand(year = 1850:2050) %>% 
  ungroup()

# Set consistent r10 ordering
r10order <- tibble(r10 = c("R10NORTH_AM", "R10EUROPE", "R10PAC_OECD", "R10REF_ECON", "R10CHINA+", "R10MIDDLE_EAST", "R10REST_ASIA", "R10LATIN_AM", "R10AFRICA", "R10INDIA+"),
                   r10label = c("NAM", "EUR", "APD", "EEA", "EAS", "MEA", "PAS", "LAC", "AFR", "SAS"),
                   r10labellong = c("North America", "Europe", "Asia-Pacific Developed",
                                    "Eastern Europe and West-Central Asia",
                                    "Eastern Asia", "North Africa and Middle East", "South-East Asia and developing Pacific",
                                    "Latin America and Caribbean", 
                                    "Sub-saharan Africa", "Southern Asia"))

# Adjust labels to reflect those for publication
iso3c_tbl <- iso3c_tbl %>% 
  left_join(r10order) %>% 
  select(iso3c, r10, r10label, r10labellong, year)

# GDP MER 1990-2019 (WDI) ------------------------------------------------------

recent_gdpmer <- 
  read_csv(here("Data", "equity_data", "API_NY.GDP.MKTP.CD_DS2_en_csv_v2_5607117.csv"),
           skip = 3, show_col_types = FALSE) %>% 
  select(iso3c = `Country Code`, matches("\\d{4}")) %>% 
  pivot_longer(-iso3c, names_to = "year", values_to = "gdpcurrmer") %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year >= 1989, year <= 2019)

recent_gdpmer <- recent_gdpmer %>% 
  group_by(iso3c) %>% 
  mutate(gdpcurrmer = na.approx(gdpcurrmer, rule = 2, maxgap = 5)) %>% 
  ungroup() %>% 
  # Add psuedo-iso3c for Bunkers
  bind_rows(
    tibble(iso3c = "Bunkers", year = 1990:2019, gdpcurrmer = 0)
  )

# GDP PPP 1990-2019 (WDI) ------------------------------------------------------

recent_gdpppp <- 
  read_csv(here("Data", "equity_data", "API_NY.GDP.MKTP.PP.CD_DS2_en_csv_v2_1090665.csv"),
           skip = 3, show_col_types = FALSE) %>% 
  select(iso3c = `Country Code`, matches("\\d{4}")) %>% 
  pivot_longer(-iso3c, names_to = "year", values_to = "gdpcurrppp") %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year >= 1989, year <= 2019)

recent_gdpppp <- recent_gdpppp %>% 
  group_by(iso3c) %>% 
  mutate(gdpcurrppp = na.approx(gdpcurrppp, rule = 2, maxgap = 5)) %>% 
  ungroup() %>% 
  # Add psuedo-iso3c for Bunkers
  bind_rows(
    tibble(iso3c = "Bunkers", year = 1990:2019, gdpcurrppp = 0)
  )

# POPULATION (OWID) ------------------------------------------------------------

pophist <- read_csv(here("Data", "equity_data", "population.csv"), show_col_types = FALSE) %>% 
  filter(Year >= 1850, Year <= 2019) %>% 
  mutate(Code = ifelse(Entity == "World", "WLD", Code)) %>% 
  select(iso3c = Code, year = Year, pop = `Population (historical estimates)`) %>% 
  # Add psuedo-iso3c for Bunkers
  bind_rows(
    tibble(iso3c = "Bunkers", year = 1990:2019, pop = 0)
  )

# GLOBAL CARBON PROJECT EMISSIONS ----------------------------------------------

# Territorial CO2-FFI emissions
terr_co2ffi <- read_xlsx(here("Data", "equity_data", 
                              "National_Fossil_Carbon_Emissions_2023v1.0.xlsx"),
                         sheet = 2, skip = 11) %>% 
  rename(year = `...1`) %>% 
  filter(year != "QF") %>%
  pivot_longer(-year, names_to = "country_name", values_to = "MtC") %>% 
  mutate(terr_GtCO2FFI = MtC * 44/12 / 1e3,
         iso3c = countrycode(country_name, origin = "country.name", destination = "iso3c"),
         iso3c = ifelse(country_name == "World", "WLD", iso3c)) %>% 
  mutate(iso3c = ifelse(country_name == "Bunkers", "Bunkers", iso3c)) %>% 
  filter(!country_name %in% c("KP Annex B", "Non KP Annex B", "OECD", "Non-OECD", 
                              "EU27", "Africa", "Asia", "Central America", "Europe", 
                              "Middle East", "North America", "Oceania", "South America", 
                              "Statistical Difference")) %>% 
  select(iso3c, year, terr_GtCO2FFI)

# Territorial LULUCF emissions
terr_lulucf_blue <- read_xlsx(here("Data", "equity_data", 
                                   "National_LandUseChange_Carbon_Emissions_2023v1.0.xlsx"),
                              sheet = 2, skip = 7) %>% 
  rename(year = `unit: Tg C/year`) %>% 
  filter(year != "QF") %>%
  mutate(year = as.numeric(year)) %>% 
  pivot_longer(-year, names_to = "country_name", values_to = "MtC") %>% 
  mutate(lulucf_GtCO2_blue = MtC * 44/12 / 1e3,
         iso3c = countrycode(country_name, origin = "country.name", destination = "iso3c")) %>% 
  filter(!country_name %in% c("OTHER", "DISPUTED", "EU27")) %>% 
  mutate(iso3c = ifelse(country_name == "Global", "WLD", iso3c)) %>% 
  select(iso3c, year, lulucf_GtCO2_blue) %>% 
  # Add psuedo-iso3c for Bunkers
  bind_rows(
    tibble(iso3c = "Bunkers", year = unique(.$year), lulucf_GtCO2_blue = 0)
  )

terr_lulucf_hc2023 <- read_xlsx(here("Data", "equity_data", 
                                     "National_LandUseChange_Carbon_Emissions_2023v1.0.xlsx"),
                                sheet = 3, skip = 7) %>% 
  rename(year = `unit: Tg C/year`) %>% 
  filter(year != "QF") %>%
  mutate(year = as.numeric(year)) %>% 
  pivot_longer(-year, names_to = "country_name", values_to = "MtC") %>% 
  mutate(lulucf_GtCO2_hc2023 = MtC * 44/12 / 1e3,
         iso3c = countrycode(country_name, origin = "country.name", destination = "iso3c")) %>% 
  filter(!country_name %in% c("OTHER", "DISPUTED", "EU27")) %>% 
  mutate(iso3c = ifelse(country_name == "Global", "WLD", iso3c)) %>% 
  select(iso3c, year, lulucf_GtCO2_hc2023) %>% 
  # Add psuedo-iso3c for Bunkers
  bind_rows(
    tibble(iso3c = "Bunkers", year = unique(.$year), lulucf_GtCO2_hc2023 = 0)
  )

terr_lulucf_oscar <- read_xlsx(here("Data", "equity_data", 
                                    "National_LandUseChange_Carbon_Emissions_2023v1.0.xlsx"),
                               sheet = 4, skip = 7) %>% 
  rename(year = `unit: Tg C/year`) %>% 
  filter(year != "QF") %>%
  mutate(year = as.numeric(year)) %>% 
  pivot_longer(-year, names_to = "country_name", values_to = "MtC") %>% 
  mutate(lulucf_GtCO2_oscar = MtC * 44/12 / 1e3,
         iso3c = countrycode(country_name, origin = "country.name", destination = "iso3c")) %>% 
  filter(!country_name %in% c("OTHER", "DISPUTED", "EU27")) %>% 
  mutate(iso3c = ifelse(country_name == "Global", "WLD", iso3c)) %>% 
  select(iso3c, year, lulucf_GtCO2_oscar) %>% 
  # Add psuedo-iso3c for Bunkers
  bind_rows(
    tibble(iso3c = "Bunkers", year = unique(.$year), lulucf_GtCO2_oscar = 0)
  )

terr_lulucf <- list(terr_lulucf_blue, terr_lulucf_hc2023, terr_lulucf_oscar) %>% 
  reduce(full_join) 

# Check global LULUCF emissions match country-reported emissions
terr_lulucf %>% 
  filter(iso3c == "WLD") %>% 
  summarise(across(starts_with("lulucf"), sum))
terr_lulucf %>% 
  filter(iso3c != "WLD") %>% 
  summarise(across(starts_with("lulucf"), sum))

emiss_gtco2 <- list(terr_co2ffi, terr_lulucf) %>% 
  reduce(full_join) %>% 
  arrange(iso3c, year) %>% 
  select(iso3c, year, terr_GtCO2FFI, everything()) %>% 
  # Set GtCO2 as zero prior to 1990 if NA.
  # This does not change the countries we consider as 'missing' data between 
  # 1990-2022 in the primary specification and addresses the issue of missing
  # data where neighbouring years are 0 or very close to 0.
  mutate(across(matches("CO2"), ~ifelse(year < 1990 & is.na(.), 0, .))) %>% 
  mutate(lulucf_GtCO2_mean = rowMeans(select(., matches("lulucf_GtCO2"))))

# PROJECTED POPULATION (IIASA WIC SSP2) ----------------------------------------

# Future population projections 2025-2100 (IIASA SSP2) 
popssp2 <- read_csv(here("data", "equity_data", 
                         "SspDb_country_data_2013-06-12.csv")) %>%
  filter(MODEL == "IIASA-WiC POP", SCENARIO %in% c("SSP2_v9_130115"), 
         VARIABLE == "Population") %>% 
  select(iso3c = REGION, `2025`, `2030`, `2035`,`2040`, `2045`,`2050`,
         `2060`, `2070`, `2080`, `2090`, `2100`) %>% 
  mutate(across(-c(iso3c), ~ . * 1e6))  %>% 
  pivot_longer(-c(iso3c), names_to = "year", values_to = "pop") %>% 
  mutate(year = as.numeric(year)) 

# Interpolate
popproj <- pophist %>% 
  filter(iso3c %in% unique(popssp2$iso3c)) %>% 
  rbind(popssp2 %>% 
          filter(iso3c %in% unique(pophist$iso3c))) %>% 
  right_join(iso3c_tbl %>% distinct(iso3c, r10)) %>% 
  arrange(iso3c, r10, year) %>% 
  group_by(iso3c, r10) %>% 
  complete(year = c(1850:2100)) %>% 
  group_by(iso3c) %>% 
  mutate(pop = round(zoo::na.approx(pop, maxgap = 50))) %>% 
  filter(year >= 2020)
  
# Determine missing countries
miss_popproj <- popproj %>% 
  group_by(iso3c) %>% 
  summarise(n_total = n(),
            n_missing = sum(is.na(pop) | is.na(r10)))

# Remove missing countries
popproj <- popproj %>%
  filter(!iso3c %in% (miss_popproj %>% filter(n_missing > 0) %>% pull(iso3c))) %>% 
  arrange(r10, iso3c, year) %>% 
  select(r10, iso3c, year, pop) %>% 
  filter(year <= 2100)

# DETERMINE SET OF COUNTRIES WITH COMPLETE DATA --------------------------------

# Remove missing data across all input sources and inner join
final_iso3c <- 
  list(pophist %>% 
         group_by(iso3c) %>%
         filter(!any(across(pop, is.na))),
       popproj %>% 
         group_by(iso3c) %>%
         filter(!any(across(pop, is.na))),
       emiss_gtco2 %>% 
         group_by(iso3c) %>%
         filter(!any(across(all_of(names(emiss_gtco2 %>% select(-iso3c))), is.na))),
       recent_gdpmer %>% 
         group_by(iso3c) %>%
         filter(!any(across(gdpcurrmer, is.na))),
       recent_gdpppp %>% 
         group_by(iso3c) %>%
         filter(!any(across(gdpcurrppp, is.na)))) %>% 
  map(~na.omit(.) %>% distinct(iso3c)) %>% 
  reduce(inner_join) %>% 
  filter(iso3c != "WLD") %>% 
  left_join(iso3c_tbl %>% distinct(iso3c, r10)) %>% 
  mutate(country_name = countrycode(iso3c, origin = "iso3c", destination = "country.name"),
         country_name = ifelse(iso3c == "Bunkers", "Bunkers", country_name)) %>% 
  arrange(r10, iso3c)

# Determine which countries were removed from analysis
iso3c_missing <- iso3c_tbl %>% 
  distinct(iso3c) %>% 
  filter(!iso3c %in% final_iso3c$iso3c) %>% 
  arrange(iso3c) %>% 
  mutate(country_name = countrycode(iso3c, origin = "iso3c", destination = "country.name"))

# Determine 2019 emissions attributable to these countries
iso3c_missing <- left_join(iso3c_missing, emiss_gtco2) %>% 
  filter(year == 2022) %>% 
  left_join(popproj) %>% 
  mutate(gtco2share = terr_GtCO2FFI / (sum(emiss_gtco2$terr_GtCO2FFI[emiss_gtco2$year == 2022], na.rm = T)),
         terr_GtCO2FFI_share = scales::percent(gtco2share, accuracy = .01),
         pop_share = pop / sum(popproj$pop[popproj$year == 2020], na.rm = T),
         pop_share = scales::percent(pop_share, accuracy = .01),
         year = 2022, 
         countryname = countrycode::countrycode(iso3c, "iso3c", "country.name")) %>% 
  select(r10, iso3c, countryname, year, terr_GtCO2FFI, terr_GtCO2FFI_share, pop, pop_share) %>% 
  arrange(desc(pop_share))

write_csv(iso3c_missing, here("Data", "processed", "01_iso3c_miss_to_agg_row.csv"))

# FINALISE COMPLETE PROCESSED ANALYSIS DATASETS --------------------------------

# Filter GDP MER dataset to analysis iso3c vector and add rest-of-world totals
recent_gdpmer_row <- 
  tibble(iso3c = "ROW", year = 1989:2019, gdpcurrmer = 
           (recent_gdpmer %>%
              filter(iso3c == "WLD") %>%
              pull(gdpcurrmer)) -
           (recent_gdpmer %>%
              filter(iso3c %in% final_iso3c$iso3c) %>%
              group_by(year) %>% 
              summarise(gdpcurrmer = sum(gdpcurrmer)) %>% 
              pull(gdpcurrmer)))

recent_gdpmer_analysis <- recent_gdpmer %>% 
  filter(iso3c %in% final_iso3c$iso3c) %>% 
  bind_rows(recent_gdpmer_row)

# Check that the rest-of-world total is correct (in international dollars) 
sum(recent_gdpmer_analysis$gdpcurrmer) - 
  (recent_gdpmer %>% 
     filter(iso3c == "WLD") %>% 
     pull(gdpcurrmer) %>% sum(.))

# Filter GDP PPP dataset to analysis iso3c vector and add rest-of-world totals
recent_gdpppp_row <- 
  tibble(iso3c = "ROW", year = 1989:2019, gdpcurrppp = 
           (recent_gdpppp %>%
              filter(iso3c == "WLD") %>%
              pull(gdpcurrppp)) -
           (recent_gdpppp %>%
              filter(iso3c %in% final_iso3c$iso3c) %>%
              group_by(year) %>% 
              summarise(gdpcurrppp = sum(gdpcurrppp)) %>% 
              pull(gdpcurrppp)))

recent_gdpppp_analysis <- recent_gdpppp %>% 
  filter(iso3c %in% final_iso3c$iso3c) %>% 
  bind_rows(recent_gdpppp_row)

# Check that the rest-of-world total is correct (in international dollars)
sum(recent_gdpppp_analysis$gdpcurrppp) - 
  (recent_gdpppp %>% 
     filter(iso3c == "WLD") %>% 
     pull(gdpcurrppp) %>% sum(.))

# Filter pophist to analysis iso3c vector, add rest-of-world totals
pophist_row <- 
  tibble(iso3c = "ROW", year = 1850:2019, pop = 
           (pophist %>%
              filter(iso3c == "WLD") %>%
              pull(pop)) -
           (pophist %>%
              filter(iso3c %in% final_iso3c$iso3c) %>%
              group_by(year) %>% 
              summarise(pop = sum(pop)) %>% 
              pull(pop)))

pophist_analysis <- pophist %>% 
  filter(iso3c %in% final_iso3c$iso3c) %>% 
  bind_rows(pophist_row)

# Check that the rest-of-world total is correct
sum(pophist_analysis$pop) - 
  (pophist %>% 
     filter(iso3c == "WLD") %>% 
     pull(pop) %>% sum(.))

# Row-bind with popproj and fill in ROW for that dataset based on past trends
popproj_rowwld <- 
  tibble(year = 1850:2100) %>% 
  left_join(pophist_row %>% select(year, ROW_incomplete = pop)) %>% 
  left_join(pophist %>%
              filter(iso3c == "WLD") %>%
              select(year, WLD_subset = pop)) %>% 
  left_join(popproj %>% 
              group_by(year) %>%
              summarise(WLD_subset_proj = sum(pop))) %>% 
  mutate(WLD_subset = ifelse(is.na(WLD_subset), WLD_subset_proj, WLD_subset)) %>%
  select(year, ROW_incomplete, WLD_subset)

# Simple linear model for ROW population
lm_row = lm(ROW_incomplete ~ WLD_subset, data = popproj_rowwld %>% na.omit())

# Predict ROW population
popproj_rowwld$ROW_fit <- predict(lm_row, newdata = popproj_rowwld)

# Set new ROW projected values
popproj_rowwld <- popproj_rowwld %>% 
  mutate(ROW = ifelse(is.na(ROW_incomplete), ROW_fit, ROW_incomplete))

# Visualise fit and prediction
popproj_rowwld %>% 
  pivot_longer(-year) %>% 
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line(alpha = 0.5)

# Complete population data
pop_analysis <- pophist_analysis %>% 
  filter(iso3c != "ROW") %>% 
  rbind(popproj %>% select(iso3c, year, pop) %>% filter(iso3c %in% pophist_analysis$iso3c)) %>% 
  rbind(popproj_rowwld %>% transmute(iso3c = "ROW", year = year, pop = ROW)) %>% 
  arrange(iso3c, year)

# Filter emiss_gtco2 dataset to analysis iso3c vector and add rest-of-world totals
emiss_gtco2_row <- emiss_gtco2 %>% 
  filter(iso3c %in% final_iso3c$iso3c) %>%
  group_by(year) %>% 
  summarise(across(matches("GtCO2"), sum)) %>% 
  left_join(
    emiss_gtco2 %>% 
      filter(iso3c == "WLD") %>%
      group_by(year) %>% 
      summarise(across(matches("GtCO2"), sum, .names = "{.col}_world")) 
  ) %>% 
  transmute(
    year = year,
    terr_GtCO2FFI = terr_GtCO2FFI_world - terr_GtCO2FFI, 
    lulucf_GtCO2_blue = lulucf_GtCO2_blue_world - lulucf_GtCO2_blue, 
    lulucf_GtCO2_hc2023 = lulucf_GtCO2_hc2023_world - lulucf_GtCO2_hc2023, 
    lulucf_GtCO2_oscar = lulucf_GtCO2_oscar_world - lulucf_GtCO2_oscar)

emiss_gtco2_analysis <- emiss_gtco2 %>% 
  filter(iso3c %in% final_iso3c$iso3c) %>% 
  bind_rows(emiss_gtco2_row %>% mutate(iso3c = "ROW"))

# Check that the rest-of-world total is correct 
emiss_gtco2_analysis %>% 
  summarise(across(terr_GtCO2FFI:lulucf_GtCO2_oscar, sum)) - 
  emiss_gtco2 %>% 
  filter(iso3c == "WLD") %>% 
  summarise(across(terr_GtCO2FFI:lulucf_GtCO2_oscar, sum))

# Filter all datasets to analysis iso3c vector and collapse into named list
analysis_datasets <- 
  list(pop_analysis = pop_analysis,
       emiss_gtco2_analysis = emiss_gtco2_analysis,
       recent_gdpmer_analysis = recent_gdpmer_analysis,
       recent_gdpppp_analysis = recent_gdpppp_analysis) %>% 
  reduce(left_join) %>% 
  mutate(lulucf_GtCO2_mean = rowMeans(select(., matches("lulucf_GtCO2")))) %>% 
  left_join(final_iso3c %>% select(iso3c, r10)) %>%
  select(r10, iso3c, year, pop, gdpcurrmer, gdpcurrppp, terr_GtCO2FFI, lulucf_GtCO2_mean, everything()) %>% 
  arrange(r10, iso3c, year)

# Save as excel sheet
analysis_datasets %>% write_csv(here("Data", "processed", "02_iso3c_indicators.csv"))

# SET REMAINING CARBON BUDGETS -------------------------------------------------

# From 2020 with a temperature target of 1.5C with a likelihood of 50%, using the
# updated 2023 value (247Gt) and adding all CO2 emissions from 2020-2022 (117Gt) 
# from Lamboll et al (2023) https://doi.org/10.1038/s41558-023-01848-5.
rcb2020_nz = 247 + 117

# Subtract estimated international bunker emissions from 2020 to 2050 under
# SSP1-19 and SSP1-26 (46 GtCO2). This is conservative and likely too low, 
# thus likely giving an upper bound available RCB for allocation.
rcb2020_nz <- rcb2020_nz - 46

# From 2015 to net zero, adding CO2-FFI emissions only from 2015-2019,
# from Friedlingstein et al (2023), https://essd.copernicus.org/articles/15/5301/2023/.  
rcb2015_nz = rcb2020_nz + emiss_gtco2 %>% 
                                  filter(year %in% 2015:2019, iso3c == "WLD") %>% 
                                  summarise(terr_GtCO2FFI = sum(terr_GtCO2FFI)) %>% pull()

# From 1990 to net zero, adding CO2-FFI emissions only from 1990-2019,
# from Friedlingstein et al (2023), https://essd.copernicus.org/articles/15/5301/2023/.  
rcb1990_nz = rcb2020_nz + emiss_gtco2 %>% 
                                  filter(year %in% 1990:2019, iso3c == "WLD") %>% 
                                  summarise(terr_GtCO2FFI = sum(terr_GtCO2FFI)) %>% pull()

# From 1850 to net zero, adding CO2-FFI emissions only from 1850-1989,
# from Friedlingstein et al (2023), https://essd.copernicus.org/articles/15/5301/2023/.  
rcb1850_nz = rcb1990_nz + emiss_gtco2 %>% 
                                  filter(year %in% 1850:1989, iso3c == "WLD") %>% 
                                  summarise(terr_GtCO2FFI = sum(terr_GtCO2FFI)) %>% pull()

# (Intermediate) from 1850 to 1989 
# from Friedlingstein et al (2023), https://essd.copernicus.org/articles/15/5301/2023/.  
rcb1850_1989 = rcb1850_nz - rcb1990_nz

# Write these to file for later use
write_csv(tibble(rcb = c("rcb2020_nz", "rcb2015_nz", "rcb1990_nz", "rcb1850_nz", "rcb1850_1989"),
                 terr_GtCO2FFI = c(rcb2020_nz, rcb2015_nz, rcb1990_nz, rcb1850_nz, rcb1850_1989)) %>% 
            mutate(terr_GtCO2FFI = round(terr_GtCO2FFI,2)),
          here("Data", "processed", "03_rcbquantities.csv"))

# DEFINE SCALING PENALTY FUNCTIONS ---------------------------------------------

# Set up possible penalty functions to apply
penaltyfunc1 <- function(x) {1 / x}
penaltyfunc2 <- function(x) {1 / sqrt(x)}
penaltyfunc3 <- function(x) {1 / asinh(x)}

# ALLOCATIONS OVER TIME (1990-2020) --------------------------------------------

# Create dummy data frame
rcb19902020 <- tibble(.rows = 0)

# Prepare analysis dataset
iso3c_analysis <- analysis_datasets %>% 
  group_by(iso3c) %>% 
  summarise(pop_18502050 = sum(pop, na.rm = T),
            pop_18501989 = sum(ifelse(year <= 1989, pop, NA_real_), na.rm = T),
            pop_19902050 = sum(ifelse(year >= 1990 & year <= 2050, pop, NA_real_), na.rm = T),
            pop_20152050 = sum(ifelse(year >= 2015 & year <= 2050, pop, NA_real_), na.rm = T),
            gtco2_cmltv18501989 = sum(ifelse(year >= 1850 & year <= 1989, terr_GtCO2FFI, NA_real_), na.rm = T))

# Loop over years 1990-2020, determining the RCB at each year
for(curr_year in 1990:2020) {
  
  iso3_analysis_loop <- 
    left_join(
      iso3c_analysis,
      analysis_datasets %>% 
        group_by(r10, iso3c) %>% 
        mutate(
          gtco2_cmltv1990 = cumsum(ifelse(year >= 1990, terr_GtCO2FFI, 0)),
          gtco2_cmltv2015 = cumsum(ifelse(year >= 2015, terr_GtCO2FFI, 0))) %>%
        ungroup() %>% 
        filter(year == curr_year - 1) %>% 
        select(iso3c, gtco2_cmltv1990, gtco2_cmltv2015)) %>% 
    left_join(
      analysis_datasets %>% 
        group_by(iso3c) %>% 
        summarise(cmltv_pop1990_curryear = 
                    sum(ifelse(year >= 1989 & year < curr_year, pop, NA_real_), na.rm = TRUE))) %>% 
    left_join(
      analysis_datasets %>% 
        group_by(iso3c) %>% 
        summarise(cmltv_gdpcurrppp1990_curryear = 
                    sum(ifelse(year >= 1989 & year < curr_year, gdpcurrppp, NA_real_), na.rm = TRUE))) %>% 
    left_join(
      analysis_datasets %>% 
        group_by(iso3c) %>% 
        summarise(cmltv_gdpcurrmer1990_curryear =
                    sum(ifelse(year >= 1989 & year < curr_year, gdpcurrmer, NA_real_), na.rm = TRUE))) %>% 
    mutate(
      cmltv_gdpcurrppp1990_curryear = ifelse(cmltv_gdpcurrppp1990_curryear == 0, NA_real_, cmltv_gdpcurrppp1990_curryear),
      cmltv_gdpcurrmer1990_curryear = ifelse(cmltv_gdpcurrmer1990_curryear == 0, NA_real_, cmltv_gdpcurrmer1990_curryear),
      cmltv_gdpcurrppp1990_curryear_pc = cmltv_gdpcurrppp1990_curryear / cmltv_pop1990_curryear,
      cmltv_gdpcurrmer1990_curryear_pc = cmltv_gdpcurrmer1990_curryear / cmltv_pop1990_curryear) %>% 
    select(iso3c, pop_20152050, pop_19902050, pop_18502050,  pop_18501989, gtco2_cmltv18501989, 
           gtco2_cmltv1990, gtco2_cmltv2015, cmltv_gdpcurrppp1990_curryear_pc, cmltv_gdpcurrmer1990_curryear_pc) 
  
  iso3_analysis_loop <- iso3_analysis_loop %>% 
    mutate(
      # ECPC 1850-2050
      ecpc_pp1850_tco2 = (rcb1850_nz * 1e9) / sum(pop_18502050),
      # ECPC 1850-1989
      ecpc_pp18501989_tco2 = (rcb1850_1989 * 1e9) / sum(pop_18501989),
      # ECPC 1990-2050
      ecpc_pp1990_tco2 = (rcb1990_nz * 1e9) / sum(pop_19902050),
      # ECPC 2015-2050
      ecpc_pp2015_tco2 = (rcb2015_nz * 1e9) / sum(pop_20152050),
      # ECPC 1990-2050, scaled using cumulative GDP per cumulative capita 1990-curr_year (PPP)
      ecpc_pp1990_atp_ppp_pf1 = 
        (penaltyfunc1(cmltv_gdpcurrppp1990_curryear_pc) * (rcb1990_nz * 1e9)) /
        sum(penaltyfunc1(cmltv_gdpcurrppp1990_curryear_pc) * pop_19902050, na.rm = T),
      ecpc_pp1990_atp_ppp_pf2 = 
        (penaltyfunc2(cmltv_gdpcurrppp1990_curryear_pc) * (rcb1990_nz * 1e9)) /
        sum(penaltyfunc2(cmltv_gdpcurrppp1990_curryear_pc) * pop_19902050, na.rm = T),
      ecpc_pp1990_atp_ppp_pf3 = 
        (penaltyfunc3(cmltv_gdpcurrppp1990_curryear_pc) * (rcb1990_nz * 1e9)) /
        sum(penaltyfunc3(cmltv_gdpcurrppp1990_curryear_pc) * pop_19902050, na.rm = T),
      # ECPC 1990-2050, scaled using cumulative GDP per cumulative capita 1990-curr_year (MER)
      ecpc_pp1990_atp_mer_pf1 = 
        (penaltyfunc1(cmltv_gdpcurrmer1990_curryear_pc) * (rcb1990_nz * 1e9)) /
        sum(penaltyfunc1(cmltv_gdpcurrmer1990_curryear_pc) * pop_19902050, na.rm = T),
      ecpc_pp1990_atp_mer_pf2 = 
        (penaltyfunc2(cmltv_gdpcurrmer1990_curryear_pc) * (rcb1990_nz * 1e9)) /
        sum(penaltyfunc2(cmltv_gdpcurrmer1990_curryear_pc) * pop_19902050, na.rm = T),
      ecpc_pp1990_atp_mer_pf3 = 
        (penaltyfunc3(cmltv_gdpcurrmer1990_curryear_pc) * (rcb1990_nz * 1e9)) /
        sum(penaltyfunc3(cmltv_gdpcurrmer1990_curryear_pc) * pop_19902050, na.rm = T),
      # ECPC 2015-2050, scaled using cumulative GDP per cumulative capita 2015-curr_year (PPP)
      ecpc_pp2015_atp_ppp_pf1 = 
        (penaltyfunc1(cmltv_gdpcurrppp1990_curryear_pc) * (rcb2015_nz * 1e9)) /
        sum(penaltyfunc1(cmltv_gdpcurrppp1990_curryear_pc) * pop_20152050, na.rm = T),
      ecpc_pp2015_atp_ppp_pf2 = 
        (penaltyfunc2(cmltv_gdpcurrppp1990_curryear_pc) * (rcb2015_nz * 1e9)) /
        sum(penaltyfunc2(cmltv_gdpcurrppp1990_curryear_pc) * pop_20152050, na.rm = T),
      ecpc_pp2015_atp_ppp_pf3 =
        (penaltyfunc3(cmltv_gdpcurrppp1990_curryear_pc) * (rcb2015_nz * 1e9)) /
        sum(penaltyfunc3(cmltv_gdpcurrppp1990_curryear_pc) * pop_20152050, na.rm = T),
      # ECPC 2015-2050, scaled using cumulative GDP per cumulative capita 2015-curr_year (MER)
      ecpc_pp2015_atp_mer_pf1 = 
        (penaltyfunc1(cmltv_gdpcurrmer1990_curryear_pc) * (rcb2015_nz * 1e9)) /
        sum(penaltyfunc1(cmltv_gdpcurrmer1990_curryear_pc) * pop_20152050, na.rm = T),
      ecpc_pp2015_atp_mer_pf2 =
        (penaltyfunc2(cmltv_gdpcurrmer1990_curryear_pc) * (rcb2015_nz * 1e9)) /
        sum(penaltyfunc2(cmltv_gdpcurrmer1990_curryear_pc) * pop_20152050, na.rm = T),
      ecpc_pp2015_atp_mer_pf3 =
        (penaltyfunc3(cmltv_gdpcurrmer1990_curryear_pc) * (rcb2015_nz * 1e9)) /
        sum(penaltyfunc3(cmltv_gdpcurrmer1990_curryear_pc) * pop_20152050, na.rm = T))
  
  iso3_analysis_loop <- iso3_analysis_loop %>% 
    transmute(iso3c = iso3c,
              year = curr_year,
              "pp2015" = (ecpc_pp2015_tco2 * pop_20152050) / 1e9 - gtco2_cmltv2015,
              "pp1990" = (ecpc_pp1990_tco2 * pop_19902050) / 1e9 - gtco2_cmltv1990,
              "pp1850" = (ecpc_pp1850_tco2 * pop_18502050) / 1e9 - gtco2_cmltv18501989 - gtco2_cmltv1990,
              
              "pp2015_atp_ppp_pf1" = (ecpc_pp2015_atp_ppp_pf1 * pop_20152050) / 1e9 - gtco2_cmltv2015,
              "pp2015_atp_ppp_pf2" = (ecpc_pp2015_atp_ppp_pf2 * pop_20152050) / 1e9 - gtco2_cmltv2015,
              "pp2015_atp_ppp_pf3" = (ecpc_pp2015_atp_ppp_pf3 * pop_20152050) / 1e9 - gtco2_cmltv2015,
              
              "pp2015_atp_mer_pf1" = (ecpc_pp2015_atp_mer_pf1 * pop_20152050) / 1e9 - gtco2_cmltv2015,
              "pp2015_atp_mer_pf2" = (ecpc_pp2015_atp_mer_pf2 * pop_20152050) / 1e9 - gtco2_cmltv2015,
              "pp2015_atp_mer_pf3" = (ecpc_pp2015_atp_mer_pf3 * pop_20152050) / 1e9 - gtco2_cmltv2015,
              
              "pp1990_atp_ppp_pf1" = (ecpc_pp1990_atp_ppp_pf1 * pop_19902050) / 1e9 - gtco2_cmltv1990,
              "pp1990_atp_ppp_pf2" = (ecpc_pp1990_atp_ppp_pf2 * pop_19902050) / 1e9 - gtco2_cmltv1990,
              "pp1990_atp_ppp_pf3" = (ecpc_pp1990_atp_ppp_pf3 * pop_19902050) / 1e9 - gtco2_cmltv1990,
              
              "pp1990_atp_mer_pf1" = (ecpc_pp1990_atp_mer_pf1 * pop_19902050) / 1e9 - gtco2_cmltv1990,
              "pp1990_atp_mer_pf2" = (ecpc_pp1990_atp_mer_pf2 * pop_19902050) / 1e9 - gtco2_cmltv1990,
              "pp1990_atp_mer_pf3" = (ecpc_pp1990_atp_mer_pf3 * pop_19902050) / 1e9 - gtco2_cmltv1990,
              
              "pp1850_atp_ppp_pf1" = ((ecpc_pp18501989_tco2 * pop_18501989) + 
                                        (ecpc_pp1990_atp_ppp_pf1 * pop_19902050)) / 1e9 - gtco2_cmltv18501989 - gtco2_cmltv1990,
              "pp1850_atp_ppp_pf2" = ((ecpc_pp18501989_tco2 * pop_18501989) + 
                                        (ecpc_pp1990_atp_ppp_pf2 * pop_19902050)) / 1e9 - gtco2_cmltv18501989 - gtco2_cmltv1990,
              "pp1850_atp_ppp_pf3" = ((ecpc_pp18501989_tco2 * pop_18501989) + 
                                        (ecpc_pp1990_atp_ppp_pf3 * pop_19902050)) / 1e9 - gtco2_cmltv18501989 - gtco2_cmltv1990,
              "pp1850_atp_mer_pf1" = ((ecpc_pp18501989_tco2 * pop_18501989) + 
                                        (ecpc_pp1990_atp_mer_pf1 * pop_19902050)) / 1e9 - gtco2_cmltv18501989 - gtco2_cmltv1990,
              "pp1850_atp_mer_pf2" = ((ecpc_pp18501989_tco2 * pop_18501989) + 
                                        (ecpc_pp1990_atp_mer_pf2 * pop_19902050)) / 1e9 - gtco2_cmltv18501989 - gtco2_cmltv1990,
              "pp1850_atp_mer_pf3" = ((ecpc_pp18501989_tco2 * pop_18501989) + 
                                        (ecpc_pp1990_atp_mer_pf3 * pop_19902050)) / 1e9 - gtco2_cmltv18501989 - gtco2_cmltv1990)
  
  rcb19902020 <- rbind(rcb19902020, iso3_analysis_loop)
  
}

# Check allocations
alloc_check_years <- rcb19902020 %>% 
  group_by(year) %>% 
  summarise(across(-c(iso3c), ~round(sum(.,na.rm = T), 2)))

# Write country level budgets to file
rcb19902020 %>% 
  left_join(analysis_datasets %>% select(iso3c, year, terr_GtCO2FFI)) %>% 
  select(iso3c, year, terr_GtCO2FFI, everything()) %>% 
  arrange(iso3c) %>% 
  write_csv(here("Data", "processed", "04_iso3c_rcb19902020gtco2ffi.csv"))

# AGGREGATE TO R10 LEVEL --------------------------------------------------------

# Calculate population from year to 2050 for every year after 1990
pop_rem <- tibble(.rows = 0)

for (i in 1990:2020) {
  
  pop_rem_loop <- analysis_datasets %>% 
    group_by(iso3c) %>% 
    filter(year >= i & year <= 2050) %>% 
    summarise(pop_yearto2050 = sum(pop, na.rm = T)) %>% 
    mutate(year = i)
  
  pop_rem <- rbind(pop_rem, pop_rem_loop)
  
}

r10_rcb19902020 <- rcb19902020 %>% 
  pivot_longer(-c(iso3c, year), values_to = "rcb") %>% 
  left_join(analysis_datasets %>% 
              select(year, r10, iso3c, pop) %>% 
              filter(year >= 1991, year <= 2050) %>% 
              group_by(r10, iso3c) %>% 
              summarise(pop19912050 = sum(pop)), by = c("iso3c")) %>% 
  left_join(analysis_datasets %>% 
              select(year, iso3c, pop) %>% 
              filter(year >= 1850, year <= 1990) %>% 
              group_by(iso3c) %>% 
              summarise(pop18501990 = sum(pop)), by = c("iso3c")) %>% 
  left_join(pop_rem) %>% 
  left_join(analysis_datasets %>% select(iso3c, year, terr_GtCO2FFI)) %>% 
  group_by(r10, year, name) %>% 
  summarise(terr_GtCO2FFI = sum(terr_GtCO2FFI, na.rm = T),
            rcb = sum(rcb, na.rm = T),
            pop18501990 = sum(pop18501990, na.rm = T),
            pop_yearto2050 = sum(pop_yearto2050, na.rm = T)) %>% 
  mutate(terr_GtCO2FFI = ifelse(terr_GtCO2FFI == 0, NA_real_, terr_GtCO2FFI)) %>% 
  ungroup() %>% 
  mutate(
    rcb_pc = case_when(
      grepl(name, pattern = "2015") ~ ifelse(year >= 2015, rcb * 1e9 / pop_yearto2050, NA_real_),
      grepl(name, pattern = "1990") ~ ifelse(year >=1990, rcb * 1e9 / pop_yearto2050, NA_real_),
      grepl(name, pattern = "1850") ~ ifelse(year >= 1850, rcb * 1e9 / pop_yearto2050, NA_real_)),
    allocation = case_when(
      name == "pp2015" ~ "PP2015",
      name == "pp1990" ~ "PP1990",
      name == "pp1850" ~ "PP1850",
      grepl(name, pattern = "atp") & grepl(name, pattern = "pp1850") ~ "PP1850adjATP",
      grepl(name, pattern = "atp") & grepl(name, pattern = "pp1990") ~ "PP1990adjATP",
      grepl(name, pattern = "atp") & grepl(name, pattern = "pp2015") ~ "PP2015adjATP"),
    allocation = factor(allocation, levels = c("PP2015", "PP1990", "PP1850", "PP2015adjATP",
                                           "PP1990adjATP", "PP1850adjATP")),
    pf = case_when(
      grepl(name, pattern = "pf1") ~ "1/x",
      grepl(name, pattern = "pf2") ~ "1/sqrt(x)",
      grepl(name, pattern = "pf3") ~ "1/asinh(x)",
      TRUE ~ NA_character_),
    ppp = case_when(
      grepl(name, pattern = "ppp") ~ "PPP",
      grepl(name, pattern = "mer") ~ "MER",
      TRUE ~ NA_character_),
    ppp = factor(ppp, levels = c("PPP", "MER", NA_character_)),
    ppp_pf = paste0(ppp, "_", pf),
    ppp_pf = ifelse(ppp_pf == "NA_NA", NA_character_, ppp_pf)) %>% 
  arrange(allocation, pf, ppp) %>% 
  mutate(r10 = ifelse(is.na(r10), "R10ROW", r10))

r10_rcb19902020 %>% 
  filter(!grepl(name, pattern = "2015"), r10 != "R10ROW") %>% 
  mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label)) %>% 
  ggplot(aes(x = year, y = rcb_pc, group = allocation)) +
  geom_point(aes(shape = allocation, colour = ppp_pf),
             data = . %>% filter(grepl(allocation, pattern = "ATP")),
             alpha = 0.7) +
  geom_path(aes(linetype = allocation), linewidth = 0.8,
            data = . %>% filter(!grepl(allocation, pattern = "ATP")), 
            colour = "black",) +
  scale_colour_brewer(type = "div", palette = "Spectral", drop = T) +
  scale_shape(drop = T) +
  scale_linetype(drop = T) +
  facet_wrap(~r10, ncol = 5) +
  theme_bw() +
  labs(x = NULL, y = "tCO\U2082 cap\U207B\U00B9 yr\U207B\U00B9", shape = "Adjusted allocations", 
       colour = "Adjustments", linetype = "Unadjusted allocations",
       subtitle = "Remaining carbon budgets to 2050, at the beginning of each year") +
  guides(colour = guide_legend(order = 3),
         shape = guide_legend(order = 2),
         linetype = guide_legend(order = 1)) +
  plot_annotation(caption = paste0(paste0(r10order$r10label[1:5], ": ",r10order$r10labellong[1:5], collapse = ", "), "\n", paste0(r10order$r10label[6:10], ": ",r10order$r10labellong[6:10], collapse = ", "), collapse = ""))

ggsave(here("Manuscript", "Figures", "SI", "SI_r10_rcb19902020pc.png"),
       height = 8, width = 14)

r10_rcb19902020 %>% 
  filter(!grepl(name, pattern = "2015"), r10 != "R10ROW") %>% 
  mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label)) %>% 
  filter(rcb_pc != 0) %>% 
  ggplot(aes(x = year, y = rcb, group = allocation)) +
  geom_point(aes(shape = allocation, colour = ppp_pf),
             data = . %>% filter(grepl(allocation, pattern = "ATP")),
             alpha = 0.7) +
  geom_path(aes(linetype = allocation), linewidth = 0.8,
            data = . %>% filter(!grepl(allocation, pattern = "ATP")), 
            colour = "black",) +
  scale_colour_brewer(type = "div", palette = "Spectral", drop = T) +
  scale_shape(drop = T) +
  scale_linetype(drop = T) +
  facet_wrap(~r10, ncol = 5) +
  theme_bw() +
  labs(x = NULL, y = "GtCO\U2082", shape = "Adjusted allocations", 
       colour = "Adjustments", linetype = "Unadjusted allocations",
       subtitle = "Remaining carbon budgets to 2050, at the beginning of each year") +
  guides(colour = guide_legend(order = 3),
         shape = guide_legend(order = 2),
         linetype = guide_legend(order = 1)) +
  plot_annotation(caption = paste0(paste0(r10order$r10label[1:5], ": ",r10order$r10labellong[1:5], collapse = ", "), "\n", paste0(r10order$r10label[6:10], ": ",r10order$r10labellong[6:10], collapse = ", "), collapse = ""))

ggsave(here("Manuscript", "Figures", "SI", "SI_r10_rcb19902020.png"),
       height = 8, width = 14)

# WRITE TO FILE ----------------------------------------------------------------

r10_rcb19902020 %>% 
  arrange(r10, allocation, ppp_pf, year) %>% 
  select(r10, allocation, ppp, pf, ppp_pf, name, year, rcb, pop_yearto2050) %>% 
  write_csv(here("Data", "processed", "05_r10_rcb19902020gtco2ffi.csv"))

