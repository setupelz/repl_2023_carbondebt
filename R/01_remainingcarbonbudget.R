# Replication archive for: "Delaying Carbon Debt Drawdown Fails Younger Generations"

# Contact for clarifications: [ANONYMISED]       

# Script contents: Determine remaining carbon budgets from the beginning of 2023.

# LOAD PACKAGES ----------------------------------------------------------------

#install.packages("pacman")
library(pacman)

# processing
p_load(dplyr, tidyr, readr, readxl, writexl, purrr, ggplot2, forcats, stringr, 
       patchwork)

# misc
p_load(here, countrycode, zoo)

# options
options(scipen = 999)

# COUNTRY NAMES AND REGIONAL GROUPING ------------------------------------------

# Determine country-years for analysis
iso3c_tbl <- read_csv(here("Data", "countrygroups", "iso3c_region_mapping.csv")) %>% 
  mutate(r10 = ifelse(is.na(r10_iamc), NA_real_, r10_unif)) %>% 
  select(iso3c, r10) %>% 
  group_by(iso3c, r10) %>% 
  expand(year = 1850:2050) %>% 
  ungroup()

# Set consistent r10 ordering
r10order <- tibble(r10 = c("R10NAM", "R10EUR", "R10PAO", "R10FSU", "R10EASPAS", "R10LAM", "R10AFRMEA", "R10SAS"),
                   r10label = c("NAM", "EUR", "APD", "EEA", "EASPAS", "LAC", "AFRMEA", "SAS"),
                   r10labellong = c("North America", "Europe", "Asia-Pacific Developed",
                               "Eastern Europe and West-Central Asia", "Africa & Middle East", 
                               "Eastern & South-East Asia and developing Pacific",
                               "Latin America and Caribbean", "Southern Asia"))

# Adjust labels to reflect those for publication
iso3c_tbl <- iso3c_tbl %>% 
  left_join(r10order) %>% 
  select(iso3c, r10, r10label, r10labellong, year)

# HISTORICAL CUMULATIVE EMISSIONS GCP (1850-1989) ------------------------------

# Historical production-based CO2-FFI emissions - Global Carbon Budget 2023
# https://zenodo.org/records/10177738
hist_prodco2 <- read_csv(here("data", "equity_data",
                               "GCB2023v36_MtCO2_flat.csv")) %>% 
  # Data provided in million tonnes of CO2 per year, convert to GtCO2
  transmute(iso3c = `ISO 3166-1 alpha-3`, year = Year, CO2 = Total * 1e-3) %>% 
  right_join(iso3c_tbl %>% filter(year >= 1850, year <= 1989), 
                              by = c("iso3c", "year")) %>% 
  filter(year >= 1850, year <= 1989)

# Check which iso3c are missing in GCB emissions data
miss_hist_prodco2 <- hist_prodco2 %>% 
  group_by(iso3c) %>% 
  summarise(n_total = n(),
            n_missing = sum(is.na(CO2) | is.na(r10)))

# Determine cumulative emissions 1850-1989
hist_prodco2 <- hist_prodco2 %>% 
  filter(!iso3c %in% 
           (miss_hist_prodco2 %>% filter(n_total == n_missing) %>% pull(iso3c))) %>% 
  group_by(iso3c) %>% 
  summarise(gtco2_18501989 = 
              sum(ifelse(year >= 1850 & year <= 1989, CO2, NA_real_), na.rm = T)) %>% 
  left_join(iso3c_tbl %>% distinct(iso3c, r10)) %>% 
  ungroup() %>% 
  select(r10, iso3c, gtco2_18501989)

# RECENT CUMULATIVE EMISSIONS (GCP) (1990-2022) --------------------------------

# Recent production-based CO2-FFI emissions - Global Carbon Budget 2022
# https://www.icos-cp.eu/science-and-impact/global-carbon-budget/2022
recent_prodco2 <- read_csv(here("data", "equity_data",
                                "GCB2023v36_MtCO2_flat.csv")) %>% 
  # Data provided in million tonnes of CO2 per year, convert to GtCO2
  transmute(iso3c = `ISO 3166-1 alpha-3`, year = Year, gtco2 = Total * 1e-3) %>% 
  right_join(iso3c_tbl %>% filter(year >= 1990, year <= 2022), 
                              by = c("iso3c", "year")) %>% 
  filter(year >= 1990, year <= 2022)

# Check which iso3c are missing in GCB emissions data in each year
miss_recent_prodco2 <- recent_prodco2 %>% 
  group_by(iso3c) %>% 
  summarise(n_total = n(),
            n_missing = sum(is.na(gtco2) | is.na(r10)))

# Determine cumulative emissions 1990-2022
recent_prodco2 <- recent_prodco2 %>% 
  filter(!iso3c %in% 
           (miss_recent_prodco2 %>% filter(n_total == n_missing) %>% pull(iso3c))) %>% 
  select(r10, iso3c, year, gtco2) %>% 
  group_by(iso3c) %>% 
  mutate(gtco2_cmltv = cumsum(gtco2)) %>% 
  ungroup() %>% 
  arrange(r10, iso3c, year)

# GDP PPP 1989-2019 (Penn World Tables) ----------------------------------------

# See Feenstra, Inklaar, & Timmer (2015). https://doi.org/10.1257/aer.20130954
# www.ggdc.net/pwt
# 
recent_gdpppp <- 
  read_xlsx(here("data", "equity_data", "pwt1001.xlsx"), sheet = 3) %>%
  # rgdpo - Output-side real GDP at chained PPPs (in mil. 2017US$)
  select(iso3c = countrycode, year, gdp2017ppp = rgdpo) %>% 
  # convert from WB iso codes to iso 3166
  mutate(iso3c = countrycode(iso3c, origin = "wb", destination = "iso3c")) %>% 
  mutate(across(c(gdp2017ppp), ~ . * 1e6)) %>% 
  filter(year >= 1989, year <= 2019) %>% 
  right_join(iso3c_tbl %>% filter(year >= 1989, year <= 2019), 
             by = c("iso3c", "year"))

# If missing, back-cast (extrapolate) GDP and POP for the year 1989 - this is a 
# crude assumption to ensure all countries have data for their first year of allocation, 
# it's effect is minor at the regional level - this can be tested by commenting this out.
recent_gdpppp <- recent_gdpppp %>% 
  group_by(iso3c, r10) %>% 
  mutate(gdp2017ppp = na.approx(gdp2017ppp, rule = 2, maxgap = 25)) %>% 
  ungroup()

# Check which iso3c are missing in PWT data
miss_recent_gdpppp <- recent_gdpppp %>%
  group_by(iso3c) %>% 
  summarise(n_total = n(),
            n_missing = sum(is.na(gdp2017ppp) | is.na(r10)))

# Remove missing countries
recent_gdpppp <- recent_gdpppp %>% 
  filter(!iso3c %in% (miss_recent_gdpppp %>% filter(n_total == n_missing) %>% pull(iso3c))) %>% 
  arrange(r10, iso3c, year) %>% 
  select(r10, iso3c, year, gdp2017ppp)

# GDP MER 1989-2019 (WDI) ------------------------------------------------------

recent_gdpmer <- 
  read_csv(here("Data", "equity_data", "API_NY.GDP.MKTP.CD_DS2_en_csv_v2_5607117.csv"),
           skip = 3) %>% 
  select(iso3c = `Country Code`, matches("\\d{4}")) %>% 
  pivot_longer(-iso3c, names_to = "year", values_to = "gdpcurrmer") %>% 
  filter(year >= 1989, year <= 2019) %>% 
  mutate(year = as.numeric(year)) %>% 
  right_join(iso3c_tbl %>% filter(year >= 1989, year <= 2019), 
             by = c("iso3c", "year"))

# If missing, back-cast (extrapolate) GDP and POP for the year 1989 - this is a
# crude assumption to ensure all countries have data for their first year of allocation, 
# it's effect is minor at the regional level - this can be tested by commenting this out.
recent_gdpmer <- recent_gdpmer %>% 
  group_by(iso3c, r10) %>% 
  mutate(gdpcurrmer = na.approx(gdpcurrmer, rule = 2, maxgap = 25)) %>% 
  ungroup()

# Check which iso3c are missing in WDI data
miss_recent_gdpmer <- recent_gdpmer %>%
  group_by(iso3c) %>% 
  summarise(n_total = n(),
            n_missing = sum(is.na(gdpcurrmer) | is.na(r10)))

# Remove missing countries
recent_gdpmer <- recent_gdpmer %>% 
  filter(!iso3c %in% (miss_recent_gdpmer %>% filter(n_total == n_missing) %>% pull(iso3c))) %>% 
  arrange(r10, iso3c, year) %>% 
  select(r10, iso3c, year, gdpcurrmer)

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

# Historical population 1850-2020
pophist <- read_csv(here("Data", "equity_data", "population.csv")) %>% 
  select(iso3c = Code, year = Year, pop = `Population (historical estimates)`) %>% 
  filter(year >= 1850, year <= 2020) 

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
  mutate(pop = zoo::na.approx(pop, maxgap = 50))
  
# Determine missing countries
miss_popproj <- popproj %>% 
  group_by(iso3c) %>% 
  summarise(n_total = n(),
            n_missing = sum(is.na(pop) | is.na(r10)))

# Remove missing countries
popproj <- popproj %>%
  filter(!iso3c %in% (miss_popproj %>% filter(n_missing > 0) %>% pull(iso3c))) %>% 
  arrange(r10, iso3c, year) %>% 
  select(r10, iso3c, year, pop)

# DETERMINE SET OF COUNTRIES WITH COMPLETE DATA --------------------------------

# Remove missing data across all input sources and inner join
final_iso3c <- 
  list(hist_prodco2, recent_prodco2, recent_gdpppp, recent_gdpmer, popproj) %>% 
  map(~na.omit(.) %>% distinct(r10, iso3c)) %>% 
  reduce(inner_join)

# Determine which countries were removed from analysis
iso3c_missing <- iso3c_tbl %>% 
  distinct(iso3c, r10) %>% 
  filter(!iso3c %in% final_iso3c$iso3c) %>% 
  arrange(r10, iso3c)

# Determine 2019 emissions attributable to these countries
iso3c_missing <- left_join(iso3c_missing, 
                           (read_csv(here("data", "equity_data",
                                          "GCB2023v36_MtCO2_flat.csv")) %>% 
                              # Data provided in million tonnes of CO2 per year, convert to GtCO2
                              transmute(iso3c = `ISO 3166-1 alpha-3`, year = Year, gtco2 = Total * 1e-3) %>% 
                              filter(year == 2022))) %>% 
  mutate(share = gtco2 / (sum(recent_prodco2$gtco2[recent_prodco2$year == 2022], na.rm = T)),
         share = scales::percent(share, accuracy = .01))

write_csv(iso3c_missing, here("Manuscript", "Tables", "iso3cmiss.csv"))

# FINALISE COMPLETE PROCESSED ANALYSIS DATASETS --------------------------------

# Filter all datasets to analysis iso3c vector and collapse into named list
analyis_datasets <- 
  list(hist_prodco2 = hist_prodco2, recent_prodco2 = recent_prodco2, 
       recent_gdpppp = recent_gdpppp, recent_gdpmer = recent_gdpmer, 
       popproj = popproj) %>% 
  map(., function(tibble) {filter(tibble, iso3c %in% final_iso3c$iso3c)})

# Save as excel sheet
analyis_datasets %>% write_xlsx(here("Data", "processed", "analysisdata.xlsx"))

# SET REMAINING CARBON BUDGETS -------------------------------------------------

# From 2020 with a temperature target of 1.5C with a likelihood of 50%, using the
# updated 2023 value (247Gt) from Lamboll et al (2023) https://doi.org/10.1038/s41558-023-01848-5, 
# and adding CO2-FFI emissions from 2020-2022,
# from Friedlingstein et al (2023), https://doi.org/10.5194/essd-15-5301-2023). 
rcb2020_nz = 247 + round(pull(analyis_datasets$recent_prodco2 %>% filter(year %in% 2020:2022) %>% summarise(gtco2 = sum(gtco2))),2)

# From 1990 to net zero, adding CO2-FFI emissions from 1990-2019,
# from Friedlingstein et al (2023), https://doi.org/10.5194/essd-15-5301-2023).  
rcb1990_nz = rcb2020_nz + round(pull(analyis_datasets$recent_prodco2 %>% filter(year %in% 1990:2019) %>% summarise(gtco2 = sum(gtco2))),2)

# From 1850 to net zero, adding CO2-FFI emissions from 1850-1989,
# from Friedlingstein et al (2023), https://doi.org/10.5194/essd-15-5301-2023).  
rcb1850_nz = rcb1990_nz + round(pull(analyis_datasets$hist_prodco2 %>% summarise(gtco2 = sum(gtco2_18501989))),2)

# (Intermediate) from 1850 to 1989 (calculated from Friedlingstein et al (2023), 
# https://doi.org/10.5194/essd-15-5301-2023).
rcb1850_1989 = rcb1850_nz - rcb1990_nz

# Write these to file for later use
write_csv(tibble(rcb = c("rcb2020_nz", "rcb1990_nz", "rcb1850_nz", "rcb1850_1989"),
                 gtco2 = c(rcb2020_nz, rcb1990_nz, rcb1850_nz, rcb1850_1989)),
          here("Data", "processed", "rcbquantities.csv"))

# DEFINE SCALING PENALTY FUNCTIONS ---------------------------------------------

# Set up possible penalty functions to apply
penaltyfunc1 <- function(x) {1 / x}
penaltyfunc2 <- function(x) {1 / sqrt(x)}
penaltyfunc3 <- function(x) {1 / asinh(x)}

# ALLOCATIONS OVER TIME (1990-2020) --------------------------------------------

# Create dummy data frame
rcb19902020 <- tibble(.rows = 0)

# Prepare analysis dataset
iso3c_analysis <- analyis_datasets$popproj %>% 
      group_by(iso3c) %>% 
      summarise(pop_18502050 = sum(pop, na.rm = T),
                pop_18501989 = sum(ifelse(year <= 1989, pop, NA_real_), na.rm = T),
                pop_19902050 = sum(ifelse(year >= 1990 & year <= 2050, pop, NA_real_), na.rm = T)) %>% 
  ungroup() %>% 
  left_join(analyis_datasets$hist_prodco2 %>% select(iso3c, gtco2_18501989))

# Loop over years 1990-2020, determining the RCB at each year
for(curr_year in 1990:2020) {
  
  iso3_analysis_loop <- 
    left_join(
      iso3c_analysis,
      analyis_datasets$recent_prodco2 %>% 
        filter(year == curr_year - 1) %>% 
        select(iso3c, gtco2_cmltv)) %>% 
    left_join(
      analyis_datasets$popproj %>% 
        summarise(cmltv_pop1990_curryear = 
                    sum(ifelse(year >= 1990 & year <= curr_year, pop, NA_real_), na.rm = TRUE))) %>% 
    left_join(
      analyis_datasets$recent_gdpppp %>% 
        group_by(iso3c) %>% 
        summarise(cmltv_gdp2017ppp = 
                    sum(ifelse(year < curr_year & curr_year >= 1990, gdp2017ppp, NA_real_), na.rm = TRUE))) %>% 
    left_join(
      analyis_datasets$recent_gdpmer %>% 
        group_by(iso3c) %>% 
        summarise(cmltv_gdp2017mer =
                    sum(ifelse(year < curr_year & curr_year >= 1990, gdpcurrmer, NA_real_), na.rm = TRUE))) %>% 
    mutate(
      cmltv_gdp2017ppp = ifelse(cmltv_gdp2017ppp == 0, NA_real_, cmltv_gdp2017ppp),
      cmltv_gdp2017mer = ifelse(cmltv_gdp2017mer == 0, NA_real_, cmltv_gdp2017mer),
      cmltv_gdp2017ppp_pc = cmltv_gdp2017ppp / cmltv_pop1990_curryear,
      cmltv_gdp2017mer_pc = cmltv_gdp2017mer / cmltv_pop1990_curryear) %>% 
    select(iso3c, pop_19902050, pop_18502050,  pop_18501989, gtco2_18501989, 
           gtco2_cmltv, cmltv_gdp2017ppp_pc, cmltv_gdp2017mer_pc) %>% 
    # For the year 1990
    mutate(gtco2_cmltv = ifelse(is.na(gtco2_cmltv), 0, gtco2_cmltv))
  
  iso3_analysis_loop <- iso3_analysis_loop %>% 
    mutate(
      # ECPC 1850-2050
      ecpc_pp1850_tco2 = (rcb1850_nz * 1e9) / sum(pop_18502050),
      # ECPC 1850-1989
      ecpc_pp18501989_tco2 = (rcb1850_1989 * 1e9) / sum(pop_18501989),
      # ECPC 1990-2050
      ecpc_pp1990_tco2 = (rcb1990_nz * 1e9) / sum(pop_19902050),
      # ECPC 1990-2050, scaled using cumulative GDP per cumulative capita 1990-curr_year (PPP)
      ecpc_pp1990_atp_ppp_pf1 = 
        (penaltyfunc1(cmltv_gdp2017ppp_pc) * (rcb1990_nz * 1e9)) /
        sum(penaltyfunc1(cmltv_gdp2017ppp_pc) * pop_19902050, na.rm = T),
      ecpc_pp1990_atp_ppp_pf2 = 
        (penaltyfunc2(cmltv_gdp2017ppp_pc) * (rcb1990_nz * 1e9)) /
        sum(penaltyfunc2(cmltv_gdp2017ppp_pc) * pop_19902050, na.rm = T),
      ecpc_pp1990_atp_ppp_pf3 = 
        (penaltyfunc3(cmltv_gdp2017ppp_pc) * (rcb1990_nz * 1e9)) /
        sum(penaltyfunc3(cmltv_gdp2017ppp_pc) * pop_19902050, na.rm = T),
      # ECPC 1990-2050, scaled using cumulative GDP per cumulative capita 1990-curr_year (MER)
      ecpc_pp1990_atp_mer_pf1 = 
        (penaltyfunc1(cmltv_gdp2017mer_pc) * (rcb1990_nz * 1e9)) /
        sum(penaltyfunc1(cmltv_gdp2017mer_pc) * pop_19902050, na.rm = T),
      ecpc_pp1990_atp_mer_pf2 = 
        (penaltyfunc2(cmltv_gdp2017mer_pc) * (rcb1990_nz * 1e9)) /
        sum(penaltyfunc2(cmltv_gdp2017mer_pc) * pop_19902050, na.rm = T),
      ecpc_pp1990_atp_mer_pf3 = 
        (penaltyfunc3(cmltv_gdp2017mer_pc) * (rcb1990_nz * 1e9)) /
        sum(penaltyfunc3(cmltv_gdp2017mer_pc) * pop_19902050, na.rm = T))
  
  iso3_analysis_loop <- iso3_analysis_loop %>% 
    transmute(iso3c = iso3c,
              year = curr_year,
              "pp1990" = (ecpc_pp1990_tco2 * pop_19902050) / 1e9 - gtco2_cmltv,
              "pp1850" = (ecpc_pp1850_tco2 * pop_18502050) / 1e9 - gtco2_18501989 - gtco2_cmltv,
              "pp1990_atp_ppp_pf1" = (ecpc_pp1990_atp_ppp_pf1 * pop_19902050) / 1e9 - gtco2_cmltv,
              "pp1990_atp_ppp_pf2" = (ecpc_pp1990_atp_ppp_pf2 * pop_19902050) / 1e9 - gtco2_cmltv,
              "pp1990_atp_ppp_pf3" = (ecpc_pp1990_atp_ppp_pf3 * pop_19902050) / 1e9 - gtco2_cmltv,
              "pp1990_atp_mer_pf1" = (ecpc_pp1990_atp_mer_pf1 * pop_19902050) / 1e9 - gtco2_cmltv,
              "pp1990_atp_mer_pf2" = (ecpc_pp1990_atp_mer_pf2 * pop_19902050) / 1e9 - gtco2_cmltv,
              "pp1990_atp_mer_pf3" = (ecpc_pp1990_atp_mer_pf3 * pop_19902050) / 1e9 - gtco2_cmltv,
              "pp1850_atp_ppp_pf1" = ((ecpc_pp18501989_tco2 * pop_18501989) + 
                                        (ecpc_pp1990_atp_ppp_pf1 * pop_19902050)) / 1e9 - gtco2_18501989 - gtco2_cmltv,
              "pp1850_atp_ppp_pf2" = ((ecpc_pp18501989_tco2 * pop_18501989) + 
                                        (ecpc_pp1990_atp_ppp_pf2 * pop_19902050)) / 1e9 - gtco2_18501989 - gtco2_cmltv,
              "pp1850_atp_ppp_pf3" = ((ecpc_pp18501989_tco2 * pop_18501989) + 
                                        (ecpc_pp1990_atp_ppp_pf3 * pop_19902050)) / 1e9 - gtco2_18501989 - gtco2_cmltv,
              "pp1850_atp_mer_pf1" = ((ecpc_pp18501989_tco2 * pop_18501989) + 
                                        (ecpc_pp1990_atp_mer_pf1 * pop_19902050)) / 1e9 - gtco2_18501989 - gtco2_cmltv,
              "pp1850_atp_mer_pf2" = ((ecpc_pp18501989_tco2 * pop_18501989) + 
                                        (ecpc_pp1990_atp_mer_pf2 * pop_19902050)) / 1e9 - gtco2_18501989 - gtco2_cmltv,
              "pp1850_atp_mer_pf3" = ((ecpc_pp18501989_tco2 * pop_18501989) + 
                                        (ecpc_pp1990_atp_mer_pf3 * pop_19902050)) / 1e9 - gtco2_18501989 - gtco2_cmltv)
  
  rcb19902020 <- rbind(rcb19902020, iso3_analysis_loop)
  
}

# Check allocations
alloc_check_years <- rcb19902020 %>% 
  group_by(year) %>% 
  summarise(across(-c(iso3c), ~round(sum(.,na.rm = T), 2)))

# Write country level budgets to file
rcb19902020 %>% 
  left_join(analyis_datasets$recent_prodco2 %>% select(iso3c, year, gtco2 = gtco2)) %>% 
  select(iso3c, year, gtco2, everything()) %>% 
  arrange(iso3c) %>% 
  write_csv(here("Data", "processed", "iso3c_rcb19902020gtco2.csv"))

# AGGREGATE TO R10 LEVEL --------------------------------------------------------

# Calculate population from year to 2050 for every year after 1990
pop_rem <- tibble(.rows = 0)

for (i in 1990:2020) {
  
  pop_rem_loop <- analyis_datasets$popproj %>% 
    filter(iso3c %in% iso3c_analysis$iso3c) %>% 
    group_by(iso3c) %>% 
    filter(year >= i & year <= 2050) %>% 
    summarise(pop_yearto2050 = sum(pop, na.rm = T)) %>% 
    mutate(year = i)
  
  pop_rem <- rbind(pop_rem, pop_rem_loop)
  
}

r10_rcb19902020 <- rcb19902020 %>% 
  pivot_longer(-c(iso3c, year), values_to = "rcb") %>% 
  left_join(analyis_datasets$popproj %>% 
              select(year, r10, iso3c, pop) %>% 
              filter(year >= 1990, year <= 2050) %>% 
              group_by(r10, iso3c) %>% 
              summarise(pop19902050 = sum(pop)), by = c("iso3c")) %>% 
  left_join(analyis_datasets$popproj %>% 
              select(year, iso3c, pop) %>% 
              filter(year >= 1850, year < 1990) %>% 
              group_by(iso3c) %>% 
              summarise(pop18501989 = sum(pop)), by = c("iso3c")) %>% 
  left_join(pop_rem) %>% 
  left_join(analyis_datasets$recent_prodco2 %>% select(iso3c, year, gtco2)) %>% 
  group_by(r10, year, name) %>% 
  # Some smaller countries missing data in 1990/1991
  summarise(gtco2 = sum(gtco2, na.rm = T),
            rcb = sum(rcb, na.rm = T),
            pop18501989 = sum(pop18501989, na.rm = T),
            pop_yearto2050 = sum(pop_yearto2050, na.rm = T)) %>% 
  mutate(gtco2 = ifelse(gtco2 == 0, NA_real_, gtco2)) %>% 
  ungroup() %>% 
  mutate(
    rcb_pc = case_when(
      grepl(name, pattern = "1990") ~ rcb * 1e9 / pop_yearto2050,
      grepl(name, pattern = "1850") ~ rcb * 1e9 / pop_yearto2050),
    category = case_when(
      name == "pp1990" ~ "1_PP1990",
      name == "pp1850" ~ "2_PP1850",
      grepl(name, pattern = "atp") & grepl(name, pattern = "pp1850") ~ "4_PP1850adjATP",
      grepl(name, pattern = "atp") ~ "3_PP1990adjATP"),
    category = factor(category, levels = c("1_PP1990", "2_PP1850",
                                           "3_PP1990adjATP", "4_PP1850adjATP")),
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
  arrange(category, pf, ppp)

r10_rcb19902020 %>% 
  select(-name) %>% 
  mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label)) %>% 
  ggplot(aes(x = year, y = rcb_pc, group = category)) +
  geom_point(aes(shape = category, colour = ppp_pf),
             data = . %>% filter(grepl(category, pattern = "ATP")),
             alpha = 0.7) +
  geom_path(aes(linetype = category), linewidth = 0.8,
            data = . %>% filter(!grepl(category, pattern = "ATP")), 
            colour = "black",) +
  scale_colour_brewer(type = "div", palette = "Spectral", drop = T) +
  scale_shape(drop = T) +
  scale_linetype(drop = T) +
  facet_wrap(~r10, ncol = 4) +
  theme_bw() +
  labs(x = NULL, y = "tCO2 / capita / year", shape = "Adjusted allocations", 
       colour = "Adjustments", linetype = "Unadjusted allocations",
       subtitle = "Remaining carbon budgets to 2050, at the beginning of each year") +
  guides(colour = guide_legend(order = 3),
         shape = guide_legend(order = 2),
         linetype = guide_legend(order = 1))

ggsave(here("Manuscript", "Figures", "SI", "SI_r10_rcb19902020pc.png"),
       height = 8, width = 14)

r10_rcb19902020 %>% 
  select(-name) %>% 
  mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label)) %>% 
  filter(rcb_pc != 0) %>% 
  ggplot(aes(x = year, y = rcb, group = category)) +
  geom_point(aes(shape = category, colour = ppp_pf),
             data = . %>% filter(grepl(category, pattern = "ATP")),
             alpha = 0.7) +
  geom_path(aes(linetype = category), linewidth = 0.8,
            data = . %>% filter(!grepl(category, pattern = "ATP")), 
            colour = "black",) +
  scale_colour_brewer(type = "div", palette = "Spectral", drop = T) +
  scale_shape(drop = T) +
  scale_linetype(drop = T) +
  facet_wrap(~r10, ncol = 4) +
  theme_bw() +
  labs(x = NULL, y = "GtCO2", shape = "Adjusted allocations", 
       colour = "Adjustments", linetype = "Unadjusted allocations",
       subtitle = "Remaining carbon budgets to 2050, at the beginning of each year") +
  guides(colour = guide_legend(order = 3),
         shape = guide_legend(order = 2),
         linetype = guide_legend(order = 1))

ggsave(here("Manuscript", "Figures", "SI", "SI_r10_rcb19902020.png"),
       height = 8, width = 14)

# WRITE TO FILE ----------------------------------------------------------------

r10_rcb19902020 %>% 
  arrange(r10, category, ppp_pf, year) %>% 
  select(r10, category, pf, ppp, ppp_pf, year, rcb, pop_yearto2050) %>% 
  write_csv(here("Data", "processed", "r10_rcb19902020.csv"))
