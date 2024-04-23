# Replication archive for: "Delaying Carbon Debt Drawdown Fails Younger Generations"

# Contact for clarifications: [ANONYMISED]       

# Script contents: Determine remaining carbon budgets from the beginning of 2023
# using a variety of different indicators and approaches for the SI.

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

# RELATIVE CONTRIBUTION TO WARMING ---------------------------------------------

gmtresp19922022 <- read_csv(here("Data", "equity_data", "GMST_response_1992-2022.csv")) %>% 
  filter(Component != "Total", Gas != "3-GHG") %>% 
  select(iso3c = ISO3, gas = Gas, component = Component, year = Year, data = Data) %>% 
  inner_join(iso3c_tbl %>% filter(!is.na(r10)) %>% distinct(r10, iso3c)) 

gmtresp19922022 %>% 
  ggplot(aes(x = year, y = data, fill = interaction(gas, component))) +
  geom_col() +
  facet_wrap(~r10, ncol = 4) +
  scale_fill_manual(values = c('#8c510a','#d8b365','#f6e8c3','#c7eae5','#5ab4ac','#01665e')) +
  theme_bw() +
  labs(x = NULL, y = "Change in degrees Celsius since 1992", fill = NULL)

# POPULATION (1992-2022) -------------------------------------------------------

# Future population projection 2025 (IIASA SSP2, used to impute 2022.
popssp2 <- read_csv(here("data", "equity_data", 
                         "SspDb_country_data_2013-06-12.csv")) %>%
  filter(MODEL == "IIASA-WiC POP", SCENARIO %in% c("SSP2_v9_130115"), 
         VARIABLE == "Population") %>% 
  select(iso3c = REGION, `2025`) %>% 
  mutate(across(-c(iso3c), ~ . * 1e6))  %>% 
  pivot_longer(-c(iso3c), names_to = "year", values_to = "pop") %>% 
  mutate(year = as.numeric(year)) 

# Historical population 1992-2021
pophist <- read_csv(here("Data", "equity_data", "population.csv")) %>% 
  select(iso3c = Code, year = Year, pop = `Population (historical estimates)`) %>% 
  filter(year >= 1992, year <= 2021) 

# Interpolate
popproj <- pophist %>%
  filter(iso3c %in% unique(popssp2$iso3c)) %>% 
  rbind(popssp2 %>% 
          filter(iso3c %in% unique(pophist$iso3c))) %>% 
  right_join(iso3c_tbl %>% distinct(iso3c, r10)) %>% 
  arrange(iso3c, r10, year) %>% 
  group_by(iso3c, r10) %>% 
  complete(year = c(1992:2025)) %>% 
  group_by(iso3c) %>% 
  mutate(pop = zoo::na.approx(pop, maxgap = 5)) %>% 
  filter(year %in% 1992:2022)
  
# Keep only those countries present in analysis data
popproj <- popproj %>% 
  filter(iso3c %in% unique(gmtresp$iso3c))

# DETERMINE SET OF COUNTRIES WITH COMPLETE DATA --------------------------------

# Remove missing data across all input sources and inner join
final_iso3c <- 
  list(gmtresp, popproj) %>% 
  map(~na.omit(.) %>% distinct(r10, iso3c)) %>% 
  reduce(inner_join)

# Determine which countries were removed from analysis
iso3c_missing <- iso3c_tbl %>% 
  distinct(iso3c, r10) %>% 
  filter(!iso3c %in% final_iso3c$iso3c) %>% 
  arrange(r10, iso3c)

# Determine 1992-2022 total warming attributable to these countries (if available)
iso3c_missing <- left_join(iso3c_missing, 
                           (read_csv(here("Data", "equity_data", "GMST_response_1992-2022.csv")) %>% 
                              filter(Component == "Total", Gas == "3-GHG") %>% 
                              select(iso3c = ISO3, gas = Gas, component = Component, year = Year, data = Data) %>% 
                              inner_join(iso3c_tbl %>% distinct(r10, iso3c)) %>% 
                              filter(year == 2022) %>% 
                              group_by(r10, iso3c) %>% 
                              summarise(data = sum(data))))

# FINALISE COMPLETE PROCESSED ANALYSIS DATASETS --------------------------------

# Filter all datasets to analysis iso3c vector and collapse into named list
analyis_datasets <- 
  list(gmtresp = gmtresp %>% filter(year == 2022),
       popproj = popproj %>% group_by(r10, iso3c) %>% summarise(pop_19922022 = sum(pop))) %>% 
  map(., function(tibble) {filter(tibble, iso3c %in% final_iso3c$iso3c)})

# SET TOTAL WARMING CONTRIBUTIONS OF ANALYSIS COUNTRIES ------------------------

# Set total warming from warming from 1992 to 2022 across analysis countries
totwarm19922022 = sum(gmtresp[gmtresp$year == 2022,]$data)

# ALLOCATIONS OVER TIME (1990-2020) --------------------------------------------

# Create dummy data frame
warm19922022 <- tibble(.rows = 0)

# Prepare analysis dataset
iso3c_analysis <- analyis_datasets$popproj %>% 
  select(r10, iso3c, pop_19922022) %>% 
  ungroup() %>% 
  left_join(analyis_datasets$gmtresp %>% select(iso3c, component, gas, data)) %>% 
  group_by(component, gas) %>% 
  mutate(worlddata = sum(data),
         share_cmltvpop = pop_19922022 / sum(pop_19922022),
         shareworldata = share_cmltvpop * worlddata,
         difference = shareworldata - data)

a <- iso3c_analysis %>% 
  group_by(r10, component, gas) %>% 
  summarise(across(data:difference, ~sum(.))) %>% 
  mutate(r10 = factor(r10, levels = r10order$r10, label = r10order$r10label)) %>% 
  ggplot(aes(x = r10, y = -difference, fill = (difference > 0))) +
  geom_col() +
  facet_wrap(~interaction(gas, component)) +
  coord_flip(ylim = c(-0.1,0.06)) +
  labs( y = "Degrees Celsius", x = NULL) +
  theme_bw() +
  guides(fill = "none") +
  scale_fill_manual(values = c("#d7191c", "#2c7bb6"))

b <- iso3c_analysis %>% 
  filter(gas == "CO[2]", component == "Fossil") %>% 
  group_by(r10, component, gas) %>% 
  summarise(across(data:difference, ~sum(.))) %>% 
  mutate(r10 = factor(r10, levels = r10order$r10, label = r10order$r10label)) %>% 
  ggplot(aes(x = r10, y = -difference, fill = (difference > 0))) +
  geom_col() +
  facet_wrap(~interaction(gas, component), ncol = 1) +
  coord_flip(ylim = c(-0.1,0.06)) +
  labs(y = NULL, x = NULL) +
  theme_bw() +
  guides(fill = "none") +
  scale_fill_manual(values = c("#d7191c", "#2c7bb6"))

c <- iso3c_analysis %>% 
  filter(gas == "CO[2]") %>% 
  group_by(r10, gas) %>% 
  summarise(across(data:difference, ~sum(.))) %>% 
  mutate(r10 = factor(r10, levels = r10order$r10, label = r10order$r10label)) %>% 
  ggplot(aes(x = r10, y = -difference, fill = (difference > 0))) +
  geom_col() +
  facet_wrap(~interaction(gas)) +
  coord_flip(ylim = c(-0.1,0.06)) +
  labs(y = NULL, x = NULL) +
  theme_bw() +
  guides(fill = "none") +
  scale_fill_manual(values = c("#d7191c", "#2c7bb6"))

d <- iso3c_analysis %>% 
  group_by(r10) %>% 
  summarise(across(data:difference, ~sum(.))) %>% 
  mutate(r10 = factor(r10, levels = r10order$r10, label = r10order$r10label)) %>% 
  ggplot(aes(x = r10, y = -difference, fill = (difference > 0))) +
  geom_col() +
  facet_wrap(~"All GHGs") +
  coord_flip(ylim = c(-0.1,0.06)) +
  labs(y = "Degrees Celsius", x = NULL) +
  theme_bw() +
  guides(fill = "none") +
  scale_fill_manual(values = c("#d7191c", "#2c7bb6"))

wrap_plots(a, wrap_plots(b,c,d, ncol = 1), ncol = 2) +
  plot_annotation(tag_levels = list("a"), tag_prefix = "(", tag_suffix = ")", 
                  caption = "NAM: North America, EUR: Europe, APD: Asia-Pacific Developed, EEA: Eastern Europe and West-Central Asia, EASPAS: Eastern and South-East Asia and developing Pacific\nLAC: Latin America and Caribbean, AFRMEA: Africa and Middle East, SAS: Southern Asia",
                  title = "Difference between regional contribution to warming and equal cumulative per capita warming allocation, 1992-2022")

ggsave(here("Manuscript", "Figures", "SI", "gmtresp_ghgs.png"), height = 7, width = 10)

