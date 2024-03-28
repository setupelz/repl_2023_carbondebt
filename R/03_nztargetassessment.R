# Replication archive for: "Delaying Carbon Debt Drawdown Fails Younger Generations"

# Contact for clarifications: [ANONYMISED]       

# Script contents: Impute net-zero GHG targets for countries missing data

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

# Determine analysis countries
iso3c_tbl_analysis <- read_csv(here("Data", "countrygroups", "iso3c_region_mapping.csv")) %>% 
  mutate(r10 = ifelse(is.na(r10_iamc), NA_real_, r10_unif)) %>% 
  select(iso3c, r10) %>% 
  right_join(read_xlsx(here("Data", "processed", "analysisdata.xlsx"),
                       sheet = "hist_prodco2") %>% select(iso3c))

# Set consistent r10 ordering
r10order <- tibble(r10 = c("R10NAM", "R10EUR", "R10PAO", "R10FSU", "R10EASPAS", "R10LAM", "R10AFRMEA", "R10SAS"),
                   r10label = c("NAM", "EUR", "APD", "EEA", "EASPAS", "LAC", "AFRMEA", "SAS"),
                   r10labellong = c("North America", "Europe", "Asia-Pacific Developed",
                                    "Eastern Europe and West-Central Asia", "Africa & Middle East", 
                                    "Eastern & South-East Asia and developing Pacific",
                                    "Latin America and Caribbean", "Southern Asia"))

# LOAD PROCESSED AND OTHER DATA ------------------------------------------------

# GDP MER
gdpmer <- read_xlsx(here("Data", "processed", "analysisdata.xlsx"),
                         sheet = "recent_gdpmer") %>% 
  group_by(r10, year) %>% 
  filter(year == 2019)

# RCB
rcb <- read_csv(here("Data", "processed", "iso3c_rcb19902020gtco2.csv")) %>% 
  filter(year == 2019) %>% 
  select(iso3c, gtco2, pp1990)

# Net-zero target years
nztargets <- read_xlsx(here("Data", "pathways", "nz_targets", "nztrackerdata_202403.xlsx"), sheet = 1) %>% 
  filter(actor_type == "Country", end_target %in% c("Net zero", "Climate neutral", "Carbon neutral(ity)"),
         end_target_status %in% c("In law", "In policy document", "Declaration / pledge")) %>% 
  select(iso3c = country, end_target, end_target_year, end_target_status, end_target_text)

# CAT net-zero target years used in EGR 2023
catnztargets <- read_xlsx(
  here("Data", "pathways", "nz_targets", "Analysis_update_ImplementationProgress_EGR2023_master_finalUpdateCheck.xlsx"), 
  skip = 2) %>% 
  select(iso3c = `...2`, cat_end_target = `Net zero target applicable to`, 
         cat_end_target_status = `Legally binding`, cat_end_target_year = `Year...12`) %>% 
  na.omit()

# ANALYSE ----------------------------------------------------------------------

# Isolate to analysis countries
nztargetanalysis <- list(gdpmer, rcb, nztargets) %>% 
  reduce(full_join) %>% 
  right_join(iso3c_tbl_analysis) %>% 
  mutate(end_target_status = ifelse(is.na(end_target_status), "None", end_target_status)) %>% 
  ungroup() %>% 
  arrange(r10, desc(gtco2))

# Aggregate CAT NZ targets and updated NZT targets
alltargets <- full_join(nztargetanalysis %>% 
                          mutate(iso3c_grp = ifelse(iso3c %in%
                                                      c("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", 
                                                        "DNK", "ESP", "EST", "FIN", "FRA", "GRC", 
                                                        "HRV", "HUN", "IRL", "ITA", "LTU", "LUX", 
                                                        "LVA", "MLT", "NLD", "POL", "PRT", "ROU", 
                                                        "SVK", "SVN", "SWE"), "EU27", iso3c)),
                        catnztargets, by = c("iso3c_grp" = "iso3c")) %>% 
  mutate(end_target_year_comb = 
           ifelse(is.na(end_target_year) & str_detect(cat_end_target_year, "\\d{4}"), cat_end_target_year, end_target_year),
         end_target_comb = case_when(
           is.na(end_target_year) & str_detect(cat_end_target_year, "\\d{4}") ~ cat_end_target,
           !is.na(end_target_year) ~ "GHG"),
         end_target_status_comb = case_when(
           is.na(end_target_year) & str_detect(cat_end_target_year, "\\d{4}") & cat_end_target_status == "Y" ~ "In law",
           is.na(end_target_year) & str_detect(cat_end_target_year, "\\d{4}") & cat_end_target_status == "N" ~ "Declaration / pledge",
           is.na(end_target_year) & str_detect(cat_end_target_year, "\\d{4}") & cat_end_target_status == "No NZT" ~ "Assumed",
           !is.na(end_target_year) ~ end_target_status,
           TRUE ~ "None"),
         end_target_status_comb = 
           factor(end_target_status_comb,
                  levels = c("In law", "In policy document",
                             "Declaration / pledge", "Assumed", "None"),
                  labels = c("In law", "In policy document",
                             "Declaration / pledge",  "Assumed", "None"))
         ) %>% 
  select(r10, iso3c, iso3c_grp, end_target, cat_end_target, end_target_year, cat_end_target_year, end_target_year_comb, end_target_comb, end_target_status_comb,
         end_target_status, cat_end_target_status, end_target_text, gtco2, pp1990, gdpcurrmer) %>% 
  arrange(r10, desc(!is.na(end_target_year_comb)), iso3c)

# Add 2020 GHGs (GWP100, AR6) for projection -----------------------------------

jonesetal <- read_csv(here("Data", "pathways", "nz_targets", "EMISSIONS_ANNUAL_1830-2022.csv")) %>% 
  select(iso3c = ISO3, gas = Gas, component = Component, year = Year, data = Data, unit = Unit) %>% 
  filter(iso3c %in% alltargets$iso3c, year == 2020, component != "Total") %>% 
  # Convert to CO2eq (GWP100) using factors from AR6 
  # N[2]*O = 273
  # CH[4] Fossil = 29.8
  # CH[4] AFOLU = 27.2
  mutate(mtco2e = 
           case_when(
             gas == "N[2]*O" ~ data * 273, # 1 Tg = 1 Mt
             gas == "CH[4]" & component == "Fossil" ~ data * 29.8, # 1 Tg = 1 Mt
             gas == "CH[4]" & component == "LULUCF" ~ data * 27.8, # 1 Tg = 1 Mt
             TRUE ~ data * 1e3 # 1 Pg = 1e3 Mt
           )) %>% 
  group_by(year, iso3c) %>% 
  summarise(mtco2e = sum(mtco2e))

alltargets <- left_join(alltargets, jonesetal)


# VISUALISE COVERAGE -----------------------------------------------------------

a <- alltargets %>% 
  group_by(r10, end_target_status_comb) %>% 
  summarise("Emiss. CO2FFI (GtCO2, 2019)" = sum(gtco2),
            "Emiss. GHGs (GtCO2e GWP100, 2020)" = sum(mtco2e / 1e3),
            "RCB PP1990 Creditors (GtCO2, 2019)" = sum(pp1990[pp1990 > 0]),
            "RCB PP1990 Debtors (GtCO2, 2019)" = sum(pp1990[pp1990 < 0]),
            "GDP MER (Billions, 2019)" = sum(gdpcurrmer / 1e12)) %>% 
  pivot_longer(cols = -c(r10, end_target_status_comb)) %>% 
  mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label)) %>% 
  group_by(r10, name) %>% 
  ggplot(aes(r10, value, fill = end_target_status_comb)) +
  geom_col() +
  scale_fill_brewer(type = "qual") +
  facet_wrap(~name, ncol = 5, scales = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = NULL, y = NULL, fill = "Net-zero targets")

b <- alltargets %>% 
  group_by(end_target_status_comb) %>% 
  summarise("Emiss. CO2FFI (GtCO2, 2019)" = sum(gtco2),
            "Emiss. GHGs (GtCO2e GWP100, 2020)" = sum(mtco2e / 1e3),
            "RCB PP1990 Creditors (GtCO2, 2019)" = sum(pp1990[pp1990 > 0]),
            "RCB PP1990 Debtors (GtCO2, 2019)" = sum(pp1990[pp1990 < 0]),
            "GDP MER (Billions, 2019)" = sum(gdpcurrmer / 1e12)) %>% 
  pivot_longer(cols = -c(end_target_status_comb)) %>% 
  group_by(name) %>% 
  ggplot(aes("Global", value, fill = end_target_status_comb)) +
  geom_col() +
  scale_fill_brewer(type = "qual") +
  facet_wrap(~name, ncol = 5, scales = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = NULL, y = NULL, fill = "Net-zero targets")

wrap_plots(a,b, ncol = 1) + plot_layout(guides = "collect") &
  theme(legend.position = "top") &
  plot_annotation(title = "Net-zero target coverage by indicators")

ggsave(here("Manuscript", "Figures", "SI", "SI_nztargetcoverage.png"),
       height = 5, width = 12)

# WRITE TO FILE ----------------------------------------------------------------

alltargets %>% 
  write_csv(here("Data", "processed", "nztargetyears.csv"), na = "")

