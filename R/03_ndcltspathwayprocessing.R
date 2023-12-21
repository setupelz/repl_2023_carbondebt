# Replication archive for: "Delaying Carbon Debt Drawdown Fails Younger Generations"

# Contact for clarifications: [ANONYMISED]       

# Script contents: Pre-process assessed regional emissions scenarios

# LOAD PACKAGES ----------------------------------------------------------------

#install.packages("pacman")
library(pacman)

# processing
p_load(dplyr, tidyr, readr, readxl, writexl, purrr, ggplot2, forcats, stringr, 
       patchwork, ggrepel, geomtextpath, colorspace)

# misc
p_load(here, countrycode, zoo)

# options
options(scipen = 999)

# Determine country-years for analysis
iso3c_tbl <- read_csv(here("Data", "countrygroups", "iso3c_region_mapping.csv")) %>% 
  select(country.name, iso3c, r10 = iamc_r10)

# Set consistent r10 ordering
r10order <- tibble(r10 = c("NAM", "EUR", "PAO", "FSU", "MEA", "EAS", "LAM", "PAS", "AFR", "SAS"),
                   r10label = c("NAM", "EUR", "APD", "EEA", "MEA", "EAS", "LAC", "SAP", "AFR", "SAS"),
                   r10labellong = c("North America", "Europe", "Asia-Pacific Developed",
                                    "Eastern Europe and West-Central Asia", "Middle East", "Eastern Asia",
                                    "Latin America and Caribbean", "South-East Asia and developing Pacific",
                                    "Africa", "Southern Asia"))

# Function to apply historical data scaling to each group, harmonising modelled
# pathways to historical 2022 values, converging to modelled pathways in 2100
hist_scaling <- function(path, history, harmonisationyear, convergenceyear) {
  
  scaling_factor <- history$gtco2[history$year == harmonisationyear] / path$gtco2[path$year == harmonisationyear]
  
  scaling_factors <- approx(
    x = c(harmonisationyear, seq(convergenceyear, 2100, 1)),
    y = c(scaling_factor, rep(1, length(seq(convergenceyear, 2100, 1)))),
    xout = path$year
  )$y
  
  df_a_scaled <- path %>% mutate(gtco2_histscale = gtco2 * scaling_factors)
  return(df_a_scaled)
  
}

# LOAD PROCESSED DATA ----------------------------------------------------------

# Recent production-based emissions
recent_prodco2 <- read_csv(here("Data", "processed", "iso3c_emiss19902022gtco2.csv")) %>% 
  select(country.name, iso3c, r10, year, gtco2) 

r10recent_prodco2 <- recent_prodco2 %>% 
  group_by(r10, year) %>% 
  summarise(gtco2 = sum(gtco2)) %>% 
  arrange(year)

# READ IN AND PROCESS REGIONAL EMISSIONS PATHS ---------------------------------

# Read modelled in r10-nz paths
ndclts_r10 <- read_csv(here("data", "pathways", "egr_paths", "r10", "infilled_extended_and_infilled_unep_r10.csv")) %>%
  separate_wider_delim(col = Scenario, delim = "|", names = c("aggregate", paste0("category_", 1:4)), too_few = "align_start") %>% 
  mutate(
    model = ifelse(grepl(category_2, pattern = "MESSAGE"), "MESSAGEix-GLOBIOM", "REMIND-MAgPIE"),
    case = case_when(
      Model == "Current policies" & 
        grepl(category_2, pattern = "KyotoFromPrice_incrate2") &
        is.na(category_3) &
        is.na(category_4) ~ "A",
      Model == "Current policies" & 
        grepl(category_2, pattern = "KyotoFromPrice_incrate3") &
        grepl(category_3, pattern = "nz_all_GHG") &
        grepl(category_4, pattern = "cert_allconf_0.1") ~ "C",
      Model == "NDC case - unconditional" & 
        grepl(category_2, pattern = "KyotoFromPrice_incrate3") &
        grepl(category_3, pattern = "nz_all_GHG") &
        grepl(category_4, pattern = "cert_allconf_0.1") ~ "E"
    )) %>% 
  filter(!is.na(case), Variable == "Emissions|CO2|Energy and Industrial Processes",
         Region != "World") %>% 
  select(model, case, r10 = Region, aggregate, matches("\\d{4}")) %>% 
  arrange(model, case, r10, aggregate) %>% 
  mutate(
    r10 = case_when(
      r10 == "R10AFRICA" ~ "AFR",
      r10 == "R10PAC_OECD" ~ "PAO",
      r10 == "R10EUROPE" ~ "EUR",
      r10 == "R10INDIA+" ~ "SAS",
      r10 == "R10LATIN_AM" ~ "LAM",
      r10 == "R10MIDDLE_EAST" ~ "MEA",
      r10 == "R10NORTH_AM" ~ "NAM",
      r10 == "R10CHINA+" ~ "EAS",
      r10 == "R10REF_ECON" ~ "FSU",
      r10 == "R10REST_ASIA" ~ "PAS"),
  ) %>% 
  filter(model == "REMIND-MAgPIE")

# Interpolate between model years
ndclts_r10_interp <- ndclts_r10 %>% 
  pivot_longer(-c(model, case, r10, aggregate), names_to = "year", values_to = "gtco2") %>% 
  mutate(gtco2 = gtco2 / 1e3,
         year = as.numeric(year)) %>% 
  filter(year >= 2019) %>% 
  arrange(model, case, r10, aggregate) %>% 
  group_by(model, case, r10, aggregate) %>% 
  complete(year = 2019:2100) %>% 
  group_by(model, case, r10, aggregate) %>% 
  mutate(gtco2 = na.approx(gtco2)) %>% 
  ungroup()

# Harmonise to historical 2022 values
ndclts_r10_interp_scaled <- ndclts_r10_interp %>%
  group_by(model, case, r10, aggregate) %>%
  do(hist_scaling(., filter(r10recent_prodco2, r10 == first(.$r10)), 
                  harmonisationyear = 2022, convergenceyear = 2030)) %>%
  ungroup() %>% 
  mutate(year = as.numeric(year))

# Combine historical and pathways data and interpolate to annual values 1990-2100 
ndclts_r10_interp_scaled <- full_join(
  r10recent_prodco2 %>% 
    select(r10, year, gtco2) %>%
    left_join(ndclts_r10_interp_scaled %>% distinct(r10, model, case, aggregate),
              relationship = "many-to-many"),
  ndclts_r10_interp_scaled %>% 
    select(model, case, r10, year, aggregate, path_scaled = gtco2_histscale, path = gtco2)) %>% 
  group_by(model, r10, case, aggregate) %>% 
  complete(year = 1990:2100) %>% 
  mutate(path_scaled = na.approx(path_scaled, maxgap = 10),
         path = na.approx(path, maxgap = 10),
  ) %>% 
  ungroup() %>% 
  select(model, case, r10, aggregate, year, gtco2, path_scaled, path) %>% 
  # Remove historical paths of projected paths
  mutate(across(matches("path"), ~ifelse(year < 2023, gtco2, .)))

# ADD IN IMP-REN PATHWAY -------------------------------------------------------

impren_r10 <- read_csv(here("Data", "pathways", "ar6_imp_rensp", "ar6_snapshot_1701264588.csv")) %>% 
  filter(Region != "World") %>%
  rename(r10 = Region) %>% 
  mutate(
    model = "REMIND-MAgPIE",
    case = case_when(
      Scenario == "DeepElec_SSP2_ HighRE_Budg900" ~ "IMP-REN"),
    r10 = case_when(
      r10 == "Countries of Sub-Saharan Africa" ~ "AFR",
      r10 == "Pacific OECD" ~ "PAO",
      r10 == "Eastern and Western Europe (i.e., the EU28)" ~ "EUR",
      r10 == "Countries of South Asia; primarily India" ~ "SAS",
      r10 == "Countries of Latin America and the Caribbean" ~ "LAM",
      r10 == "Countries of the Middle East; Iran, Iraq, Israel, Saudi Arabia, Qatar, etc." ~ "MEA",
      r10 == "North America; primarily the United States of America and Canada" ~ "NAM",
      r10 == "Countries of centrally-planned Asia; primarily China" ~ "EAS",
      r10 == "Reforming Economies of Eastern Europe and the Former Soviet Union; primarily Russia" ~ "FSU",
      r10 == "Other countries of Asia" ~ "PAS"),
    aggregate = "Median") %>% 
  select(model, case, r10, aggregate, matches("\\d{4}")) %>% 
  pivot_longer(-c(model, case, r10, aggregate), names_to = "year", values_to = "gtco2") 

# Interpolate between model years
impren_r10_interp <- impren_r10 %>% 
  mutate(gtco2 = gtco2 / 1e3,
         year = as.numeric(year)) %>% 
  filter(year >= 2015) %>% 
  arrange(model, case, r10, aggregate) %>% 
  group_by(model, case, r10, aggregate) %>% 
  complete(year = 2015:2100) %>% 
  group_by(model, case, r10, aggregate) %>% 
  mutate(gtco2 = na.approx(gtco2)) %>% 
  ungroup()

# Harmonise to historical 2022 values
impren_r10_interp_scaled <- impren_r10_interp %>%
  group_by(model, case, r10, aggregate) %>%
  do(hist_scaling(., filter(r10recent_prodco2, r10 == first(.$r10)), 
                  harmonisationyear = 2015, convergenceyear = 2030)) %>%
  ungroup() %>% 
  mutate(year = as.numeric(year))

# Combine historical and pathways data and interpolate to annual values 1990-2100 
impren_r10_interp_scaled <- full_join(
  r10recent_prodco2 %>% 
    select(r10, year, gtco2) %>%
    left_join(impren_r10_interp_scaled %>% distinct(r10, model, case, aggregate),
              relationship = "many-to-many"),
  impren_r10_interp_scaled %>% 
    select(model, case, r10, year, aggregate, path_scaled = gtco2_histscale, path = gtco2)) %>% 
  group_by(model, r10, case, aggregate) %>% 
  complete(year = 1990:2100) %>% 
  mutate(path_scaled = na.approx(path_scaled, maxgap = 10),
         path = na.approx(path, maxgap = 10),
  ) %>% 
  ungroup() %>% 
  select(model, case, r10, aggregate, year, gtco2, path_scaled, path) %>% 
  # Remove historical paths of projected paths
  mutate(across(matches("path"), ~ifelse(year < 2023, gtco2, .)))

# Sum up rescaled and original global emissions paths to check scaling effect
rbind(ndclts_r10_interp_scaled, impren_r10_interp_scaled) %>% 
  filter(year >= 2023, aggregate == "Median") %>% 
  group_by(model, case, aggregate, r10) %>% 
  summarise(path_scaled = sum(path_scaled),
            path = sum(path)) %>% 
  mutate(percentage = path_scaled / path - 1) %>% 
  ggplot(aes(r10, percentage)) +
  geom_col(position = "dodge") +
  facet_wrap(~case, ncol = 4) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw() +
  labs(x = NULL, y = "Percentage difference in cumulative GtCO2-FFI",
       subtitle = "Percentage difference in regional (R10) cumulative CO2-FFI emissions 2023-2100 after harmonising to historical data and converging to modelled paths in 2030")

ggsave(here("Manuscript", "Figures", "SI", "SI_ndclts_paths_harmonisation.png"),
       height = 4, width = 12)

# READ IN AND PROCESS GLOBAL MEAN TEMPERATURE PATHS ----------------------------

# Read modelled in r10-nz paths
ndclts_temp <- read_csv(here("data", "pathways", "egr_paths", "r10", "2023_emission_gap_temp_summary_data.csv")) %>%
  mutate(
    # extract everything before the first underscore using str_extract
    aggregate = str_extract(scenario, pattern = "^[^_]*"),
    case = case_when(
      model == "Current_policies" & 
        grepl(scenario, pattern = "KyotoFromPrice_incrate2") &
        !grepl(scenario, pattern = "nz_all_GHG") &
        !grepl(scenario, pattern = "cert_allconf_0.1") ~ "A",
      model == "Current_policies" & 
        grepl(scenario, pattern = "KyotoFromPrice_incrate3") &
        grepl(scenario, pattern = "nz_all_GHG") &
        grepl(scenario, pattern = "cert_allconf_0.1") ~ "C",
      model == "NDC_case_-_conditional" & 
        grepl(scenario, pattern = "KyotoFromPrice_incrate3") &
        grepl(scenario, pattern = "nz_all_GHG") &
        grepl(scenario, pattern = "cert_allconf_0.1") ~ "E"),
    model = ifelse(grepl(scenario, pattern = "MESSAGE"), "MESSAGEix-GLOBIOM", "REMIND-MAgPIE")
  ) %>% 
  filter(!is.na(case)) %>% 
  select(model, case, aggregate, quantile, matches("\\d{4}")) %>% 
  pivot_longer(-c(model, case, aggregate, quantile), names_to = "year", values_to = "gmt") %>% 
  arrange(model, case, aggregate, quantile, year)  %>% 
  filter(year >= 1990) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(model == "REMIND-MAgPIE")

# ADD IN IMP-REN PATHWAY -------------------------------------------------------

# Load in relevant modelled temperature pathways for analysis
impren_temp <- read_csv(here("data", "pathways", "ar6_imp_rensp", "ar6_ren_sp_climate_assessed.csv")) %>% 
  mutate(aggregate = "Median",
         model = "REMIND-MAgPIE",
         case = case_when(
           Scenario == "DeepElec_SSP2_ HighRE_Budg900" ~ "IMP-REN"
         )) %>% 
  filter(!is.na(case)) %>% 
  select(model, case, aggregate, quantile = Quantile, matches("\\d{4}")) %>% 
  pivot_longer(-c(model, case, aggregate, quantile), names_to = "year", values_to = "gmt") %>% 
  filter(year >= 1990, year <= 2100) %>% 
  mutate(year = as.numeric(year))

# COMBINE EMISSIONS AND TEMPERATURES AND SAVE FOR ANALYSIS ---------------------

rbind(ndclts_r10_interp_scaled, impren_r10_interp_scaled) %>% 
  select(model, case, aggregate, r10, year, gtco2 = path_scaled) %>% 
  write_csv(here("Data", "processed", "r10_ndclts_impren_emiss.csv"))

rbind(ndclts_temp, impren_temp) %>% 
  write_csv(here("Data", "processed", "r10_ndclts_impren_temp.csv"))

# VISUALISE FOR SI -------------------------------------------------------------

a <- rbind(ndclts_r10_interp_scaled, impren_r10_interp_scaled) %>% 
  select(r10, year, case, aggregate, path_scaled) %>% 
  pivot_wider(names_from = aggregate, values_from = path_scaled) %>% 
  mutate(case = case_when(
    case == "A" ~ "CurPol",
    case == "C" ~ "CurPol+allNZ",
    case == "E" ~ "CurPledge+allNZ",
    TRUE ~ case),
    case = factor(case, levels = c("CurPol", "CurPol+allNZ", "CurPledge+allNZ", "IMP-REN")),
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label)) %>% 
  ggplot(aes(year, fill = case)) +
  geom_ribbon(aes(ymin = Min, ymax = Max), alpha = 0.5) +
  geom_path(aes(y = Median, colour = case), linewidth = 1) +
  geom_path(aes(y = Median), linewidth = 1, 
            data = . %>% filter(year < 2023), colour = "black") +
  facet_wrap(~r10, ncol = 5) +
  scale_colour_brewer(palette = "BrBG", direction = 1) +
  scale_fill_brewer(palette = "BrBG", direction = 1) +
  theme_bw() +
  labs(x = NULL, y = "GtCO2-FFI", colour = "Scenario", fill = "Scenario")

b <- rbind(ndclts_r10_interp_scaled, impren_r10_interp_scaled) %>% 
  group_by(year, case, aggregate) %>% 
  summarise(path_scaled = sum(path_scaled)) %>% 
  select(year, case, aggregate, path_scaled) %>% 
  pivot_wider(names_from = aggregate, values_from = path_scaled) %>% 
  mutate(case = case_when(
    case == "A" ~ "CurPol",
    case == "C" ~ "CurPol+allNZ",
    case == "E" ~ "CurPledge+allNZ",
    TRUE ~ case),
    case = factor(case, levels = c("CurPol", "CurPol+allNZ", "CurPledge+allNZ", "IMP-REN"))) %>% 
  ggplot(aes(year, fill = case)) +
  geom_ribbon(aes(ymin = Min, ymax = Max), alpha = 0.5) +
  geom_path(aes(y = Median, colour = case), linewidth = 1) +
  geom_path(aes(y = Median), linewidth = 1, 
            data = . %>% filter(year < 2023), colour = "black") +
  scale_colour_brewer(palette = "BrBG", direction = 1) +
  scale_fill_brewer(palette = "BrBG", direction = 1) +
  theme_bw() +
  labs(x = NULL, y = "GtCO2-FFI", colour = "Scenario", fill = "Scenario")
  
wrap_plots(wrap_plots(b), a, ncol = 1, heights = c(1,2)) + 
  plot_layout(guides = "collect") & theme(legend.position = "top")

ggsave(here("Manuscript", "Figures", "SI", "SI_ndclts_paths_regional.png"),
       height = 9, width = 10)
