# Replication archive for: "Using net-zero carbon debt to track climate overshoot responsibility"

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

# Set consistent r10 ordering
r10order <- tibble(r10 = c("R10NORTH_AM", "R10EUROPE", "R10PAC_OECD", "R10REF_ECON", "R10CHINA+", "R10MIDDLE_EAST", "R10REST_ASIA", "R10LATIN_AM", "R10AFRICA", "R10INDIA+"),
                   r10label = c("NAM", "EUR", "APD", "EEA", "EAS", "MEA", "PAS", "LAC", "AFR", "SAS"),
                   r10labellong = c("North America", "Europe", "Asia-Pacific Developed",
                                    "Eastern Europe and West-Central Asia",
                                    "Eastern Asia", "North Africa and Middle East", "South-East Asia and developing Pacific",
                                    "Latin America and Caribbean", 
                                    "Sub-saharan Africa", "Southern Asia"))

# Function to apply historical data scaling to each group, harmonising modelled
# pathways to historical 2022 values, converging to modelled pathways at a desired year.
hist_scaling <- function(path, history, harmonisationyear, convergenceyear) {
  
  scaling_factor <- history$terr_GtCO2[history$year == harmonisationyear] / path$terr_GtCO2[path$year == harmonisationyear]
  
  scaling_factors <- approx(
    x = c(harmonisationyear, seq(convergenceyear, 2100, 1)),
    y = c(scaling_factor, rep(1, length(seq(convergenceyear, 2100, 1)))),
    xout = path$year
  )$y
  
  df_a_scaled <- path %>% mutate(terr_GtCO2_histscale = terr_GtCO2 * scaling_factors)
  return(df_a_scaled)
  
}

# LOAD PROCESSED DATA ----------------------------------------------------------

# Analysis dataset, aggregated to R10
r10_analysisdata <- read_csv(here("Data", "processed", "2_analysisdata.csv")) %>%
  filter(iso3c != "ROW", year >= 1990) %>% 
  select(-iso3c) %>% 
  group_by(r10, year) %>% 
  summarise(across(everything(), ~ sum(.))) %>% 
  arrange(year)

# READ IN AND PROCESS REGIONAL EMISSIONS PATHS ---------------------------------

# Read modelled in r10-nz paths
ndclts_r10 <- read_csv(here("data", "pathways", "egr_paths", "r10", "kyoto_and_co2_emissions_summary_r10_6.csv")) %>%
  separate_wider_delim(col = Scenario, delim = "|", names = c("aggregate", paste0("category_", 1:4)), too_few = "align_start") %>% 
  mutate(
    model = ifelse(grepl(category_2, pattern = "MESSAGE"), "MESSAGEix-GLOBIOM", "REMIND-MAgPIE"),
    case = case_when(
      Model == "Current policies" & 
        grepl(category_2, pattern = "KyotoFromPrice_incrate2") &
        is.na(category_3) &
        is.na(category_4) ~ "A",
      Model == "NDC case - unconditional" & 
        grepl(category_2, pattern = "KyotoFromPrice_incrate3") &
        grepl(category_3, pattern = "nz_all_GHG") &
        grepl(category_4, pattern = "cert_allconf_0.1") ~ "E"
    )) %>% 
  filter(!is.na(case), Variable == "Emissions|CO2|Energy and Industrial Processes",
         Region != "World") %>% 
  arrange(model, case, Region, aggregate) %>% 
  mutate(
    r10 = Region) %>% 
  filter(model == "REMIND-MAgPIE") %>% 
  select(model, case, r10, aggregate, matches("\\d{4}")) %>% 
  group_by(model, case, r10, aggregate) %>% 
  summarise(across(matches("\\d{4}"), ~sum(.))) %>% 
  ungroup()

# Interpolate between model years
ndclts_r10_interp <- ndclts_r10 %>% 
  pivot_longer(-c(model, case, r10, aggregate), names_to = "year", values_to = "terr_GtCO2") %>% 
  mutate(terr_GtCO2 = terr_GtCO2 / 1e3,
         year = as.numeric(year)) %>% 
  filter(year >= 2019) %>% 
  arrange(model, case, r10, aggregate) %>% 
  group_by(model, case, r10, aggregate) %>% 
  complete(year = 2019:2100) %>% 
  group_by(model, case, r10, aggregate) %>% 
  mutate(terr_GtCO2 = na.approx(terr_GtCO2)) %>% 
  ungroup()

# Harmonise to historical 2022 values
ndclts_r10_interp_scaled <- ndclts_r10_interp %>%
  group_by(model, case, r10, aggregate) %>%
  do(hist_scaling(., filter(r10_analysisdata, r10 == first(.$r10)), 
                  harmonisationyear = 2022, convergenceyear = 2030)) %>%
  ungroup() %>% 
  mutate(year = as.numeric(year))

# Combine historical and pathways data and interpolate to annual values 1990-2100 
ndclts_r10_interp_scaled <- full_join(
  r10_analysisdata %>% 
    select(r10, year, terr_GtCO2) %>%
    left_join(ndclts_r10_interp_scaled %>% distinct(r10, model, case, aggregate),
              relationship = "many-to-many"),
  ndclts_r10_interp_scaled %>% 
    select(model, case, r10, year, aggregate, path_scaled = terr_GtCO2_histscale, path = terr_GtCO2)) %>% 
  group_by(model, r10, case, aggregate) %>% 
  complete(year = 1990:2100) %>% 
  mutate(path_scaled = na.approx(path_scaled, maxgap = 10),
         path = na.approx(path, maxgap = 10),
  ) %>% 
  ungroup() %>% 
  select(model, case, r10, aggregate, year, terr_GtCO2, path_scaled, path) %>% 
  # Remove historical paths of projected paths
  mutate(across(matches("path"), ~ifelse(year < 2023, terr_GtCO2, .)))

# ADD IN IMP-REN PATHWAY -------------------------------------------------------

impren_r10 <- read_csv(here("Data", "pathways", "ar6_imp_rensp", "ar6_snapshot_1701264588.csv")) %>% 
  filter(Region != "World") %>%
  rename(r10 = Region) %>% 
  mutate(
    model = "REMIND-MAgPIE",
    case = case_when(
      Scenario == "DeepElec_SSP2_ HighRE_Budg900" ~ "IMP-REN"),
    r10 = case_when(
      r10 == "Countries of Sub-Saharan Africa" ~ "R10AFRICA",
      r10 == "Pacific OECD" ~ "R10PAC_OECD",
      r10 == "Eastern and Western Europe (i.e., the EU28)" ~ "R10EUROPE",
      r10 == "Countries of South Asia; primarily India" ~ "R10INDIA+",
      r10 == "Countries of Latin America and the Caribbean" ~ "R10LATIN_AM",
      r10 == "Countries of the Middle East; Iran, Iraq, Israel, Saudi Arabia, Qatar, etc." ~ "R10MIDDLE_EAST",
      r10 == "North America; primarily the United States of America and Canada" ~ "R10NORTH_AM",
      r10 == "Countries of centrally-planned Asia; primarily China" ~ "R10CHINA+",
      r10 == "Reforming Economies of Eastern Europe and the Former Soviet Union; primarily Russia" ~ "R10REF_ECON",
      r10 == "Other countries of Asia" ~ "R10REST_ASIA"),
    aggregate = "Median") %>% 
  select(model, case, r10, aggregate, matches("\\d{4}")) %>% 
  group_by(model, case, r10, aggregate) %>% 
  summarise(across(matches("\\d{4}"), ~sum(.))) %>% 
  ungroup() %>% 
  pivot_longer(-c(model, case, r10, aggregate), names_to = "year", values_to = "terr_GtCO2") 

# Interpolate between model years
impren_r10_interp <- impren_r10 %>% 
  mutate(terr_GtCO2 = terr_GtCO2 / 1e3,
         year = as.numeric(year)) %>% 
  filter(year >= 2015) %>% 
  arrange(model, case, r10, aggregate) %>% 
  group_by(model, case, r10, aggregate) %>% 
  complete(year = 2015:2100) %>% 
  group_by(model, case, r10, aggregate) %>% 
  mutate(terr_GtCO2 = na.approx(terr_GtCO2)) %>% 
  ungroup()

# Harmonise to historical 2022 values
impren_r10_interp_scaled <- impren_r10_interp %>%
  group_by(model, case, r10, aggregate) %>%
  do(hist_scaling(., filter(r10_analysisdata, r10 == first(.$r10)), 
                  harmonisationyear = 2015, convergenceyear = 2030)) %>%
  ungroup() %>% 
  mutate(year = as.numeric(year))

# Combine historical and pathways data and interpolate to annual values 1990-2100 
impren_r10_interp_scaled <- full_join(
  r10_analysisdata %>% 
    select(r10, year, terr_GtCO2) %>%
    left_join(impren_r10_interp_scaled %>% distinct(r10, model, case, aggregate),
              relationship = "many-to-many"),
  impren_r10_interp_scaled %>% 
    select(model, case, r10, year, aggregate, path_scaled = terr_GtCO2_histscale, path = terr_GtCO2)) %>% 
  group_by(model, r10, case, aggregate) %>% 
  complete(year = 1990:2100) %>% 
  mutate(path_scaled = na.approx(path_scaled, maxgap = 10),
         path = na.approx(path, maxgap = 10),
  ) %>% 
  ungroup() %>% 
  select(model, case, r10, aggregate, year, terr_GtCO2, path_scaled, path) %>% 
  # Remove historical paths of projected paths
  mutate(across(matches("path"), ~ifelse(year < 2023, terr_GtCO2, .)))

# Sum up rescaled and original global emissions paths to check scaling effect
rbind(ndclts_r10_interp_scaled, impren_r10_interp_scaled) %>% 
  filter(year >= 2023, aggregate == "Median") %>% 
  group_by(model, case, aggregate, r10) %>% 
  summarise(path_scaled = sum(path_scaled),
            path = sum(path)) %>% 
  mutate(percentage = path_scaled / path - 1,
         r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label)) %>% 
  ggplot(aes(r10, percentage)) +
  geom_col(position = "dodge") +
  facet_wrap(~case, ncol = 4) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = NULL, y = "Percentage difference in cumulative GtCO2-FFI",
       subtitle = "Percentage difference in regional (R10) cumulative CO2-FFI emissions 2023-2100 after harmonising to historical data and converging to modelled paths in 2030",
       caption = paste0(paste0(r10order$r10label[1:5], ": ",r10order$r10labellong[1:5], collapse = ", "), 
                        "\n", paste0(r10order$r10label[6:10], ": ",r10order$r10labellong[6:10], collapse = ", "), collapse = ""))

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
  select(model, case, aggregate, r10, year, terr_GtCO2 = path_scaled) %>% 
  write_csv(here("Data", "processed", "r10_ndclts_impren_emiss.csv"))

rbind(ndclts_temp, impren_temp) %>% 
  write_csv(here("Data", "processed", "r10_ndclts_impren_temp.csv"))

# VISUALISE FOR SI -------------------------------------------------------------

a <- rbind(ndclts_temp,impren_temp) %>% 
  filter(quantile %in% c(0.33, 0.5, 0.66), aggregate == "Median") %>% 
  pivot_wider(names_from = quantile, values_from = gmt) %>% 
  mutate(case = factor(case, levels = c("A", "E", "IMP-REN"),
                       labels = c("CurPol", "CurPledge+allNZ", "IMP-REN"))) %>% 
  ggplot(aes(year, fill = case)) +
  geom_ribbon(aes(ymin = `0.33`, ymax = `0.66`), alpha = 0.2, show.legend = F) +
  geom_path(aes(y = `0.5`, colour = case), linewidth = 1) +
  scale_colour_manual(values = c("#1f78b4", "#66c2a5", "#b2df8a")) +
  scale_fill_manual(values = c("#1f78b4", "#66c2a5", "#b2df8a")) +
  theme_bw() +
  labs(x = NULL, y = "Temperature (C)", colour = "Scenario", fill = "Scenario")

b <- rbind(ndclts_r10_interp_scaled, impren_r10_interp_scaled) %>% 
  select(r10, year, case, aggregate, path_scaled) %>% 
  pivot_wider(names_from = aggregate, values_from = path_scaled) %>% 
  mutate(case = case_when(
    case == "A" ~ "CurPol",
    case == "E" ~ "CurPledge+allNZ",
    TRUE ~ case),
    case = factor(case, levels = c("CurPol", "CurPledge+allNZ", "IMP-REN")),
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label)) %>% 
  ggplot(aes(year, fill = case)) +
  geom_path(aes(y = Median, colour = case), linewidth = 1) +
  geom_path(aes(y = Median), linewidth = 1, 
            data = . %>% filter(year < 2023), colour = "black") +
  facet_wrap(~r10, ncol = 5) +
  scale_colour_manual(values = c("#1f78b4", "#66c2a5", "#b2df8a")) +
  scale_fill_manual(values = c("#1f78b4", "#66c2a5", "#b2df8a")) +
  theme_bw() +
  labs(x = NULL, y = "GtCO2-FFI", colour = "Scenario", fill = "Scenario")

c <- rbind(ndclts_r10_interp_scaled, impren_r10_interp_scaled) %>% 
  group_by(year, case, aggregate) %>% 
  summarise(path_scaled = sum(path_scaled)) %>% 
  select(year, case, aggregate, path_scaled) %>% 
  pivot_wider(names_from = aggregate, values_from = path_scaled) %>% 
  mutate(case = case_when(
    case == "A" ~ "CurPol",
    case == "E" ~ "CurPledge+allNZ",
    TRUE ~ case),
    case = factor(case, levels = c("CurPol", "CurPledge+allNZ", "IMP-REN"))) %>% 
  ggplot(aes(year, fill = case)) +
  geom_path(aes(y = Median, colour = case), linewidth = 1) +
  geom_path(aes(y = Median), linewidth = 1, 
            data = . %>% filter(year < 2023), colour = "black") +
  scale_colour_manual(values = c("#1f78b4", "#66c2a5", "#b2df8a")) +
  scale_fill_manual(values = c("#1f78b4", "#66c2a5", "#b2df8a")) +
  theme_bw() +
  labs(x = NULL, y = "GtCO2-FFI", colour = "Scenario", fill = "Scenario")
  
wrap_plots(wrap_plots(a,c), b, ncol = 1, heights = c(1,2)) + 
  plot_layout(guides = "collect") + 
  plot_annotation(caption = paste0(paste0(r10order$r10label[1:5], ": ",r10order$r10labellong[1:5], collapse = ", "), 
                                 "\n", paste0(r10order$r10label[6:10], ": ",r10order$r10labellong[6:10], collapse = ", "), collapse = "")) & 
  theme(legend.position = "top")

ggsave(here("Manuscript", "Figures", "SI", "SI_ndclts_paths_regional.png"),
       height = 9, width = 10)
