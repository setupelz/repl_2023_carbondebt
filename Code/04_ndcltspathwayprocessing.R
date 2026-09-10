# Replication archive for: "Using net-zero carbon debt to track climate overshoot responsibility"

# Contact for clarifications: Setu Pelz (pelz@iiasa.ac.at)    

# Script contents: Pre-process assessed regional emissions scenarios

# For scenario generation, please see: https://github.com/Rlamboll/CountryEmissionsProjections

# For scenario temperature assessment, see: 00_tempassessment*.ipynb

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
# pathways to historical values in a desired here, converging to modelled pathways 
# at a desired year.
hist_scaling <- function(path, history, harmonisationyear, convergenceyear) {
  
  scaling_factor <- history$terr_GtCO2FFI[history$year == harmonisationyear] / path$terr_GtCO2FFI[path$year == harmonisationyear]
  
  scaling_factors <- approx(
    x = c(harmonisationyear, seq(convergenceyear, 2100, 1)),
    y = c(scaling_factor, rep(1, length(seq(convergenceyear, 2100, 1)))),
    xout = path$year
  )$y
  
  df_a_scaled <- path %>% mutate(terr_GtCO2FFI_histscale = terr_GtCO2FFI * scaling_factors)
  return(df_a_scaled)
  
}

# LOAD PROCESSED DATA ----------------------------------------------------------

# Analysis dataset, aggregated to R10
r10_analysisdata <- read_csv(here("Data", "processed", "02_iso3c_indicators.csv")) %>%
  filter(iso3c != "ROW", year >= 1990) %>% 
  select(-iso3c) %>% 
  group_by(r10, year) %>% 
  summarise(across(everything(), ~ sum(.))) %>% 
  arrange(year)

# READ IN AND PROCESS REGIONAL EMISSIONS PATHS ---------------------------------

# Read modelled in r10-nz paths
curpol_curpledge_r10 <- read_csv(here("data", "pathways", "scenarios", "r10", "emiss_curpol_curpledge_r10.csv")) %>%
  mutate(
    Scenario = trimws(Scenario),  
    model = ifelse(grepl(Scenario, pattern = "MESSAGE"), "MESSAGEix-GLOBIOM", "REMIND-MAgPIE"),
    case = case_when(
      Model == "Current policies" & 
        grepl("Median", Scenario) &
        grepl("KyotoFromPrice_incrate2", Scenario) &
        grepl("NPi$", Scenario) ~ "A",
      Model == "NDC case - unconditional" & 
        grepl("Median", Scenario) &
        grepl("KyotoFromPrice_incrate3", Scenario) &
        grepl("nz_all_GHG", Scenario) &
        grepl("cert_allconf_0.1", Scenario) ~ "E"
    )) %>% 
  filter(!is.na(case), 
         Variable == "Emissions|CO2|Energy and Industrial Processes",
         Region != "World")

curpol_curpledge_r10 <- curpol_curpledge_r10 %>% 
  arrange(model, case, Region) %>% 
  mutate(r10 = Region) %>% 
  select(model, case, r10, matches("\\d{4}"))

# Interpolate between model years
curpol_curpledge_r10_interp <- curpol_curpledge_r10 %>% 
  pivot_longer(-c(model, case, r10), names_to = "year", values_to = "terr_GtCO2FFI") %>% 
  mutate(terr_GtCO2FFI = terr_GtCO2FFI / 1e3,
         year = as.numeric(year)) %>% 
  filter(year >= 2019) %>% 
  arrange(model, case, r10) %>% 
  group_by(model, case, r10) %>% 
  complete(year = 2019:2100) %>% 
  group_by(model, case, r10) %>% 
  mutate(terr_GtCO2FFI = na.approx(terr_GtCO2FFI)) %>% 
  ungroup()

# Harmonise to historical 2022 values
curpol_curpledge_r10_interp_scaled <- curpol_curpledge_r10_interp %>%
  group_by(model, case, r10) %>%
  do(hist_scaling(., filter(r10_analysisdata, r10 == first(.$r10)), 
                  harmonisationyear = 2022, convergenceyear = 2030)) %>%
  ungroup() %>% 
  mutate(year = as.numeric(year))

# Combine historical and pathways data and interpolate to annual values 1990-2100 
curpol_curpledge_r10_interp_scaled <- full_join(
  r10_analysisdata %>% 
    select(r10, year, terr_GtCO2FFI) %>%
    left_join(curpol_curpledge_r10_interp_scaled %>% distinct(r10, model, case),
              relationship = "many-to-many"),
  curpol_curpledge_r10_interp_scaled %>% 
    select(model, case, r10, year, path_scaled = terr_GtCO2FFI_histscale, path = terr_GtCO2FFI)) %>% 
  group_by(model, r10, case) %>% 
  complete(year = 1990:2100) %>% 
  mutate(path_scaled = na.approx(path_scaled, maxgap = 10),
         path = na.approx(path, maxgap = 10),
  ) %>% 
  ungroup() %>% 
  select(model, case, r10, year, terr_GtCO2FFI, path_scaled, path) %>% 
  # Remove historical paths of projected paths
  mutate(across(matches("path"), ~ifelse(year < 2023, terr_GtCO2FFI, .))) %>% 
  filter(model == "REMIND-MAgPIE")  # as this is the same as IMP-REN
  
# ADD IN IMP-REN PATHWAY -------------------------------------------------------

impren_r10 <- read_csv(here("Data", "pathways", "ar6_impren", "ar6_snapshot_1701264588.csv")) %>% 
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
  select(model, case, r10, matches("\\d{4}")) %>% 
  group_by(model, case, r10) %>% 
  summarise(across(matches("\\d{4}"), ~sum(.))) %>% 
  ungroup() %>% 
  pivot_longer(-c(model, case, r10), names_to = "year", values_to = "terr_GtCO2FFI") 

# Interpolate between model years
impren_r10_interp <- impren_r10 %>% 
  mutate(terr_GtCO2FFI = terr_GtCO2FFI / 1e3,
         year = as.numeric(year)) %>% 
  filter(year >= 2015) %>% 
  arrange(model, case, r10) %>% 
  group_by(model, case, r10) %>% 
  complete(year = 2015:2100) %>% 
  group_by(model, case, r10) %>% 
  mutate(terr_GtCO2FFI = na.approx(terr_GtCO2FFI)) %>% 
  ungroup()

# Harmonise to historical 2015 values
impren_r10_interp_scaled <- impren_r10_interp %>%
  group_by(model, case, r10) %>%
  do(hist_scaling(., filter(r10_analysisdata, r10 == first(.$r10)), 
                  harmonisationyear = 2015, convergenceyear = 2030)) %>%
  ungroup() %>% 
  mutate(year = as.numeric(year))

# Combine historical and pathways data and interpolate to annual values 1990-2100 
impren_r10_interp_scaled <- full_join(
  r10_analysisdata %>% 
    select(r10, year, terr_GtCO2FFI) %>%
    left_join(impren_r10_interp_scaled %>% distinct(r10, model, case),
              relationship = "many-to-many"),
  impren_r10_interp_scaled %>% 
    select(model, case, r10, year, path_scaled = terr_GtCO2FFI_histscale, path = terr_GtCO2FFI)) %>% 
  group_by(model, r10, case) %>% 
  complete(year = 1990:2100) %>% 
  mutate(path_scaled = na.approx(path_scaled, maxgap = 10),
         path = na.approx(path, maxgap = 10),
  ) %>% 
  ungroup() %>% 
  select(model, case, r10, year, terr_GtCO2FFI, path_scaled, path) %>% 
  # Remove historical paths of projected paths
  mutate(across(matches("path"), ~ifelse(year < 2023, terr_GtCO2FFI, .)))

# Sum up rescaled and original global emissions paths to check scaling effect
rbind(curpol_curpledge_r10_interp_scaled, impren_r10_interp_scaled) %>% 
  filter(year >= 2023) %>% 
  group_by(model, case, r10) %>% 
  summarise(path_scaled = sum(path_scaled),
            path = sum(path)) %>% 
  mutate(percentage = path_scaled / path - 1,
         r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
         case = factor(case, levels = c("A", "E", "IMP-REN"),
                       labels = c("CurPol", "CurPledge", "IMP-REN"))) %>% 
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

ggsave(here("Manuscript", "Figures", "SI", "SI_curpol_curpledge_paths_harmonisation.png"),
       height = 4, width = 12)

# READ IN AND PROCESS GLOBAL MEAN TEMPERATURE PATHS ----------------------------

# Read modelled in r10-nz paths
curpol_curpledge_temp <- read_csv(here("data", "pathways", "scenarios", "emiss_curpol_curpledge_temps.csv")) %>%
  mutate(
    Scenario = trimws(Scenario),  
    model = ifelse(grepl(Scenario, pattern = "MESSAGE"), "MESSAGEix-GLOBIOM", "REMIND-MAgPIE"),
    case = case_when(
      grepl("Median", Scenario) &
        grepl("KyotoFromPrice_incrate2", Scenario) &
        grepl("NPi$", Scenario) ~ "A",
      grepl("Median", Scenario) &
        grepl("KyotoFromPrice_incrate3", Scenario) &
        grepl("nz_all_GHG", Scenario) &
        grepl("cert_allconf_0.1", Scenario) ~ "E"
    )) %>% 
  filter(!is.na(case)) %>% 
  select(model, case, quantile = Quantile, matches("\\d{4}")) %>% 
  pivot_longer(-c(model, case, quantile), names_to = "year", values_to = "gmt") %>% 
  arrange(model, case, quantile, year)  %>% 
  filter(year >= 1990) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(model == "REMIND-MAgPIE")

# ADD IN IMP-REN PATHWAY -------------------------------------------------------

# Load in relevant modelled temperature pathways for analysis
impren_temp <- read_csv(here("data", "pathways", "ar6_impren", "emiss_ar6_impren_temps.csv")) %>% 
  mutate(model = "REMIND-MAgPIE",
         case = case_when(
           Scenario == "DeepElec_SSP2_ HighRE_Budg900" ~ "IMP-REN"
         )) %>% 
  filter(!is.na(case)) %>% 
  select(model, case, quantile = Quantile, matches("\\d{4}")) %>% 
  pivot_longer(-c(model, case, quantile), names_to = "year", values_to = "gmt") %>% 
  filter(year >= 1990, year <= 2100) %>% 
  mutate(year = as.numeric(year))

# COMBINE EMISSIONS AND TEMPERATURES AND SAVE FOR ANALYSIS ---------------------

rbind(curpol_curpledge_r10_interp_scaled, impren_r10_interp_scaled) %>% 
  select(model, case, r10, year, terr_GtCO2FFI = path_scaled) %>% 
  write_csv(here("Data", "processed", "10_r10_curpol_curpledge_impren_emiss.csv"))

rbind(curpol_curpledge_temp, impren_temp) %>% 
  write_csv(here("Data", "processed", "11_curpol_curpledge_impren_temp.csv"))

# VISUALISE FOR SI -------------------------------------------------------------

a <- rbind(curpol_curpledge_temp, impren_temp) %>% 
  filter(quantile %in% c(0.33, 0.5, 0.66)) %>% 
  pivot_wider(names_from = quantile, values_from = gmt) %>% 
  mutate(case = factor(case, levels = c("IMP-REN", "E", "A"),  # IMP-REN first, then E, then A
                       labels = c("IMP-REN", "CurPledge+allNZ", "CurPol"))) %>% 
  arrange(desc(case)) %>% 
  ggplot(aes(year, fill = case)) +
  geom_ribbon(aes(ymin = `0.33`, ymax = `0.66`), alpha = 0.2, show.legend = F) +
  geom_path(aes(y = `0.5`, colour = case), linewidth = 1) +
  scale_colour_manual(values = c("#b2df8a", "#66c2a5", "#1f78b4")) +  # Adjust colour order
  scale_fill_manual(values = c("#b2df8a", "#66c2a5", "#1f78b4")) +    # Adjust fill order
  theme_bw() +
  labs(x = NULL, y = "GMT increase (K)", colour = "Scenario", fill = "Scenario") +
  guides(colour = guide_legend(reverse = T), fill = guide_legend(reverse = T))

b <- rbind(curpol_curpledge_r10_interp_scaled, impren_r10_interp_scaled) %>% 
  select(r10, year, case, path_scaled) %>% 
  mutate(case = case_when(
    case == "A" ~ "CurPol",
    case == "E" ~ "CurPledge+allNZ",
    TRUE ~ case),
    case = factor(case, levels = c("CurPol", "CurPledge+allNZ", "IMP-REN")),
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label)) %>% 
  ggplot(aes(year, fill = case)) +
  geom_path(aes(y = path_scaled, colour = case, group = case), linewidth = 1) +
  geom_path(aes(y = path_scaled), linewidth = 1, 
            data = . %>% filter(year < 2023), colour = "black") +
  facet_wrap(~r10, ncol = 5) +
  scale_colour_manual(values = c("#1f78b4", "#66c2a5", "#b2df8a")) +
  scale_fill_manual(values = c("#1f78b4", "#66c2a5", "#b2df8a")) +
  theme_bw() +
  labs(x = NULL, y = "GtCO\U2082-FFI", colour = "Scenario", fill = "Scenario")

c <- rbind(curpol_curpledge_r10_interp_scaled, impren_r10_interp_scaled) %>% 
  group_by(year, case) %>% 
  summarise(path_scaled = sum(path_scaled)) %>% 
  select(year, case, path_scaled) %>% 
  mutate(case = case_when(
    case == "A" ~ "CurPol",
    case == "E" ~ "CurPledge+allNZ",
    TRUE ~ case),
    case = factor(case, levels = c("CurPol", "CurPledge+allNZ", "IMP-REN"))) %>% 
  ggplot(aes(year, fill = case)) +
  geom_path(aes(y = path_scaled, colour = case), linewidth = 1) +
  geom_path(aes(y = path_scaled), linewidth = 1, 
            data = . %>% filter(year < 2023), colour = "black") +
  scale_colour_manual(values = c("#1f78b4", "#66c2a5", "#b2df8a")) +
  scale_fill_manual(values = c("#1f78b4", "#66c2a5", "#b2df8a")) +
  theme_bw() +
  labs(x = NULL, y = "GtCO\U2082-FFI", colour = "Scenario", fill = "Scenario")
  
wrap_plots(wrap_plots(a,c), b, ncol = 1, heights = c(1,2)) + 
  plot_layout(guides = "collect") + 
  plot_annotation(caption = paste0(paste0(r10order$r10label[1:5], ": ",r10order$r10labellong[1:5], collapse = ", "), 
                                 "\n", paste0(r10order$r10label[6:10], ": ",r10order$r10labellong[6:10], collapse = ", "), collapse = "")) & 
  theme(legend.position = "top")

ggsave(here("Manuscript", "Figures", "SI", "SI_curpol_curpledge_paths_regional.png"),
       height = 9, width = 10)
