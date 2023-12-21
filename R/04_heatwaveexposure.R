# Replication archive for: "Delaying Carbon Debt Drawdown Fails Younger Generations"

# Contact for clarifications: [ANONYMISED]       

# Script contents: Pre-process exposure to extreme heatwaves under assessed scenarios

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

# LOAD PROCESSED DATA ----------------------------------------------------------

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

# LOAD UNPROCESSED HEATWAVE EXPOSURE DATA --------------------------------------

# Unzip and convert netcdf file to stars tbl_cube
unzip(here("Data", "impacts", "nc_lifetime_exposure_new_heatwavedarea_HWMId99.nc4.zip"), exdir = tempdir())
netcdf_file <- list.files(tempdir(), pattern = "\\.nc4$", full.names = TRUE)
exp_heatwave <- stars::read_ncdf(netcdf_file, var = "lifetime_exposure") %>%
  stars::as.tbl_cube.stars() %>%
  as_tibble() %>%
  filter(!is.nan(lifetime_exposure)) %>% 
  mutate(iso3c = countrycode(country, origin = "country.name", destination = "wb"),
         run = ceiling(run),
         gcm = case_when(
           run %in% 1:3 ~ "gcm1",
           run %in% 4:6 ~ "gcm2",
           run %in% 7:9 ~ "gcm3",
           run %in% 10:12 ~ "gcm4"))

# Read in cohort populations
cohort_pop <- stars::read_ncdf(here("Data", "impacts", "nc_cohort_sizes.nc4"),
                               var = "cohort_size") %>%
  stars::as.tbl_cube.stars() %>%
  as_tibble() %>%
  filter(ages == 0) %>%
  mutate(iso3c = countrycode(country, origin = "country.name", destination = "wb")) %>%
  select(birth_year = time, iso3c, cohort_size)

# PROCESS HEATWAVE EXPOSURE DATA -----------------------------------------------

# Determine which countries we assess are covered in the exposure dataset
iso3c_exp <- distinct(exp_heatwave, country) %>% mutate(data = 1) %>%
  mutate(iso3c = countrycode(country, origin = "country.name", destination = "wb")) %>%
  select(iso3c, country, data) %>%
  right_join(iso3c_tbl) %>%
  ungroup() %>%
  select(iso3c, country, country.name, r10, data)

# Add population data for weighted aggregation to r10 level
exp_heatwave <- left_join(exp_heatwave, cohort_pop) %>%
  filter(country %in% iso3c_exp$country)

# Aggregate to r10 level and separate assessed pathways from IMP-REN
exp_heatwave_r10 <- exp_heatwave %>%
  left_join(iso3c_tbl %>% select(iso3c, r10)) %>% 
  separate_wider_delim(GMT, delim = "_", names = c("case", "case2", "aggregate", "quantile")) %>% 
  filter(aggregate == "Median") %>% 
  group_by(birth_year, r10, case, aggregate, quantile, gcm, run) %>%
  summarise(lifetime_exposure = weighted.mean(lifetime_exposure, weights = cohort_size))

exp_heatwave_r10_impren <- exp_heatwave_r10 %>% 
  filter(case == "IMP-REN") %>% 
  ungroup() %>% 
  pivot_wider(names_from = case, values_from = lifetime_exposure) %>% 
  rename(lifetime_exposure_impren = `IMP-REN`) %>% 
  select(birth_year, r10, quantile, gcm, run, lifetime_exposure_impren)

# Check distribution of EMFs
left_join(exp_heatwave_r10, exp_heatwave_r10_impren) %>% 
  filter(birth_year == 2020, quantile %in% c(0.33, 0.5, 0.66)) %>% 
  ggplot(aes(x = r10, y = lifetime_exposure / lifetime_exposure_impren,
             colour = case)) +
  geom_boxplot() +
  facet_wrap(~quantile, ncol = 1)

# Determine EMFs relative to IMP-REN
exp_heatwave_r10_emf_impren <- left_join(exp_heatwave_r10, exp_heatwave_r10_impren) %>% 
  na.omit() %>% 
  ungroup() %>%
  mutate(quantile = as.numeric(quantile)) %>% 
  group_by(birth_year, r10, case, quantile) %>% 
  summarise(emf_impren_0.5 = quantile(lifetime_exposure / lifetime_exposure_impren, probs = 0.5),
            emf_impren_0.33 = quantile(lifetime_exposure / lifetime_exposure_impren, probs = 0.33),
            emf_impren_0.66 = quantile(lifetime_exposure / lifetime_exposure_impren, probs = 0.66)) %>% 
  filter(case %in% c("A", "C", "E"))

# Save temperature response quantile panels for SI
exp_heatwave_r10_emf_impren %>% 
  mutate(
    case = case_when(
      case == "A" ~ "CurPol",
      case == "C" ~ "CurPol+allNZ",
      case == "E" ~ "CurPledge+allNZ",
      TRUE ~ case),
    case = factor(case, levels = c("CurPol", "CurPol+allNZ", "CurPledge+allNZ", "IMP-REN"))) %>% 
  filter(birth_year == 2020, quantile %in% c(0.33, 0.5, 0.66)) %>% 
  ggplot() +
  geom_hline(yintercept = 1, linetype = 2, colour = "red") +
  geom_point(aes(x = r10, y = emf_impren_0.5, colour = case),
             position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(x = r10, ymin = emf_impren_0.33, 
                    ymax = emf_impren_0.66, colour = case), 
                position = position_dodge(width = 0.7), width = 0.05) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = "x")) +
  scale_colour_brewer(palette = "Set2", direction = -1) +
  facet_wrap(~quantile, ncol = 1, scales = "free_y") +
  theme_bw() +
  labs(x = NULL, y = "EMF relative to illustrative 1.5C Scenario (AR6 IMP-REN)",
       colour = NULL) +
  theme(legend.position = "top")

ggsave(here("Manuscript", "Figures", "SI", "SI_heatwaveexp_quantile.png"),
       height = 10, width = 4)

# Write to file
write_csv(exp_heatwave_r10_emf_impren, here("Data", "processed", "exp_heatwave_r10_emf.csv"))
