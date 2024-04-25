# Replication archive for: "Delaying Carbon Debt Drawdown Fails Younger Generations"

# Contact for clarifications: [ANONYMISED]       

# Script contents: Pre-process exposure to extreme heatwaves under assessed scenarios

# LOAD PACKAGES ----------------------------------------------------------------

#install.packages("pacman")
library(pacman)

# processing
p_load(dplyr, tidyr, readr, readxl, writexl, purrr, ggplot2, forcats, stringr, 
       patchwork, ggrepel, geomtextpath, colorspace, Hmisc)

# misc
p_load(here, countrycode, zoo)

# options
options(scipen = 999)

# LOAD PROCESSED DATA ----------------------------------------------------------

# Determine analysis countries
iso3c_tbl_analysis <- read_csv(here("Data", "countrygroups", "iso3c_region_mapping.csv")) %>% 
  mutate(r10 = r10_iamc) %>% 
  select(iso3c, r10) %>% 
  right_join(read_xlsx(here("Data", "processed", "analysisdata.xlsx"),
                       sheet = "hist_prodco2") %>% select(iso3c))

# Write to file 
write_csv(iso3c_tbl_analysis, here("data", "processed", "iso3c_tbl_analysis.csv"))

# Set consistent r10 ordering
r10order <- tibble(r10 = c("R10NORTH_AM", "R10EUROPE", "R10PAC_OECD", "R10REF_ECON", "R10CHINA+", "R10MIDDLE_EAST", "R10REST_ASIA", "R10LATIN_AM", "R10AFRICA", "R10INDIA+"),
                   r10label = c("NAM", "EUR", "APD", "EEA", "EAS", "MEA", "PAS", "LAC", "AFR", "SAS"),
                   r10labellong = c("North America", "Europe", "Asia-Pacific Developed",
                                    "Eastern Europe and West-Central Asia",
                                    "Eastern Asia", "North Africa and Middle East", "South-East Asia and developing Pacific",
                                    "Latin America and Caribbean", 
                                    "Sub-saharan Africa", "Southern Asia"))

# LOAD UNPROCESSED HEATWAVE EXPOSURE DATA --------------------------------------

# Unzip and convert netcdf file to stars tbl_cube
unzip(here("Data", "impacts", "nc_lifetime_exposure_new_heatwavedarea_HWMId99.nc4.zip"), exdir = tempdir())
netcdf_file <- list.files(tempdir(), pattern = "\\.nc4$", full.names = TRUE)
exp_heatwave <- stars::read_ncdf(netcdf_file, var = "lifetime_exposure") %>%
  stars::as.tbl_cube.stars() %>%
  as_tibble() %>%
  filter(!is.nan(lifetime_exposure)) %>% 
  mutate(iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
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
  mutate(iso3c = countrycode(country, origin = "country.name", destination = "iso3c")) %>%
  select(birth_year = time, iso3c, cohort_size)

# PROCESS HEATWAVE EXPOSURE DATA -----------------------------------------------

# Determine which countries we assess are covered in the exposure dataset
iso3c_exp <- distinct(exp_heatwave, country) %>% mutate(data = 1) %>%
  mutate(iso3c = countrycode(country, origin = "country.name", destination = "iso3c")) %>%
  select(iso3c, country, data) %>%
  right_join(iso3c_tbl_analysis) %>%
  ungroup() %>%
  select(iso3c, country, r10, data)

# Add population data for weighted aggregation to r10 level
exp_heatwave <- left_join(exp_heatwave, cohort_pop) %>%
  filter(country %in% iso3c_exp$country)

# Aggregate to r10 level and separate assessed pathways from IMP-REN
exp_heatwave_r10 <- exp_heatwave %>%
  left_join(iso3c_tbl_analysis %>% select(iso3c, r10)) %>% 
  separate_wider_delim(GMT, delim = "_", names = c("case", "case2", "aggregate", "quantile")) %>% 
  filter(case %in% c("A", "E", "IMP-REN")) %>% 
  group_by(birth_year, r10, case, aggregate, quantile, gcm, run) %>%
  summarise(lifetime_exposure = wtd.mean(lifetime_exposure, weights = cohort_size, na.rm = T))

exp_heatwave_r10_impren <- exp_heatwave_r10 %>% 
  filter(case == "IMP-REN") %>% 
  ungroup() %>% 
  rename(lifetime_exposure_impren = lifetime_exposure) %>% 
  select(-case, -aggregate)

# Check distribution of EMFs
a <- left_join(exp_heatwave_r10, exp_heatwave_r10_impren) %>% 
  filter(birth_year == 2020, aggregate == "Median") %>% 
  group_by(birth_year, r10, case, aggregate, quantile) %>% 
  summarise(lifetime_exposure_mean = mean(lifetime_exposure),
            lifetime_exposure_sd = sd(lifetime_exposure)) %>% 
  mutate(
    case = case_when(
      case == "A" ~ "CurPol",
      case == "E" ~ "CurPledge+allNZ",
      TRUE ~ case),
    case = factor(case, levels = c("CurPol", "CurPledge+allNZ", "IMP-REN")),
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label)) %>% 
  ggplot(aes(x = quantile, group = case)) +
  geom_ribbon(aes(ymin = lifetime_exposure_mean - lifetime_exposure_sd,
                  ymax = lifetime_exposure_mean + lifetime_exposure_sd,
                  fill = case), alpha = 0.2) +
  geom_line(aes(y = lifetime_exposure_mean, colour = case)) +
  scale_colour_discrete_qualitative(drop = F) +
  facet_wrap(~r10, ncol = 5) +
  theme_bw() +
  theme(legend.position = "top") +
  labs(x = "Temperature response quantile",
       y = "Lifetime exposure (years with extreme heatwaves)",
       colour = NULL, fill = NULL)

b <- left_join(exp_heatwave_r10, exp_heatwave_r10_impren) %>% 
  filter(birth_year == 2020, aggregate == "Median") %>% 
  group_by(birth_year, r10, case, aggregate) %>% 
  mutate(add = lifetime_exposure - lifetime_exposure_impren,
         emf = lifetime_exposure / lifetime_exposure_impren) %>% 
  mutate(
    case = case_when(
      case == "A" ~ "CurPol",
      case == "E" ~ "CurPledge+allNZ",
      TRUE ~ case),
    case = factor(case, levels = c("CurPol", "CurPledge+allNZ", "IMP-REN")),
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label)) %>% 
  filter(case != "IMP-REN") %>% 
  ggplot(aes(colour = case)) +
  geom_point(aes(x = emf, y = add), alpha = 0.6, size = 3) +
  scale_colour_discrete_qualitative(drop = F) +
  facet_wrap(~r10, ncol = 5) +
  guides(colour = "none") +
  theme_bw() +
  theme(legend.position = "top") +
  labs(y = "Increase in years with extreme heatwave exposure relative to IMP-REN (Years)",
       x = "Increase in extreme heatwave exposure relative to IMP-REN (Factor)",
       colour = NULL, fill = NULL)

wrap_plots(a,b, ncol = 1) + 
  plot_annotation(caption = paste0(paste0(r10order$r10label[1:5], ": ",r10order$r10labellong[1:5], collapse = ", "), 
                                   "\n", paste0(r10order$r10label[6:10], ": ",r10order$r10labellong[6:10], collapse = ", "), collapse = ""))

ggsave(here("Manuscript", "Figures", "SI", "SI_heatwaveexp_quantile.png"),
       height = 12, width = 12)

# Determine additional years of exposure relative to IMP-REN
exp_heatwave_r10_emf_impren <- left_join(exp_heatwave_r10, exp_heatwave_r10_impren) %>% 
  na.omit() %>% 
  ungroup() %>%
  group_by(birth_year, r10, case, aggregate) %>% 
  summarise(add_impren_0.5 = quantile(lifetime_exposure - lifetime_exposure_impren, probs = 0.5),
            add_impren_0.33 = quantile(lifetime_exposure - lifetime_exposure_impren, probs = 0.33),
            add_impren_0.66 = quantile(lifetime_exposure - lifetime_exposure_impren, probs = 0.66),
            emf_impren_0.5 = quantile(lifetime_exposure / lifetime_exposure_impren, probs = 0.5),
            emf_impren_0.33 = quantile(lifetime_exposure / lifetime_exposure_impren, probs = 0.33),
            emf_impren_0.66 = quantile(lifetime_exposure / lifetime_exposure_impren, probs = 0.66),
            lifetime_exposure_impren_0.5 = quantile(lifetime_exposure_impren, probs = 0.5),
            lifetime_exposure_impren_0.33 = quantile(lifetime_exposure_impren, probs = 0.33),
            lifetime_exposure_impren_0.66 = quantile(lifetime_exposure_impren, probs = 0.66)) %>%
  filter(case %in% c("A", "E"))

# Save temperature response quantile panels for SI
a <- exp_heatwave_r10_emf_impren %>% 
  mutate(
    case = case_when(
      case == "A" ~ "CurPol",
      case == "E" ~ "CurPledge+allNZ",
      TRUE ~ case),
    case = factor(case, levels = c("CurPledge+allNZ", "CurPol"),
                  labels = c("All pledges and net-zero targets", "Current policies")),
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label)) %>% 
  filter(birth_year == 2020, aggregate == "Median") %>% 
  ggplot() +
  geom_hline(yintercept = 1, linetype = 2, colour = "red") +
  geom_point(aes(x = lifetime_exposure_impren_0.5, y = add_impren_0.5, colour = r10)) +
  geom_errorbar(aes(x = lifetime_exposure_impren_0.5, ymin = add_impren_0.33, 
                    ymax = add_impren_0.66, colour = r10), width = 0,  alpha = 0.4) +
  geom_errorbar(aes(y = add_impren_0.5, xmin = lifetime_exposure_impren_0.33,
                    xmax = lifetime_exposure_impren_0.66 , colour = r10), width = 0, alpha = 0.4) +
  scale_colour_discrete_qualitative() +
  facet_wrap(~case, ncol = 3) +
  theme_bw() +
  labs(x = "Lifetime years with extreme heatwaves in IMP-REN, relative to pre-industrial control", 
       y = "Additional years with extreme heatwaves in scenario, relative to IMP-REN",
       colour = NULL) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1))

b <- exp_heatwave_r10_emf_impren %>% 
  mutate(
    case = case_when(
      case == "A" ~ "CurPol",
      case == "E" ~ "CurPledge+allNZ",
      TRUE ~ case),
    case = factor(case, levels = c("CurPledge+allNZ", "CurPol"),
                  labels = c("All pledges and net-zero targets", "Current policies")),
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label)) %>% 
  filter(birth_year == 2020, aggregate == "Median") %>% 
  ggplot() +
  geom_hline(yintercept = 1, linetype = 2, colour = "red") +
  geom_point(aes(x = lifetime_exposure_impren_0.5, y = emf_impren_0.5, colour = r10)) +
  geom_errorbar(aes(x = lifetime_exposure_impren_0.5, ymin = emf_impren_0.33, 
                    ymax = emf_impren_0.66, colour = r10), width = 0,  alpha = 0.4) +
  geom_errorbar(aes(y = emf_impren_0.5, xmin = lifetime_exposure_impren_0.33,
                    xmax = lifetime_exposure_impren_0.66 , colour = r10), width = 0, alpha = 0.4) +
  scale_colour_discrete_qualitative() +
  facet_wrap(~case, ncol = 3) +
  theme_bw() +
  labs(x = "Lifetime years with extreme heatwaves in IMP-REN, relative to pre-industrial control", 
       y = "Exposure multiplication factor, relative to IMP-REN",
       colour = NULL) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1))

wrap_plots(a,b, ncol = 1) + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = list("a"), tag_prefix = "(", tag_suffix = ")", 
                  caption = paste0(paste0(r10order$r10label[1:5], ": ",r10order$r10labellong[1:5], collapse = ", "), 
                                   "\n", paste0(r10order$r10label[6:10], ": ",r10order$r10labellong[6:10], collapse = ", "), collapse = "")) &
  theme(legend.position = "top") & guides(colour = guide_legend(nrow = 2))

ggsave(here("Manuscript", "Figures", "SI", "SI_heatwaveexp.png"),
       height = 12, width = 10)

# Write to file
exp_heatwave_r10_emf_impren <- 
  exp_heatwave_r10_emf_impren %>% 
  left_join(cohort_pop %>% right_join(iso3c_tbl_analysis) %>% 
              group_by(r10, birth_year) %>% 
              summarise(cohort_size = sum(cohort_size, na.rm = T)))

write_csv(exp_heatwave_r10_emf_impren, here("Data", "processed", "r10_exp_heatwave_emf.csv"))

