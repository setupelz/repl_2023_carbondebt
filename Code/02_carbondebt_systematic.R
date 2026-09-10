# Replication archive for: "Using net-zero carbon debt to track climate overshoot responsibility"

# Contact for clarifications: Setu Pelz (pelz@iiasa.ac.at)

# Script contents: Systematically explore carbon debt accrual using the AR6 database.

# LOAD PACKAGES ----------------------------------------------------------------

#install.packages("pacman")
library(pacman)

# processing
p_load(dplyr, tidyr, readr, readxl, writexl, purrr, ggplot2, forcats, stringr, 
       patchwork, ggrepel, geomtextpath, colorspace, ggridges)

# misc
p_load(here, countrycode, zoo)

# options
options(scipen = 999)

# helper function for rank middle (tie = lower) of a vector
rank_middle <- function(x, na.rm = FALSE) {
  x <- sort(x)
  if (!length(x)) return(NA)
  i <- if (length(x) %% 2 == 1) {
    (length(x) + 1) / 2
    } else {
      length(x) / 2
      }
  x[i]
}

# LOAD PROCESSED DATA ----------------------------------------------------------

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

# Remaining carbon budgets from 1990 to 2020, aggregated to r10
r10_rcb19902020 <- read_csv(here("Data", "processed", "05_r10_rcb19902020gtco2ffi.csv"))

# Analysis dataset, aggregated to R10
r10_analysisdata <- read_csv(here("Data", "processed", "02_iso3c_indicators.csv")) %>%
  filter(iso3c != "ROW", year >= 1990) %>% 
  select(-iso3c) %>% 
  group_by(r10, year) %>% 
  summarise(across(everything(), ~ sum(.))) %>% 
  arrange(year)

# AR6 CO2-FFI data
r10_ar6_co2ffi <- read_csv(here("Data", "pathways", "ar6_all", "ar6_all_co2ffi.csv"))

# RCB quantities
rcb <- read_csv(here("Data", "processed", "03_rcbquantities.csv"))

# DETERMINE PEAK/NETZERO PATHWAYS USING AR6 DATABASE ---------------------------

# Pivot data and interpolate between model periods (linear)
r10_ar6_co2ffi_processed <- r10_ar6_co2ffi %>% 
  select(-Unit) %>% 
  filter(!is.na(Category)) %>% 
  pivot_longer(-c(Model, Scenario, Region, Variable, Category),
               names_to = "Year", values_to = "value") %>%
  mutate(Year = as.numeric(Year)) %>% 
  group_by(Model, Scenario, Region, Variable, Category) %>% 
  complete(Year = 2020:2100, fill = list(value = NA)) %>% 
  filter(Year >= 2020) %>% 
  arrange(Model, Scenario, Region, Variable, Category, Year) %>% 
  group_by(Model, Scenario, Region, Variable, Category) %>% 
  mutate(value = na.approx(value, maxgap = 10)) %>% 
  filter(Region != "R10ROWO") %>% 
  mutate(Variable = ifelse(Variable == "Population", "pop", "mtco2")) %>% 
  pivot_wider(names_from = Variable, values_from = value) %>% 
  ungroup() %>% 
  transmute(model = Model, scen = Scenario, 
            r10 = Region,
            cat = Category, year = Year, terr_GtCO2FFI = mtco2 / 1e3, pop) %>% 
  group_by(model, scen, r10, cat, year) %>% 
  summarise(terr_GtCO2FFI = sum(terr_GtCO2FFI))

# Function to apply historical data scaling to each group, harmonising modelled
# pathways to historical <=2022 values, converging to modelled pathways at a desired year.
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

# Apply scaling to all model-scenario-r10 groups
r10_ar6_co2ffi_processed_scaled <- r10_ar6_co2ffi_processed %>%
  # Remove pathways missing any co2-ffi data 
  filter(!is.na(terr_GtCO2FFI)) %>% 
  rename(terr_GtCO2FFI = terr_GtCO2FFI) %>%
  group_by(model, scen, r10) %>% 
  mutate(n = n()) %>% 
  filter(n == 81) %>% 
  select(-n) %>% 
  # Apply historical scaling
  group_by(model, scen, r10) %>%
  do(hist_scaling(., filter(r10_analysisdata, r10 == first(.$r10)), 
                  harmonisationyear = 2022, convergenceyear = 2050)) %>%
  ungroup() %>% 
  mutate(year = as.numeric(year))

# Prepare emissions pathways
r10_ar6_co2ffi_processed_scaled_long <- r10_ar6_co2ffi_processed_scaled %>% 
  group_by(model, scen, r10, cat) %>% 
  complete(year = 1990:2100) %>% 
  # Add historical data pre-harmonisation
  left_join(r10_analysisdata, by = c("r10", "year")) %>%
  mutate(terr_GtCO2FFI_histscale = ifelse(is.na(terr_GtCO2FFI_histscale), terr_GtCO2FFI.y, terr_GtCO2FFI_histscale),
         terr_GtCO2FFI_orig = ifelse(is.na(terr_GtCO2FFI.x), terr_GtCO2FFI.y, terr_GtCO2FFI.x)) %>% 
  select(model, scen, r10, cat, year, terr_GtCO2FFI_orig, terr_GtCO2FFI_histscale, pop) %>% 
  # Remove scenarios not reporting any emissions
  na.omit() %>% 
  group_by(model, scen, r10) %>% 
  mutate(
    # Set net-zero CO2-FFI year such that a buffer of 100Mt is used (for near-net-zero paths)
    netzeroyear = ifelse(min(terr_GtCO2FFI_histscale) > 0, 2110, year[which.max(terr_GtCO2FFI_histscale - 0.1 <= 0 & !is.na(terr_GtCO2FFI_histscale))]),
    # Set paths as 0 after hitting zero CO2-FFI (ignoring later resurgent use of CO2-FFI enabled through
    # negative emissions technologies)
    terr_GtCO2FFI = ifelse(terr_GtCO2FFI_histscale < 0 | year > netzeroyear, 0, terr_GtCO2FFI_histscale),
    terr_GtCO2FFI_orig = ifelse(terr_GtCO2FFI_orig < 0 | year > netzeroyear, 0, terr_GtCO2FFI_orig),
    peakyear = year[which.max(terr_GtCO2FFI)]) %>% 
  # Bin by 10 year periods from the half year (so 2026-3035 is in 2030)
  mutate(
    pkyearbin = ceiling((peakyear - 5)/10) * 10,
    nzyearbin = ceiling((netzeroyear - 5)/10) * 10) %>% 
  mutate(
    peakyearbin = paste("peak:", pkyearbin),
    netzeroyearbin = paste0("net-zero: ", nzyearbin)) %>% 
  # Select desired variables
  select(model, scen, cat, r10,	year, pop, terr_GtCO2FFI_orig,	terr_GtCO2FFI, 
         netzeroyear,	peakyear, pkyearbin,	nzyearbin,	peakyearbin,	netzeroyearbin)

# Visualise harmonisation
r10_ar6_co2ffi_processed_scaled_long %>% 
  filter(r10 == "R10NORTH_AM", nzyearbin == 2060, cat == "C2", pkyearbin == 2010,
         grepl("MESSAGE", model)) %>%
  pivot_longer(cols = matches("terr")) %>% 
  ggplot(aes(x = year, y = value, color = interaction(model, scen), linetype = name)) +
  geom_line(show.legend = F) +
  geom_line(show.legend = F, data = . %>% filter(year <= 2022, name == "terr_GtCO2FFI"), colour = "black")

# Determine cumulative CO2-FFI emissions 
r10_ar6_co2ffi_processed_scaled_cmltv <- r10_ar6_co2ffi_processed_scaled_long %>% 
  # Only retain years from 2022
  filter(year >= 2022) %>% 
  group_by(model, scen, r10, peakyearbin, netzeroyearbin, pkyearbin, nzyearbin) %>% 
  summarise(
            terr_GtCO2FFI_cmltv = sum(terr_GtCO2FFI),
            terr_GtCO2FFI_orig_cmltv = sum(terr_GtCO2FFI_orig)) %>% 
  group_by(r10, peakyearbin, netzeroyearbin, pkyearbin, nzyearbin) %>% 
  mutate(modelledpaths = n())

# Only keep scenarios where all region-years are represented
r10_ar6_co2ffi_processed_scaled_cmltv <- r10_ar6_co2ffi_processed_scaled_cmltv %>% 
  group_by(model, scen) %>% 
  mutate(complete = n() == 10) %>% 
  filter(complete == TRUE) %>% 
  select(-complete)
# Apply the same requirement to the long dataset
r10_ar6_co2ffi_processed_scaled_long <- 
  right_join(r10_ar6_co2ffi_processed_scaled_long,
             r10_ar6_co2ffi_processed_scaled_cmltv %>% distinct(model, scen))

# Write to file
write_csv(r10_ar6_co2ffi_processed_scaled_long, here("Data", "processed", "06_r10_ar6_gtco2ffi_processed_scaled.csv"))

# Determine change in cumulative emissions from the year 2023 onwards between
# harmonised and original modelled pathways (SI)
r10_ar6_co2ffi_processed_scaled_cmltv %>% 
  filter(!netzeroyearbin %in% c("net-zero: 2100", "net-zero: 2110")) %>% 
  mutate(percentage_change = terr_GtCO2FFI_cmltv / terr_GtCO2FFI_orig_cmltv - 1) %>% 
  mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label)) %>% 
  ungroup() %>% 
  group_by(nzyearbin) %>% 
  mutate(n = length(unique(scen))) %>% 
  ungroup() %>% 
  ggplot(aes(r10, percentage_change)) +
  geom_boxplot() +
  geom_text(aes(x = "NAM", y = 1, label = paste0("Scenarios: ", n)), hjust = 0,
            data = . %>% distinct(nzyearbin, n, .keep_all = T)) +
  coord_cartesian(ylim = c(-1,1)) +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~netzeroyearbin) +
  theme_bw() +
  labs(x = NULL, y = "Percentage change",
       caption = paste0(paste0(r10order$r10label[1:5], ": ",r10order$r10labellong[1:5], collapse = ", "), 
                        "\n", paste0(r10order$r10label[6:10], ": ",r10order$r10labellong[6:10], collapse = ", "), collapse = ""),
       subtitle = "Percentage change in regional (R10) cumulative CO2-FFI emissions 2023-2100 after harmonising to historical 2022 data and converging to modelled paths in 2050")

ggsave(here("Manuscript", "Figures", "SI", "SI_ar6harmonisation_cmltvco2ffi.png"),
       height = 8, width = 16)

# Visualise distributions of all combinations
for (region in r10order$r10) {
  
  data <- r10_ar6_co2ffi_processed_scaled_cmltv %>% 
    filter(r10 == region, !netzeroyearbin %in% c("net-zero: 2100", "net-zero: 2110")) %>%
    mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10labellong))
  
  fig <- data %>% 
    ggplot(aes(terr_GtCO2FFI_cmltv)) +
    geom_histogram() + 
    geom_vline(aes(xintercept = terr_GtCO2FFI_cmltv), linetype = 2, colour = "red",
               data = . %>% group_by(r10, netzeroyearbin, peakyearbin) %>% 
                 summarise(terr_GtCO2FFI_cmltv = median(terr_GtCO2FFI_cmltv))) +
    geom_text(aes(x = median(terr_GtCO2FFI_cmltv), y = 0, label = paste0("Total paths: ", modelledpaths)), size = 3, hjust = 0, vjust = -1,
              data = . %>% distinct(r10, .keep_all = T)) +
    facet_grid(peakyearbin~netzeroyearbin, scales = "free_y") +
    labs(y = "Number of paths", x = "Cumulative GtCO\U2082-FFI",
         title = unique(data$r10),
         subtitle = ) +
    theme_bw() 
  
  ggsave(fig, filename = here("Manuscript", "Figures", "SI", "AR6_paths",  
              paste0("SI_ar6_pathways_cmltv_", 
                     str_to_lower(str_remove(region, pattern = " ")), ".png")),
         height = 5, width = 14)
}

# Write to file
r10_ar6_co2ffi_processed_scaled_cmltv %>% 
  select(-modelledpaths) %>% 
  write_csv(here("Data", "processed", "07_r10_ar6_gtco2ffi_processed_scaled_cmltv.csv"))

# NET-ZERO CARBON DEBTS --------------------------------------------------------

r10_carbondebt_2100 <- r10_ar6_co2ffi_processed_scaled_cmltv %>% 
  arrange(r10, pkyearbin, nzyearbin) %>% 
  left_join(r10_rcb19902020 %>% 
              filter(year == 2020) %>% 
              select(r10, allocation, ppp_pf, rcb)) %>% 
  mutate(ppp_pf = ifelse(is.na(ppp_pf), "NA", ppp_pf),
         rcb2100_terr_GtCO2FFI_cmltv = rcb - terr_GtCO2FFI_cmltv) %>% 
  select(model, scen, r10, peakyearbin, pkyearbin, netzeroyearbin, nzyearbin, allocation, ppp_pf, 
         rcb2100_terr_GtCO2FFI_cmltv) %>% 
  arrange(model, scen, allocation, ppp_pf)
  
write_csv(r10_carbondebt_2100, here("Data", "processed", "08_r10_ar6_carbondebt_2100_gtco2ffi.csv"))

# FIGURE 2 ---------------------------------------------------------------------

r10_drawdown <- r10_carbondebt_2100 %>% 
  group_by(r10, nzyearbin, allocation, ppp_pf) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(allocation %in% c("PP1990"), n > 1) %>% 
  left_join(r10_analysisdata %>% 
              mutate(
                pop_cmltv = cumsum(pop),
                pop_cmltv_rem_20252100 = pop_cmltv[year == 2100] - pop_cmltv[year == 2025],
                pop_cmltv_rem_20252050 = pop_cmltv[year == 2050] - pop_cmltv[year == 2025]) %>% 
              filter(year == 2100) %>% 
              select(r10, pop_cmltv_rem_20252100, pop_cmltv_rem_20252050)) %>% 
  mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
         drawdowncap_2100 = rcb2100_terr_GtCO2FFI_cmltv * 1e9 / pop_cmltv_rem_20252100,
         drawdowncap_2050 = rcb2100_terr_GtCO2FFI_cmltv * 1e9 / pop_cmltv_rem_20252050)

r10_carbondebt_2100 %>% 
  group_by(r10, nzyearbin, allocation, ppp_pf) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(allocation %in% c("PP1990"), n > 1) %>% 
  mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label)) %>% 
  ggplot(aes(y = r10, x = rcb2100_terr_GtCO2FFI_cmltv, fill = factor(nzyearbin), group = r10)) +
  ggridges::geom_density_ridges(data = . %>% filter(nzyearbin == 2090), alpha = 0.7,
                                scale = 0.95, panel_scaling = F, rel_min_height = 0.01) +
  ggridges::geom_density_ridges(data = . %>% filter(nzyearbin == 2080), alpha = 0.7,
                                scale = 0.95, panel_scaling = F, rel_min_height = 0.01) +
  ggridges::geom_density_ridges(data = . %>% filter(nzyearbin == 2070) , alpha = 0.7,
                                scale = 0.95, panel_scaling = F, rel_min_height = 0.01) +
  ggridges::geom_density_ridges(data = . %>% filter(nzyearbin == 2060), alpha = 0.7,
                                scale = 0.95, panel_scaling = F, rel_min_height = 0.01) +
  ggridges::geom_density_ridges(data = . %>% filter(nzyearbin == 2050) , alpha = 0.7,
                                scale = 0.95, panel_scaling = F, rel_min_height = 0.01) +
  ggridges::geom_density_ridges(data = . %>% filter(nzyearbin == 2040), alpha = 0.7,
                                scale = 0.95, panel_scaling = F, rel_min_height = 0.01) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.3) +
  geom_point(aes(x = rcb2100_terr_GtCO2FFI_cmltv, fill = factor(nzyearbin)), size = 2, alpha = 0.7, shape = 21,
             data = r10_drawdown %>% filter(nzyearbin < 2100, nzyearbin > 2030) %>% 
               group_by(r10, nzyearbin) %>% filter(rcb2100_terr_GtCO2FFI_cmltv == rank_middle(rcb2100_terr_GtCO2FFI_cmltv))) +
  annotate(geom = "text", x = -20, y = "NAM", label = "Debt", vjust = 1.4, hjust = 1) +
  annotate(geom = "text", x = 20, y = "NAM", label = "Credit", vjust = 1.4, hjust = 0) +
  scale_fill_brewer(palette = "RdYlBu", direction = 1) +
  scale_x_continuous(breaks = seq(-800,500,100), position = "bottom") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        plot.subtitle = element_text(size = 14),
        plot.tag = element_text(size = 14),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        strip.background = element_blank()) +
  guides(fill = guide_legend(nrow = 1, reverse = F)) +
  labs(y = NULL, x = "Net-zero carbon debt (GtCO\U2082)",
       subtitle = "Net-zero carbon debt accrual in AR6 scenarios",
       fill = "Net-zero timing",
       caption = paste0(paste0(r10order$r10label[1:5], ": ",r10order$r10labellong[1:5], collapse = ", "), 
                                   "\n", paste0(r10order$r10label[6:10], ": ",r10order$r10labellong[6:10], collapse = ", "), collapse = "")) &
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        plot.subtitle = element_text(size = 14),
        plot.tag = element_text(size = 14),
        plot.caption = element_text(size = 6),
        strip.background = element_blank())

ggsave(filename = here("Manuscript", "Figures", "fig2.svg"),
       height = 6, width = 6.5)

# SI ---------------------------------------------------------------------------

fig2asi <- r10_carbondebt_2100 %>% 
  ungroup() %>% 
  filter(ppp_pf %in% c("NA", "PPP_1/sqrt(x)")) %>% 
  mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
         ppp_pf = factor(ppp_pf, levels = c("NA", "PPP_1/sqrt(x)"),
                         labels = c("Responsibility only",
                                    "Responsibility and Capability (PPP)")),
         allocation = case_when(
           grepl(allocation, pattern = "PP1990") ~ "PP1990",
           grepl(allocation, pattern = "PP2015") ~ "PP2015",
           TRUE ~ "PP1850")) %>% 
  filter(!allocation %in% c("PP2015")) %>% 
  ggplot(aes(y = r10, x = rcb2100_terr_GtCO2FFI_cmltv, fill = factor(nzyearbin), group = r10)) +
  ggridges::geom_density_ridges(data = . %>% filter(nzyearbin == 2090), alpha = 0.7,
                                scale = 0.95, panel_scaling = T, rel_min_height = 0.01) +
  ggridges::geom_density_ridges(data = . %>% filter(nzyearbin == 2080), alpha = 0.7,
                                scale = 0.95, panel_scaling = T, rel_min_height = 0.01) +
  ggridges::geom_density_ridges(data = . %>% filter(nzyearbin == 2070), alpha = 0.7,
                                scale = 0.95, panel_scaling = T, rel_min_height = 0.01) +
  ggridges::geom_density_ridges(data = . %>% filter(nzyearbin == 2060), alpha = 0.7,
                                scale = 0.95, panel_scaling = T, rel_min_height = 0.01) +
  ggridges::geom_density_ridges(data = . %>% filter(nzyearbin == 2050), alpha = 0.7,
                                scale = 0.95, panel_scaling = T, rel_min_height = 0.01) +
  ggridges::geom_density_ridges(data = . %>% filter(nzyearbin == 2040), alpha = 0.7,
                                scale = 0.95, panel_scaling = T, rel_min_height = 0.01) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.5) +
  annotate(geom = "text", x = 40, y = "NAM", label = "Credit", vjust = 0.5, hjust = 0) +
  annotate(geom = "text", x = -40, y = "NAM", label = "Debt", vjust = 0.5, hjust = 1) +
  scale_fill_brewer(palette = "RdYlBu", direction = 1) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_bw() +
  theme(legend.position = "top") +
  guides(fill = guide_legend(nrow = 1, reverse = F)) +
  labs(y = NULL, x = "Regional remaining carbon budget (GtCO\U2082)", 
       fill = "Regional net-zero CO\U2082-FFI year bin",
       caption = paste0(paste0(r10order$r10label[1:5], ": ",r10order$r10labellong[1:5], collapse = ", "), 
                        "\n", paste0(r10order$r10label[6:10], ": ",r10order$r10labellong[6:10], collapse = ", "), collapse = "")) +
  facet_grid(ppp_pf ~ allocation)

ggsave(plot = fig2asi, filename = here("Manuscript", "Figures", "SI", "SI_fig2a.png"),
       height = 7, width = 10)

fig2bsi <- r10_carbondebt_2100 %>% 
  group_by(model, scen, allocation, ppp_pf) %>%
  mutate(debt2100 = ifelse(rcb2100_terr_GtCO2FFI_cmltv<0,rcb2100_terr_GtCO2FFI_cmltv,0),
         scen_exceedance = sum(-rcb2100_terr_GtCO2FFI_cmltv),
         debtshare = ifelse(scen_exceedance < 0, 0, debt2100 / sum(debt2100))) %>% 
  ungroup() %>% 
  filter(ppp_pf %in% c("NA", "PPP_1/sqrt(x)"), nzyearbin < 2100,
         nzyearbin > 2030) %>% 
  left_join(r10_analysisdata %>% 
              mutate(
                pop_cmltv = cumsum(pop),
                pop_cmltv_rem_2025 = pop_cmltv[year == 2100] - pop_cmltv[year == 2025]) %>% 
              filter(year == 2100) %>% 
              select(r10, pop_cmltv_rem_2025)) %>% 
  mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
         ppp_pf = factor(ppp_pf, levels = c("NA", "PPP_1/sqrt(x)"),
                         labels = c("Responsibility only", 
                                    "Responsibility and Capability (PPP)")),
         allocation = case_when(
           grepl(allocation, pattern = "PP1990") ~ "PP1990",
           grepl(allocation, pattern = "PP2015") ~ "PP2015",
           TRUE ~ "PP1850")) %>% 
  filter(!allocation %in% c("PP2015")) %>% 
  group_by(r10, nzyearbin, allocation, ppp_pf) %>%
  filter(n() > 1) %>% 
  summarise(
    debtshare = median(debtshare),
    drawdowncap = median(debt2100 * 1e9 / pop_cmltv_rem_2025)) %>% 
  ggplot(aes(y = debtshare, 
             x = drawdowncap)) +
  geom_path(aes(group = r10), colour = "black", linetype = 2, alpha = 0.6,
            data = . %>% filter(!r10 %in% c("SAS", "AFR", "LAC", "PAS"))) +
  geom_point(alpha = 1, aes(colour = factor(nzyearbin)), size = 5, alpha = 0.6,
             data = . %>% filter(!r10 %in% c("SAS", "AFR", "LAC", "PAS"))) +
  geom_textpath(aes(label = r10, group = r10), colour = "black",
                text_only = TRUE, vjust = 1, straight = TRUE, hjust = 0, 
                data = . %>% filter(!r10 %in% c("SAS", "AFR", "LAC", "PAS"))) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks(),
                     labels = scales::percent_format()) +
  scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  facet_grid(ppp_pf ~ allocation) +
  theme_bw() +
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        legend.position = "top") +
  labs(x = "Median drawdown obligation (tCO\U2082 yr\U207B\U00B9, 2025-2100)",
       y = "Median responsibility for scenario overshoot",
       colour = "Regional net-zero CO\U2082-FFI timing",
       caption = paste0(paste0(r10order$r10label[1:5], ": ",r10order$r10labellong[1:5], collapse = ", "), 
                        "\n", paste0(r10order$r10label[6:10], ": ",r10order$r10labellong[6:10], collapse = ", "), collapse = "")) +
  guides(colour = guide_legend(nrow = 1, reverse = T)) +
  facet_grid(ppp_pf ~ allocation)

fig2bsi

ggsave(plot = fig2bsi, filename = here("Manuscript", "Figures", "SI", "SI_fig2b.png"),
       height = 7, width = 10)

fig2csi <- r10_ar6_co2ffi_processed_scaled_long %>% 
  mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label)) %>% 
  filter(r10 %in% c("NAM", "EAS") , nzyearbin %in% c(2050,2070)) %>% 
  right_join(r10_drawdown %>% filter(r10 %in% c("NAM", "EAS"), nzyearbin %in% c(2050,2070)) %>% 
               group_by(r10, nzyearbin) %>% filter(rcb2100_terr_GtCO2FFI_cmltv == rank_middle(rcb2100_terr_GtCO2FFI_cmltv))) %>% 
  mutate(terr_GtCO2FFI_cap = terr_GtCO2FFI * 1e9 / pop,
         drawdowncap_2100 = ifelse(year >= 2025, drawdowncap_2100, NA_real_),
         drawdowncap_2050 = ifelse(year >= 2025 & year <= 2050, drawdowncap_2050, NA_real_),
         r10 = fct_rev(factor(r10, levels = r10order$r10label)),
         netzeroyearbin = str_to_title(netzeroyearbin)) %>% 
  ggplot(aes(x = year)) +
  geom_path(aes(y = terr_GtCO2FFI_cap * pop / 1e9),
            data = . %>% filter(year <= 2022)) +
  geom_path(aes(y = terr_GtCO2FFI_cap * pop / 1e9),
            data = . %>% filter(year > 2022), linetype = 8) +
  geom_ribbon(aes(ymin = ifelse(year >= 2025, (value * pop) / 1e9, NA_real_), ymax = 0, fill = name),
              data = . %>% pivot_longer(cols = matches("drawdowncap")) %>% 
                mutate(name = factor(name, levels = c("drawdowncap_2050", "drawdowncap_2100"),
                                     labels = c("2050", "2100"))),
              alpha = 0.5) +
  geom_path(aes(y = ifelse(year >= 2025, (value * pop) / 1e9, NA_real_), colour = name),
            data = . %>% pivot_longer(cols = matches("drawdowncap")) %>% 
              mutate(name = factor(name, levels = c("drawdowncap_2050", "drawdowncap_2100"),
                                   labels = c("2050", "2100"))),
            alpha = 1) +
  geom_text(aes(x = 2060, y = -5, label = paste0((round(rcb2100_terr_GtCO2FFI_cmltv,1)), " GtCO2")),
            data = . %>% filter(year == 2060),
            hjust = 0.2, vjust = 0, size = 3) +
  scale_x_continuous(breaks = seq(2000,2100,25)) +
  scale_fill_manual(values = c("#8856a7", "#d95f02")) +
  scale_colour_manual(values = c("#8856a7", "#d95f02")) +
  labs(y = "GtCO\U2082yr\U207B\U00B9", x = NULL, fill = "Drawdown timeframe", colour = "Drawdown timeframe") +
  theme_bw() +
  theme(legend.position = "top") +
  facet_grid(r10~netzeroyearbin)

fig2csi

ggsave(plot = fig2csi, filename = here("Manuscript", "Figures", "SI", "SI_fig2c.png"),
       height = 4, width = 7)

for (i in r10order$r10label) {
  
  region <- r10order$r10labellong[which(r10order$r10label == i)]
  
  r10_ar6_co2ffi_processed_scaled_long %>% 
    mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label)) %>% 
    filter(r10 == i) %>% 
    right_join(r10_drawdown %>% filter(r10 == i) %>% 
                 group_by(r10, nzyearbin) %>% filter(rcb2100_terr_GtCO2FFI_cmltv == rank_middle(rcb2100_terr_GtCO2FFI_cmltv))) %>% 
    filter(nzyearbin < 2100) %>% 
    mutate(terr_GtCO2FFI_cap = terr_GtCO2FFI * 1e9 / pop,
           drawdowncap_2100 = ifelse(year >= 2025, drawdowncap_2100, NA_real_),
           drawdowncap_2050 = ifelse(year >= 2025 & year <= 2050, drawdowncap_2050, NA_real_),
           r10 = fct_rev(factor(r10, levels = r10order$r10label)),
           netzeroyearbin = str_to_title(netzeroyearbin),
           across(matches("drawdowncap"), ~ifelse(. > 0, 0, .))) %>% 
    ggplot(aes(x = year)) +
    geom_path(aes(y = terr_GtCO2FFI_cap * pop / 1e9),
              data = . %>% filter(year <= 2022)) +
    geom_path(aes(y = terr_GtCO2FFI_cap * pop / 1e9),
              data = . %>% filter(year > 2022), linetype = 8) +
    geom_ribbon(aes(ymin = ifelse(year >= 2025, (value * pop) / 1e9, NA_real_), ymax = 0, fill = name),
                data = . %>% pivot_longer(cols = matches("drawdowncap")) %>% 
                  mutate(name = factor(name, levels = c("drawdowncap_2050", "drawdowncap_2100"),
                                       labels = c("2050", "2100"))),
                alpha = 0.5) +
    geom_path(aes(y = ifelse(year >= 2025, (value * pop) / 1e9, NA_real_), colour = name),
              data = . %>% pivot_longer(cols = matches("drawdowncap")) %>% 
                mutate(name = factor(name, levels = c("drawdowncap_2050", "drawdowncap_2100"),
                                     labels = c("2050", "2100"))),
              alpha = 1) +
    geom_text(aes(x = 2100, 
                  y = max(terr_GtCO2FFI) / 2, 
                  label = paste0("RCB 2100: ", (round(rcb2100_terr_GtCO2FFI_cmltv,1)), " GtCO2")),
              data = . %>% filter(year == 2060),
              hjust = 1, vjust = -1, size = 3) +
    scale_x_continuous(breaks = seq(2000,2100,25)) +
    scale_fill_manual(values = c("#8856a7", "#d95f02")) +
    scale_colour_manual(values = c("#8856a7", "#d95f02")) +
    labs(y = "GtCO\U2082yr\U207B\U00B9", x = NULL, fill = "Drawdown timeframe", colour = "Drawdown timeframe",
         subtitle = paste0("Illustrative carbon drawdown obligations, ", region)) +
    theme_bw() +
    theme(legend.position = "bottom") +
    facet_wrap(~netzeroyearbin)
  
  ggsave(filename = here("Manuscript", "Figures", "SI", "AR6_paths", paste0("SI_AR6_drawdownobligations_",i,".png")),
         height = 5, width = 8)
  
}

