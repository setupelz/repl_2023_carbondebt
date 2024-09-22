# Replication archive for: "Using net-zero carbon debt to track climate overshoot responsibility"

# Contact for clarifications: [ANONYMISED]       

# Script contents: Systematically explore carbon debt accrual using the AR6 database

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
r10_rcb19912020 <- read_csv(here("Data", "processed", "r10_rcb19912020.csv"))

# Analysis dataset, aggregated to R10
r10_analysisdata <- read_csv(here("Data", "processed", "2_analysisdata.csv")) %>%
  filter(iso3c != "ROW", year >= 1990) %>% 
  select(-iso3c) %>% 
  group_by(r10, year) %>% 
  summarise(across(everything(), ~ sum(.))) %>% 
  arrange(year)

# AR6 CO2-FFI data
r10_ar6_co2ffi <- read_csv(here("Data", "pathways", "ar6_all", "ar6_all_co2ffi.csv"))

# RCB quantities
rcb <- read_csv(here("Data", "processed", "rcbquantities.csv"))

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
            cat = Category, year = Year, terr_GtCO2 = mtco2 / 1e3, pop) %>% 
  group_by(model, scen, r10, cat, year) %>% 
  summarise(terr_GtCO2 = sum(terr_GtCO2))

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

# Apply scaling to all model-scenario-r10 groups
r10_ar6_co2ffi_processed_scaled <- r10_ar6_co2ffi_processed %>%
  # Remove pathways missing any co2-ffi data 
  filter(!is.na(terr_GtCO2)) %>% 
  rename(terr_GtCO2 = terr_GtCO2) %>%
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

# Write to file
write_csv(r10_ar6_co2ffi_processed_scaled, here("Data", "processed", "r10_ar6_co2ffi_processed_scaled.csv"))

# Determine cumulative CO2-FFI emissions distributions for each combination
r10_ar6_co2ffi_processed_scaled_cmltv <- r10_ar6_co2ffi_processed_scaled %>% 
  # Add historical data pre-harmonisation
  left_join(r10_analysisdata %>% filter(year >= 2020), by = c("r10", "year")) %>%
  mutate(terr_GtCO2_histscale = ifelse(is.na(terr_GtCO2_histscale), terr_GtCO2.y, terr_GtCO2_histscale)) %>% 
  select(model, scen, r10, cat, year, terr_GtCO2_orig = terr_GtCO2.x, terr_GtCO2_histscale) %>% 
  # Remove scenarios not reporting any emissions
  na.omit() %>% 
  group_by(model, scen, r10) %>% 
  mutate(
    # Determine implied CDR
    terr_GtCO2_cdr = ifelse(terr_GtCO2_histscale < 0, -terr_GtCO2_histscale, 0),
    # Set net-zero CO2-FFI year such that a buffer of 100Mt is used (for near-net-zero paths)
    netzeroyear = ifelse(min(terr_GtCO2_histscale) > 0, 2110, year[which.max(terr_GtCO2_histscale - 0.1 <= 0 & !is.na(terr_GtCO2_histscale))]),
    # Set paths as 0 after hitting zero CO2-FFI (ignoring later resurgent use of CO2-FFI enabled through
    # negative emissions technologies)
    terr_GtCO2_histscale = ifelse(terr_GtCO2_histscale < 0 | year > netzeroyear, 0, terr_GtCO2_histscale),
    terr_GtCO2_orig = ifelse(terr_GtCO2_orig < 0 | year > netzeroyear, 0, terr_GtCO2_orig),
    peakyear = year[which.max(terr_GtCO2_histscale)],
    peakterr_GtCO2 = round(max(terr_GtCO2_histscale, na.rm = TRUE))) %>% 
  # Bin by 10 year periods from the half year (so 2026-3035 is in 2030)
  mutate(
    peakyearbin = ceiling((peakyear - 5)/10) * 10,
    netzeroyearbin = ceiling((netzeroyear - 5)/10) * 10) %>% 
  # Only retain years from 2022
  filter(year >= 2022) %>% 
  group_by(model, scen, r10, peakyearbin, netzeroyearbin) %>% 
  summarise(terr_GtCO2_cmltv = sum(terr_GtCO2_histscale),
            terr_GtCO2_orig_cmltv = sum(terr_GtCO2_orig),
            terr_GtCO2_cdr_cmltv = sum(terr_GtCO2_cdr)) %>% 
  group_by(r10, peakyearbin, netzeroyearbin) %>% 
  mutate(modelledpaths = n()) %>% 
  mutate(
    peakyearbin = paste("peak:", peakyearbin),
    netzeroyearbin = paste0("net-zero: ", netzeroyearbin),
    pkyearbin = str_remove(peakyearbin, "peak: ") %>% as.numeric(),
    nzyearbin = str_remove(netzeroyearbin, "net-zero: ") %>% as.numeric())

# Only keep scenarios where all region-years are represented
r10_ar6_co2ffi_processed_scaled_cmltv <- r10_ar6_co2ffi_processed_scaled_cmltv %>% 
  group_by(model, scen) %>% 
  mutate(complete = n() == 10) %>% 
  filter(complete == TRUE) %>% 
  select(-complete)

# Determine change in cumulative emissions from the year 2023 onwards between
# harmonised and original modelled pathways (SI)
r10_ar6_co2ffi_processed_scaled_cmltv %>% 
  mutate(percentage_change = terr_GtCO2_cmltv / terr_GtCO2_orig_cmltv - 1) %>% 
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
  
  data %>% 
    ggplot(aes(terr_GtCO2_cmltv)) +
    geom_histogram() + 
    geom_vline(aes(xintercept = terr_GtCO2_cmltv), linetype = 2, colour = "red",
               data = . %>% group_by(r10, netzeroyearbin, peakyearbin) %>% 
                 summarise(terr_GtCO2_cmltv = median(terr_GtCO2_cmltv))) +
    geom_text(aes(x = median(terr_GtCO2_cmltv), y = 0, label = paste0("Total paths: ", modelledpaths)), size = 3, hjust = 0, vjust = -1,
              data = . %>% distinct(r10, .keep_all = T)) +
    facet_grid(peakyearbin~netzeroyearbin, scales = "free_y") +
    labs(y = "Number of paths", x = "Cumulative GtCO2-FFI",
         title = unique(data$r10),
         subtitle = ) +
    theme_bw() 
  
  ggsave(here("Manuscript", "Figures", "SI", "AR6_paths",  
              paste0("SI_ar6_pathways_cmltv_", 
                     str_to_lower(str_remove(region, pattern = " ")), ".png")),
         height = 5, width = 14)
}

# Write to file
r10_ar6_co2ffi_processed_scaled_cmltv %>% 
  write_csv(here("Data", "processed", "r10_ar6_terr_GtCO2ffi_cmltv.csv"))

# NET-ZERO CARBON DEBTS --------------------------------------------------------

r10_carbondebt_2100 <- r10_ar6_co2ffi_processed_scaled_cmltv %>% 
  arrange(r10, pkyearbin, nzyearbin) %>% 
  left_join(r10_rcb19912020 %>% 
              filter(year == 2020) %>% 
              select(r10, category, ppp_pf, rcb)) %>% 
  mutate(ppp_pf = ifelse(is.na(ppp_pf), "NA", ppp_pf),
         rcb2100_terr_GtCO2_cmltv = rcb - terr_GtCO2_cmltv,
         debt2100 = ifelse(rcb2100_terr_GtCO2_cmltv < 0, rcb2100_terr_GtCO2_cmltv, 0)) %>% 
  select(model, scen, r10, peakyearbin, pkyearbin, netzeroyearbin, nzyearbin, category, ppp_pf, 
         rcb2100_terr_GtCO2_cmltv, terr_GtCO2_cdr_cmltv, debt2100) %>% 
  group_by(model, scen, category, ppp_pf) %>% 
  mutate(debtshare = debt2100 / sum(debt2100),
         scen_exceedance = sum(-rcb2100_terr_GtCO2_cmltv),
         scen_exceedance = ifelse(scen_exceedance < 0, 0, scen_exceedance),
         scen_exceedace_share = ifelse(scen_exceedance == 0, 0, debtshare),
         scen_exceedance_resp = scen_exceedance * debtshare) %>% 
  arrange(model, scen, category, ppp_pf)
  
write_csv(r10_carbondebt_2100, here("Data", "processed", "r10_carbondebt_2100_terr_GtCO2.csv"))

# REVISED FIGURE 1 -------------------------------------------------------------

a <- r10_carbondebt_2100 %>% 
  
  filter(category %in% c("PP1990")) %>% 
  
  mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label)) %>% 
  
  ggplot(aes(y = r10, x = rcb2100_terr_GtCO2_cmltv, fill = factor(nzyearbin), group = r10)) +
  
  ggridges::geom_density_ridges(data = . %>% filter(nzyearbin == 2090) %>% complete(r10), alpha = 0.7,
                                scale = 0.95, panel_scaling = F, rel_min_height = 0.01) +
  ggridges::geom_density_ridges(data = . %>% filter(nzyearbin == 2080) %>% complete(r10), alpha = 0.7,
                                scale = 0.95, panel_scaling = F, rel_min_height = 0.01) +
  ggridges::geom_density_ridges(data = . %>% filter(nzyearbin == 2070) %>% complete(r10), alpha = 0.7,
                                scale = 0.95, panel_scaling = F, rel_min_height = 0.01) +
  ggridges::geom_density_ridges(data = . %>% filter(nzyearbin == 2060) %>% complete(r10), alpha = 0.7,
                                scale = 0.95, panel_scaling = F, rel_min_height = 0.01) +
  ggridges::geom_density_ridges(data = . %>% filter(nzyearbin == 2050) %>% complete(r10), alpha = 0.7,
                                scale = 0.95, panel_scaling = F, rel_min_height = 0.01) +
  ggridges::geom_density_ridges(data = . %>% filter(nzyearbin == 2040) %>% complete(r10), alpha = 0.7,
                                scale = 0.95, panel_scaling = F, rel_min_height = 0.01) +
  
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.3) +
  
  annotate(geom = "text", x = -20, y = "NAM", label = "Debt", vjust = 0.5, hjust = 1) +
  
  annotate(geom = "text", x = 20, y = "NAM", label = "Credit", vjust = 0.5, hjust = 0) +
  
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
  
  guides(fill = guide_legend(nrow = 1, reverse = T)) +
  
  labs(y = NULL, x = "GtCO2",
       subtitle = "Regional carbon budget remaining, 2100",
       fill = "Regional net-zero CO2-FFI year bin")

b <- r10_carbondebt_2100 %>% 
  
  filter(category %in% c("PP1990"), nzyearbin <= 2090, scen_exceedance > 0) %>% 
  
  left_join(r10_analysisdata %>% 
              mutate(pop_cmltv = cumsum(pop),
                     pop_cmltv_rem_2050 = pop_cmltv[year == 2100] - pop_cmltv[year == 2050]) %>% 
              filter(year == 2100) %>% 
              select(r10, pop_cmltv_rem_2050)) %>% 
  
  mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label)) %>%
  
  mutate(figgroup = case_when(
    r10 %in% c("NAM", "EUR", "SAS", "AFR") ~ "NAM",
    r10 %in% c("EAS", "LAC") ~ "EAS")) %>% 
  
  filter(!is.na(figgroup)) %>% 
  
  arrange(desc(nzyearbin)) %>% 
  
  ggplot(aes(x = scen_exceedace_share,
             colour = factor(nzyearbin), shape = r10,
             y = -scen_exceedance_resp * 1e9 / pop_cmltv_rem_2050)) +
  
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.3) +
  
  geom_jitter(alpha = 0.5, size = 4) +
  
  facet_wrap(~fct_rev(figgroup), ncol = 2) +
  
  scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  
  theme_bw() +
  
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  
  guides(colour = "none", shape = "none") +

  labs(y = "tCO2 / person / year",
       subtitle = "Median annual per capita debt drawdown rate required, 2050-2100",
       x = "Share of total scenario budget exceedance (%)")

wrap_plots(a,b, ncol = 2, widths = c(0.7,1.1)) + 
  plot_layout(guides = "collect", tag_level = "new") & 
  plot_annotation(tag_levels = list("a"), tag_prefix = "(", tag_suffix = ")", 
                  caption = paste0(paste0(r10order$r10label[1:5], ": ",r10order$r10labellong[1:5], collapse = ", "), 
                                   "\n", paste0(r10order$r10label[6:10], ": ",r10order$r10labellong[6:10], collapse = ", "), collapse = "")) &
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        strip.text = element_blank(),
        plot.subtitle = element_text(size = 14),
        plot.tag = element_text(size = 14),
        strip.background = element_blank())

ggsave(filename = here("Manuscript", "Figures", "fig1.svg"),
       height = 7, width = 12)

# Figure 1A SI using other allocation approaches
fig1asi <- r10_carbondebt_2100 %>% 
  
  ungroup() %>% 
  
  filter(ppp_pf %in% c("NA", "MER_1/sqrt(x)", "PPP_1/sqrt(x)")) %>% 
  
  mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
         ppp_pf = factor(ppp_pf, levels = c("NA", "MER_1/sqrt(x)", "PPP_1/sqrt(x)")),
         category = case_when(
           grepl(category, pattern = "PP1990") ~ "PP1990",
           grepl(category, pattern = "PP2015") ~ "PP2015",
           TRUE ~ "PP1850")) %>% 
  
  ggplot(aes(y = r10, x = -rcb2100_terr_GtCO2_cmltv, fill = factor(nzyearbin), group = r10)) +
  
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
  
  annotate(geom = "text", x = -40, y = "NAM", label = "Credit", vjust = 0.5, hjust = 1) +
  
  annotate(geom = "text", x = 40, y = "NAM", label = "Debt", vjust = 0.5, hjust = 0) +
  
  scale_fill_brewer(palette = "RdYlBu", direction = 1) +
  
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  
  theme_bw() +
  
  theme(legend.position = "top") +
  
  guides(fill = guide_legend(nrow = 1, reverse = T)) +
  
  labs(y = NULL, x = "Regional net-zero carbon debt (GtCO2)", 
       fill = "Regional net-zero CO2-FFI year bin",
       caption = paste0(paste0(r10order$r10label[1:5], ": ",r10order$r10labellong[1:5], collapse = ", "), 
                        "\n", paste0(r10order$r10label[6:10], ": ",r10order$r10labellong[6:10], collapse = ", "), collapse = "")) +
  facet_grid(ppp_pf ~ category)

ggsave(plot = fig1asi, filename = here("Manuscript", "Figures", "SI", "SI_fig1a.png"),
       height = 14, width = 14)

# Fig 1b SI

r10_carbondebt_2100 %>% 
  
  filter(category %in% c("PP1990"), nzyearbin <= 2090, scen_exceedance > 0) %>% 
  
  left_join(r10_analysisdata %>% 
              mutate(pop_cmltv = cumsum(pop),
                     pop_cmltv_rem_2050 = pop_cmltv[year == 2100] - pop_cmltv[year == 2050]) %>% 
              filter(year == 2100) %>% 
              select(r10, pop_cmltv_rem_2050)) %>% 
  
  mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label)) %>%
  
  arrange(desc(nzyearbin)) %>% 
  
  ggplot(aes(x = scen_exceedace_share,
             colour = factor(nzyearbin),
             y = -scen_exceedance_resp * 1e9 / pop_cmltv_rem_2050)) +
  
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.3) +
  
  geom_jitter(alpha = 0.5, size = 4, shape = 16) +
  
  facet_wrap(~fct_rev(r10), ncol = 2) +
  
  scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  
  theme_bw() +
  
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  
  guides(colour = "none") +
  
  labs(y = "tCO2 / person / year",
       subtitle = "Minimum annual per capita exceedance drawdown rate 2050-2100",
       x = "Responsibility for exceedance (%)")
