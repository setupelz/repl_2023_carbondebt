# Replication archive for: "Delaying Carbon Debt Drawdown Fails Younger Generations"

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
iso3c_tbl <- read_csv(here("Data", "countrygroups", "iso3c_region_mapping.csv")) %>% 
  select(country.name, iso3c, r10 = iamc_r10) %>% 
  group_by(country.name, iso3c, r10) %>% 
  expand(year = 1850:2050) %>% 
  ungroup()

# Set consistent r10 ordering
r10order <- tibble(r10 = c("NAM", "EUR", "PAO", "FSU", "MEA", "EAS", "LAM", "PAS", "AFR", "SAS"),
                   r10label = c("NAM", "EUR", "APD", "EEA", "MEA", "EAS", "LAC", "SAP", "AFR", "SAS"),
                   r10labellong = c("North America", "Europe", "Asia-Pacific Developed",
                                    "Eastern Europe and West-Central Asia", "Middle East", "Eastern Asia",
                                    "Latin America and Caribbean", "South-East Asia and developing Pacific",
                                    "Africa", "Southern Asia"))

# Remaining carbon budgets from 1990 to 2020, aggregated to r10
r10_rcb19902020gtco2 <- read_csv(here("Data", "processed", "r10_rcb19902020.csv")) %>% 
  filter(year >= 1990, year <= 2020) 

# Recent production-based emissions
recent_prodco2 <- read_csv(here("Data", "processed", "iso3c_emiss19902022gtco2.csv")) %>% 
  select(country.name, iso3c, r10, year, gtco2)

r10recent_prodco2 <- recent_prodco2 %>% 
  group_by(r10, year) %>% 
  summarise(gtco2 = sum(gtco2)) %>% 
  arrange(year)

# Population
projected_pop <- read_csv(here("Data", "processed", "iso3c_popssp218502100.csv")) %>% 
  filter(r10 %in% r10order$r10, iso3c %in% iso3c_tbl$iso3c) 

# AR6 CO2-FFI data
ar6_co2ffi <- read_csv(here("Data", "pathways", "ar6_all", "ar6_all_co2ffi.csv"))

# RCB quantities
rcb <- read_csv(here("Data", "processed", "rcbquantities.csv"))

# DETERMINE PEAK/NETZERO PATHWAYS USING AR6 DATABASE ---------------------------

# Pivot data and interpolate between modelled periods (linear)
ar6_co2ffi_processed <- ar6_co2ffi %>% 
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
            r10 = case_when(
              Region == "R10AFRICA" ~ "AFR",
              Region == "R10PAC_OECD" ~ "PAO",
              Region == "R10EUROPE" ~ "EUR",
              Region == "R10INDIA+" ~ "SAS",
              Region == "R10LATIN_AM" ~ "LAM",
              Region == "R10MIDDLE_EAST" ~ "MEA",
              Region == "R10NORTH_AM" ~ "NAM",
              Region == "R10CHINA+" ~ "EAS",
              Region == "R10REF_ECON" ~ "FSU",
              Region == "R10REST_ASIA" ~ "PAS"),
            cat = Category, year = Year, gtco2 = mtco2 / 1e3, pop)

# Function to apply historical data scaling to each group, harmonising modelled
# pathways to historical 2022 values, converging to modelled pathways at a desired year.
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

# Apply scaling to all model-scenario-r10 groups
ar6_co2ffi_processed_scaled <- ar6_co2ffi_processed %>%
  # Remove regional pathways missing any co2-ffi data
  filter(!is.na(gtco2)) %>%
  group_by(model, scen, r10) %>% 
  mutate(n = n()) %>% 
  filter(n == 81) %>% 
  select(-n) %>% 
  # Apply historical scaling
  group_by(model, scen, r10) %>%
  do(hist_scaling(., filter(r10recent_prodco2, r10 == first(.$r10)), 
                  harmonisationyear = 2022, convergenceyear = 2050)) %>%
  ungroup() %>% 
  mutate(year = as.numeric(year))

# Determine cumulative CO2-FFI emissions distributions for each combination
ar6_co2ffi_processed_scaled_cmltv <- ar6_co2ffi_processed_scaled %>% 
  # Add historical data pre-harmonisation
  left_join(r10recent_prodco2, by = c("r10", "year")) %>%
  mutate(gtco2_histscale = ifelse(is.na(gtco2_histscale), gtco2.y, gtco2_histscale)) %>% 
  select(model, scen, r10, cat, year, pop, gtco2_orig = gtco2.x, gtco2_histscale) %>% 
  group_by(model, scen, r10) %>% 
  mutate(
    # Determine implied CDR
    gtco2_cdr = ifelse(gtco2_histscale < 0, -gtco2_histscale, 0),
    # Set net-zero CO2-FFI year such that a buffer of 100Mt is used (for near-net-zero paths)
    netzeroyear = ifelse(min(gtco2_histscale) > 0, 2110, year[which.max(gtco2_histscale - 0.1 <= 0 & !is.na(gtco2_histscale))]),
    # Set paths as 0 after hitting zero CO2-FFI (ignoring later resurgent use of CO2-FFI enabled through
    # negative emissions technologies)
    gtco2_histscale = ifelse(gtco2_histscale < 0 | year > netzeroyear, 0, gtco2_histscale),
    gtco2_orig = ifelse(gtco2_orig < 0 | year > netzeroyear, 0, gtco2_orig),
    peakyear = year[which.max(gtco2_histscale)],
    peakgtco2 = round(max(gtco2_histscale, na.rm = TRUE))) %>% 
  # Bin by 10 year periods from the half year (so 2026-3035 is in 2030)
  mutate(
    peakyearbin = ceiling((peakyear - 5)/10) * 10,
    netzeroyearbin = ceiling((netzeroyear - 5)/10) * 10) %>% 
  # Only retain years from 2022
  filter(year >= 2022) %>% 
  group_by(model, scen, r10, peakyearbin, netzeroyearbin) %>% 
  summarise(gtco2_cmltv = sum(gtco2_histscale),
            gtco2_orig_cmltv = sum(gtco2_orig),
            gtco2_cdr_cmltv = sum(gtco2_cdr)) %>% 
  group_by(r10, peakyearbin, netzeroyearbin) %>% 
  mutate(modelledpaths = n()) %>% 
  mutate(
    peakyearbin = paste("peak:", peakyearbin),
    netzeroyearbin = paste0("net-zero: ", netzeroyearbin),
    pkyearbin = str_remove(peakyearbin, "peak: ") %>% as.numeric(),
    nzyearbin = str_remove(netzeroyearbin, "net-zero: ") %>% as.numeric()) %>% 
  filter(
    nzyearbin %in% c(2040, 2050, 2060, 2070, 2080, 2090))

# Determine change in cumulative emissions from the year 2023 onwards between
# harmonised and original modelled pathways (SI)
ar6_co2ffi_processed_scaled_cmltv %>% 
  mutate(percentage_change = gtco2_cmltv / gtco2_orig_cmltv - 1) %>% 
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
       subtitle = "Percentage change in regional (R10) cumulative CO2-FFI emissions 2023-2100 after harmonising to historical 2022 data and converging to modelled paths in 2050")

ggsave(here("Manuscript", "Figures", "SI", "SI_ar6harmonisation_cmltvco2ffi.png"),
       height = 8, width = 16)

# Visualise distributions of all combinations
for (region in r10order$r10) {
  
  data <- ar6_co2ffi_processed_scaled_cmltv %>% 
    filter(r10 == region) %>%
    mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10labellong))
  
  data %>% 
    ggplot(aes(gtco2_cmltv)) +
    geom_histogram() + 
    geom_vline(aes(xintercept = gtco2_cmltv), linetype = 2, colour = "red",
               data = . %>% group_by(r10, netzeroyearbin, peakyearbin) %>% 
                 summarise(gtco2_cmltv = median(gtco2_cmltv))) +
    geom_text(aes(x = median(gtco2_cmltv), y = 0, label = paste0("Total paths: ", modelledpaths)), size = 3, hjust = 0, vjust = -1,
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
ar6_co2ffi_processed_scaled_cmltv %>% 
  write_csv(here("Data", "processed", "r10_ar6_gtco2ffi_cmltv.csv"))

# NET-ZERO CARBON DEBTS --------------------------------------------------------

carbondebt_2100 <- ar6_co2ffi_processed_scaled_cmltv %>% 
  arrange(r10, pkyearbin, nzyearbin) %>% 
  left_join(r10_rcb19902020gtco2 %>% 
              filter(year == 2020) %>% 
              select(r10, category, ppp_pf, rcb)) %>% 
  mutate(ppp_pf = ifelse(is.na(ppp_pf), "NA", ppp_pf),
         rcb2100_gtco2_cmltv = rcb - gtco2_cmltv) %>% 
  select(model, scen, r10, peakyearbin, pkyearbin, netzeroyearbin, nzyearbin, category, ppp_pf, 
         rcb2100_gtco2_cmltv, gtco2_cdr_cmltv)
  
write_csv(carbondebt_2100, here("Data", "processed", "r10_carbondebt_2100_gtco2.csv"))

# REVISED FIGURE 1 -------------------------------------------------------------

a <- carbondebt_2100 %>% 
  
  filter(category %in% c("1_PP1990")) %>% 
  
  mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label)) %>% 
  
  ggplot(aes(y = r10, x = -rcb2100_gtco2_cmltv, fill = factor(nzyearbin), group = r10)) +
  
  ggridges::geom_density_ridges(data = . %>% filter(nzyearbin == 2090), alpha = 0.7,
                                scale = 0.95, panel_scaling = F, rel_min_height = 0.01) +
  ggridges::geom_density_ridges(data = . %>% filter(nzyearbin == 2080), alpha = 0.7,
                                scale = 0.95, panel_scaling = F, rel_min_height = 0.01) +
  ggridges::geom_density_ridges(data = . %>% filter(nzyearbin == 2070), alpha = 0.7,
                                scale = 0.95, panel_scaling = F, rel_min_height = 0.01) +
  ggridges::geom_density_ridges(data = . %>% filter(nzyearbin == 2060), alpha = 0.7,
                                scale = 0.95, panel_scaling = F, rel_min_height = 0.01) +
  ggridges::geom_density_ridges(data = . %>% filter(nzyearbin == 2050), alpha = 0.7,
                                scale = 0.95, panel_scaling = F, rel_min_height = 0.01) +
  ggridges::geom_density_ridges(data = . %>% filter(nzyearbin == 2040), alpha = 0.7,
                                scale = 0.95, panel_scaling = F, rel_min_height = 0.01) +
  
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.5) +
  
  annotate(geom = "text", x = -20, y = "NAM", label = "Credit", vjust = 0.5, hjust = 1) +
  
  annotate(geom = "text", x = 20, y = "NAM", label = "Debt", vjust = 0.5, hjust = 0) +
  
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), position = "bottom") +
  
  theme_bw() +
  
  theme(legend.position = "top") +
  
  guides(fill = guide_legend(nrow = 1)) +
  
  labs(y = NULL, x = "Regional net-zero carbon debt (GtCO2)", 
       fill = "Regional net-zero CO2-FFI year bin")

b <- carbondebt_2100 %>% 
  
  filter(category %in% c("1_PP1990")) %>% 
  
  mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
         rcb2100_gtco2_cmltv = ifelse(rcb2100_gtco2_cmltv > 0, 0, rcb2100_gtco2_cmltv)) %>%
  
  ggplot(aes(x = -rcb2100_gtco2_cmltv / (2100 - nzyearbin), 
             y = gtco2_cdr_cmltv / (2100 - nzyearbin), 
             colour = factor(nzyearbin))) +
  
  geom_point(alpha = 0.5) +
  
  geom_textabline(label = "Identity", linetype = 2, alpha = 0.6, size = 2) +
  
  scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  
  facet_wrap(~fct_rev(r10), ncol = 2) +
  
  theme_bw() +
  
  guides(colour = "none") +
  
  labs(y = "Average annual net-negative novel CDR in scenario pathway (GtCO2/yr)",
       x = "Average annual carbon debt drawdown required by pathway (GtCO2/yr)")

wrap_plots(a,b, ncol = 2, widths = c(0.7,1)) + 
  plot_layout(guides = "collect", tag_level = "new") & 
  plot_annotation(tag_levels = list("a"), tag_prefix = "(", tag_suffix = ")", 
                  caption = "NAM: North America, EUR: Europe, APD: Asia-Pacific Developed, EEA: Eastern Europe and West-Central Asia, MEA: Middle East\nEAS: Eastern Asia, LAC: Latin America and Caribbean, SAP: South-East Asia and developing Pacific, AFR: Africa, SAS: Southern Asia") & 
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 13), plot.tag = element_text(size = 14),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        strip.background = element_blank())

ggsave(filename = here("Manuscript", "Figures", "fig1.png"),
       height = 8, width = 11)

# Figure 1A SI using other allocation approaches
fig1asi <- carbondebt_2100 %>% 
  
  filter(ppp_pf %in% c("NA", "MER_1/sqrt(x)", "PPP_1/sqrt(x)")) %>% 
  
  mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
         ppp_pf = factor(ppp_pf, levels = c("NA", "MER_1/sqrt(x)", "PPP_1/sqrt(x)")),
         category = ifelse(grepl(category, pattern = "PP1990"), "PP1990", "PP1850")) %>% 
  
  ggplot(aes(y = r10, x = -rcb2100_gtco2_cmltv, fill = factor(nzyearbin), group = r10)) +
  
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
  
  annotate(geom = "text", x = -40, y = "NAM", label = "No debt", vjust = 0.5, hjust = 1) +
  
  annotate(geom = "text", x = 40, y = "NAM", label = "Debt", vjust = 0.5, hjust = 0) +
  
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  
  theme_bw() +
  
  theme(legend.position = "top") +
  
  guides(fill = guide_legend(nrow = 1)) +
  
  labs(y = NULL, x = "Regional net-zero carbon debt (GtCO2)", 
       fill = "Regional net-zero CO2-FFI year bin") +
  
  facet_grid(ppp_pf ~ category)

ggsave(plot = fig1asi, filename = here("Manuscript", "Figures", "SI", "SI_fig1a.png"),
       height = 10, width = 14)
