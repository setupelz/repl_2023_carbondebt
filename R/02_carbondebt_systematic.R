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
iso3c_tbl <- read_csv(here("Data", "countrygroups", "iso3c_region_mapping.csv")) %>% 
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
r10_rcb19902020gtco2 <- read_csv(here("Data", "processed", "r10_rcb19902020.csv")) %>% 
  filter(year >= 1990, year <= 2020) 

# Recent production-based emissions, aggregated to r10
r10_recent_prodco2 <- read_xlsx(here("Data", "processed", "analysisdata.xlsx"),
                            sheet = "recent_prodco2") %>% 
  group_by(r10, year) %>% 
  summarise(gtco2 = sum(gtco2)) %>% 
  arrange(year)

# Population, aggregated to r10
r10_popproj <- read_xlsx(here("Data", "processed", "analysisdata.xlsx"),
                     sheet = "popproj") %>% 
  group_by(r10, year) %>% 
  summarise(pop = sum(pop)) %>% 
  arrange(year)

# AR6 CO2-FFI data
r10_ar6_co2ffi <- read_csv(here("Data", "pathways", "ar6_all", "ar6_all_co2ffi.csv"))

# RCB quantities
rcb <- read_csv(here("Data", "processed", "rcbquantities.csv"))

# DETERMINE PEAK/NETZERO PATHWAYS USING AR6 DATABASE ---------------------------

# Pivot data and interpolate between modelled periods (linear)
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
            cat = Category, year = Year, gtco2 = mtco2 / 1e3, pop) %>% 
  group_by(model, scen, r10, cat, year) %>% 
  summarise(gtco2 = sum(gtco2))

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
r10_ar6_co2ffi_processed_scaled <- r10_ar6_co2ffi_processed %>%
  # Remove pathways missing any co2-ffi data 
  filter(!is.na(gtco2)) %>% 
  group_by(model, scen, r10) %>% 
  mutate(n = n()) %>% 
  filter(n == 81) %>% 
  select(-n) %>% 
  # Apply historical scaling
  group_by(model, scen, r10) %>%
  do(hist_scaling(., filter(r10_recent_prodco2, r10 == first(.$r10)), 
                  harmonisationyear = 2022, convergenceyear = 2050)) %>%
  ungroup() %>% 
  mutate(year = as.numeric(year))

# Determine cumulative CO2-FFI emissions distributions for each combination
r10_ar6_co2ffi_processed_scaled_cmltv <- r10_ar6_co2ffi_processed_scaled %>% 
  # Add historical data pre-harmonisation
  left_join(r10_recent_prodco2, by = c("r10", "year")) %>%
  mutate(gtco2_histscale = ifelse(is.na(gtco2_histscale), gtco2.y, gtco2_histscale)) %>% 
  select(model, scen, r10, cat, year, gtco2_orig = gtco2.x, gtco2_histscale) %>% 
  # Remove scenarios not reporting any emissions
  na.omit() %>% 
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
       caption = paste0(paste0(r10order$r10label[1:5], ": ",r10order$r10labellong[1:5], collapse = ", "), 
                        "\n", paste0(r10order$r10label[6:10], ": ",r10order$r10labellong[6:10], collapse = ", "), collapse = ""),
       subtitle = "Percentage change in regional (R10) cumulative CO2-FFI emissions 2023-2100 after harmonising to historical 2022 data and converging to modelled paths in 2050")

ggsave(here("Manuscript", "Figures", "SI", "SI_ar6harmonisation_cmltvco2ffi.png"),
       height = 8, width = 16)

# Visualise distributions of all combinations
for (region in r10order$r10) {
  
  data <- r10_ar6_co2ffi_processed_scaled_cmltv %>% 
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
r10_ar6_co2ffi_processed_scaled_cmltv %>% 
  write_csv(here("Data", "processed", "r10_ar6_gtco2ffi_cmltv.csv"))

# NET-ZERO CARBON DEBTS --------------------------------------------------------

r10_carbondebt_2100 <- r10_ar6_co2ffi_processed_scaled_cmltv %>% 
  arrange(r10, pkyearbin, nzyearbin) %>% 
  left_join(r10_rcb19902020gtco2 %>% 
              filter(year == 2020) %>% 
              select(r10, category, ppp_pf, rcb)) %>% 
  mutate(ppp_pf = ifelse(is.na(ppp_pf), "NA", ppp_pf),
         rcb2100_gtco2_cmltv = rcb - gtco2_cmltv,
         debt2100 = ifelse(rcb2100_gtco2_cmltv < 0, rcb2100_gtco2_cmltv, 0)) %>% 
  select(model, scen, r10, peakyearbin, pkyearbin, netzeroyearbin, nzyearbin, category, ppp_pf, 
         rcb2100_gtco2_cmltv, gtco2_cdr_cmltv, debt2100) %>% 
  group_by(model, scen, category, ppp_pf) %>% 
  mutate(debtshare = debt2100 / sum(debt2100),
         scen_exceedance = sum(-rcb2100_gtco2_cmltv),
         scen_exceedance = ifelse(scen_exceedance < 0, 0, scen_exceedance),
         scen_exceedace_share = ifelse(scen_exceedance == 0, 0, debtshare),
         scen_exceedance_resp = scen_exceedance * debtshare) %>% 
  arrange(model, scen, category, ppp_pf)
  
write_csv(r10_carbondebt_2100, here("Data", "processed", "r10_carbondebt_2100_gtco2.csv"))

# REVISED FIGURE 1 -------------------------------------------------------------

a <- r10_carbondebt_2100 %>% 
  
  filter(category %in% c("1_PP1990")) %>% 
  
  mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label)) %>% 
  
  ggplot(aes(y = r10, x = -rcb2100_gtco2_cmltv, fill = factor(nzyearbin), group = r10)) +
  
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
  
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.5) +
  
  annotate(geom = "text", x = -20, y = "NAM", label = "Credit", vjust = 0.5, hjust = 1) +
  
  annotate(geom = "text", x = 20, y = "NAM", label = "Debt", vjust = 0.5, hjust = 0) +
  
  scale_fill_brewer(palette = "RdYlBu", direction = 1) +
  
  scale_x_continuous(breaks = seq(-200,500,100), position = "bottom") +
  
  theme_bw() +
  
  theme(legend.position = "top") +
  
  guides(fill = guide_legend(nrow = 1, reverse = T)) +
  
  labs(y = NULL, x = "Regional net-zero carbon debt (GtCO2)", 
       fill = "Regional net-zero CO2-FFI year bin")

b <- r10_carbondebt_2100 %>% 
  
  filter(category %in% c("1_PP1990"), nzyearbin <= 2090, scen_exceedance > 0) %>% 
  
  left_join(r10_popproj %>% 
              mutate(pop_cmltv = cumsum(pop),
                     pop_cmltv_rem_2050 = pop_cmltv[year == 2100] - pop_cmltv[year == 2050]) %>% 
              filter(year == 2100) %>% 
              select(r10, pop_cmltv_rem_2050)) %>% 
  
  mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label)) %>%
  
  arrange(desc(nzyearbin)) %>% 
  
  ggplot(aes(x = scen_exceedace_share,
             colour = factor(nzyearbin),
             y = scen_exceedance_resp * 1e9 / pop_cmltv_rem_2050)) +
  
  geom_jitter(alpha = 0.5, size = 4, shape = 16) +
  
  facet_wrap(~fct_rev(r10), ncol = 2) +
  
  scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  
  theme_bw() +
  
  guides(colour = "none") +

  labs(y = "Required per capita exceedance drawdown rate (tCO2/capita/yr, 2050-2100)",
       x = "Responsibility for exceedance (%)")

wrap_plots(a,b, ncol = 2, widths = c(0.8,1)) + 
  plot_layout(guides = "collect", tag_level = "new") & 
  plot_annotation(tag_levels = list("a"), tag_prefix = "(", tag_suffix = ")", 
                  caption = paste0(paste0(r10order$r10label[1:5], ": ",r10order$r10labellong[1:5], collapse = ", "), 
                                   "\n", paste0(r10order$r10label[6:10], ": ",r10order$r10labellong[6:10], collapse = ", "), collapse = "")) &
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 13), plot.tag = element_text(size = 14),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        strip.background = element_blank())

ggsave(filename = here("Manuscript", "Figures", "fig1.png"),
       height = 14, width = 14)

# Figure 1A SI using other allocation approaches
fig1asi <- r10_carbondebt_2100 %>% 
  
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
  
  annotate(geom = "text", x = -40, y = "NAM", label = "Credit", vjust = 0.5, hjust = 1) +
  
  annotate(geom = "text", x = 40, y = "NAM", label = "Debt", vjust = 0.5, hjust = 0) +
  
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  
  theme_bw() +
  
  theme(legend.position = "top") +
  
  guides(fill = guide_legend(nrow = 1)) +
  
  labs(y = NULL, x = "Regional net-zero carbon debt (GtCO2)", 
       fill = "Regional net-zero CO2-FFI year bin",
       caption = paste0(paste0(r10order$r10label[1:5], ": ",r10order$r10labellong[1:5], collapse = ", "), 
                        "\n", paste0(r10order$r10label[6:10], ": ",r10order$r10labellong[6:10], collapse = ", "), collapse = "")) +
  facet_grid(ppp_pf ~ category)

ggsave(plot = fig1asi, filename = here("Manuscript", "Figures", "SI", "SI_fig1a.png"),
       height = 14, width = 14)

