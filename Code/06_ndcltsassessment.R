# Replication archive for: "Using net-zero carbon debt to track climate overshoot responsibility"

# Contact for clarifications: Setu Pelz (pelz@iiasa.ac.at)      

# Script contents: Evaluate assessed scenarios

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

# Analysis dataset, aggregated to R10
r10_analysisdata <- read_csv(here("Data", "processed", "02_iso3c_indicators.csv")) %>%
  filter(iso3c != "ROW", year >= 1990) %>% 
  select(-iso3c) %>% 
  group_by(r10, year) %>% 
  summarise(across(everything(), ~ sum(.))) %>% 
  arrange(year)

# Calculate population from year to 2050
r10_poprem <- tibble(.rows = 0)

for (i in 1990:2020) {
  
  pop_rem_loop <- r10_analysisdata %>% 
    filter(year >= i & year <= 2050) %>% 
    summarise(pop_yearto2050 = sum(pop, na.rm = T)) %>% 
    mutate(year = i)
  
  r10_poprem <- rbind(r10_poprem, pop_rem_loop)
  
}

# RCB quantities
rcb <- read_csv(here("Data", "processed", "03_rcbquantities.csv"))

# Regional RCBs from 1990 to 2020
r10_rcb19902020 <- read_csv(here("Data", "processed", "05_r10_rcb19902020gtco2ffi.csv")) 

# Assessed pathways
r10_curpol_curpledge_impren_emiss <- read_csv(here("Data", "processed", "10_r10_curpol_curpledge_impren_emiss.csv"))

# Heatwave EMFs and additional years
r10_exp_heatwave_emf <- read_csv(here("Data", "processed", "12_r10_exp_heatwave_emf.csv"))

# Temperatures
global_temps <- read_csv(here("Data", "processed", "11_curpol_curpledge_impren_temp.csv"))

# ASSESS MODELLED PATHWAYS -----------------------------------------------------

# Determine regional remaining carbon budget allocation evolution along assessed paths
r10_curpol_curpledge_impren_rcbyear <- r10_curpol_curpledge_impren_emiss %>%
  filter(year >= 1990) %>% 
  mutate(terr_GtCO2FFI = ifelse(terr_GtCO2FFI < 0 , 0, terr_GtCO2FFI)) %>% 
  group_by(case, r10) %>% 
  mutate(terr_GtCO2FFI_cmltv_1990 = cumsum(terr_GtCO2FFI),
         terr_GtCO2FFI_cmltv_2015 = cumsum(ifelse(year >= 2015, terr_GtCO2FFI, 0))) %>% 
  left_join(r10_rcb19902020 %>% distinct(r10, allocation, ppp_pf)) %>% 
  left_join(r10_rcb19902020 %>% filter(year == 1990, grepl(allocation, pattern = "1990|1850")) %>% 
              select(r10, allocation, ppp_pf, rcb1990 = rcb)) %>% 
  left_join(r10_rcb19902020 %>% filter(year == 2015, grepl(allocation, pattern = "2015")) %>% 
              select(r10, allocation, ppp_pf, rcb2015 = rcb)) %>% 
  arrange(model, case, r10, allocation, ppp_pf, year) %>% 
  group_by(model, case, r10, allocation, ppp_pf) %>%
  mutate(rcbyear = ifelse(grepl(allocation, pattern = "2015"), 
          rcb2015 - lag(terr_GtCO2FFI_cmltv_2015, default = 0),
          rcb1990 - lag(terr_GtCO2FFI_cmltv_1990, default = 0))) %>% 
  arrange(model, case, allocation, ppp_pf, r10, year) %>% 
  group_by(model, case, allocation, ppp_pf, year) %>% 
  mutate(scenexceedanceyear = ifelse(sum(-rcbyear) > 0, sum(-rcbyear), 0),
         debtyear = ifelse(-rcbyear > 0, -rcbyear, 0)) %>% 
  group_by(model, case, allocation, ppp_pf, year) %>% 
  mutate(scenexceedanceshareyear = (debtyear / sum(debtyear) * scenexceedanceyear) / scenexceedanceyear,
         scenexceedanceshareyear = ifelse(is.nan(scenexceedanceshareyear), NA_real_, scenexceedanceshareyear)) %>% 
  arrange(model, case, r10, allocation, ppp_pf, year) %>% 
  left_join(r10_analysisdata %>% select(r10, year, pop)) %>% 
  select(model, case, r10, year, terr_GtCO2FFI, allocation, ppp_pf, rcbyear, scenexceedanceyear, debtyear, scenexceedanceshareyear, pop)

# Write to file for SI
write_csv(r10_curpol_curpledge_impren_rcbyear, here("Data", "processed", "13_r10_curpol_curpledge_impren_rcbyear.csv"))

# Determine regional end of century carbon debt associated with assessed paths
r10_curpol_curpledge_impren_debt <- r10_curpol_curpledge_impren_emiss %>%
  filter(year >= 1990) %>% 
  mutate(cdr_GtCO2 = ifelse(terr_GtCO2FFI < 0, -terr_GtCO2FFI, 0),
         terr_GtCO2FFI = ifelse(terr_GtCO2FFI < 0 , 0, terr_GtCO2FFI)) %>% 
  group_by(case, r10) %>% 
  mutate(terr_GtCO2FFI_cmltv_1990 = cumsum(terr_GtCO2FFI),
         terr_GtCO2FFI_cmltv_2015 = cumsum(ifelse(year >= 2015, terr_GtCO2FFI, 0)),
         cdr_GtCO2_cmltv = cumsum(cdr_GtCO2)) %>% 
  filter(year == 2100) %>% 
  left_join(r10_rcb19902020 %>% distinct(r10, allocation, ppp_pf)) %>% 
  left_join(r10_rcb19902020 %>% filter(year == 1990, grepl(allocation, pattern = "1990|1850")) %>% 
              select(r10, allocation, ppp_pf, rcb1990 = rcb)) %>% 
  left_join(r10_rcb19902020 %>% filter(year == 2015, grepl(allocation, pattern = "2015")) %>% 
              select(r10, allocation, ppp_pf, rcb2015 = rcb)) %>% 
  arrange(model, case, r10, allocation, ppp_pf, year) %>% 
  mutate(rcb2100 = ifelse(grepl(allocation, pattern = "2015"), 
          rcb2015 - terr_GtCO2FFI_cmltv_2015,
          rcb1990 - terr_GtCO2FFI_cmltv_1990)) %>% 
  select(model, r10, case, allocation, ppp_pf, rcb1990, 
         terr_GtCO2FFI_19902100 = terr_GtCO2FFI_cmltv_1990, rcb2100, 
         gtco2_cdr_2100 = cdr_GtCO2_cmltv) %>% 
  mutate(ppp_pf = ifelse(is.na(ppp_pf), "NA", ppp_pf)) %>% 
  left_join(r10_analysisdata %>%
              filter(year >= 1990) %>%
              mutate(pop_cmltv = cumsum(pop),
                     pop_cmltv_rem_2025 = pop_cmltv[year == 2100] - pop_cmltv[year == 2025]) %>%
              filter(year == 2100) %>%
              select(r10, pop_cmltv_rem_2025)) %>%
  ungroup() %>%
  arrange(model, case, r10, allocation, ppp_pf) %>% 
  select(model, r10, case, allocation, ppp_pf, rcb2100, pop_cmltv_rem_2025)

# Write to file for SI
write_csv(r10_curpol_curpledge_impren_debt, here("Data", "processed", "14_r10_curpol_curpledge_impren_debt.csv"))

# Combine extreme heatwave EMFs and carbon debt
r10_exp_heatwave_emf_temp_debt <- left_join(r10_exp_heatwave_emf,
                                               r10_curpol_curpledge_impren_debt,
                                               by = c("case", "r10"))

# FIGURE 2 ---------------------------------------------------------------------

a <- global_temps %>% 
  filter(quantile %in% c(0.33, 0.5, 0.66), case %in% c("A", "E")) %>% 
  pivot_wider(names_from = quantile, values_from = gmt) %>% 
  mutate(case = factor(case, levels = c("A", "E", "IMP-REN"),
                       labels = c("CurPol", "CurPledge", "IMP-REN"))) %>% 
  ggplot(aes(x = year, linetype = case)) +
  geom_texthline(yintercept = 1.5, linetype = 2, label = "1.5°C", hjust = 0.1) +
  geom_ribbon(aes(ymin = `0.33`, ymax = `0.66`), alpha = 0.1, show.legend = F) +
  geom_textpath(aes(y = `0.5`, label = case, hjust = 0.9), size = 3, linewidth = 0.5,
                show.legend = F, straight = TRUE) +
  geom_line(aes(y = `0.5`, group = interaction(case)), 
            colour = "black", data = . %>% filter(year <= 2022),
            linewidth = 1, show.legend = F) +
  scale_colour_discrete_qualitative() +
  scale_x_continuous(breaks = c(1990, seq(2000,2100,20))) +
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_blank(), strip.placement = "inside", strip.text = element_text(hjust = 0),
        strip.text.x = element_text(size = 12),
        # Change y axis ticks to right side
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 11), panel.grid = element_blank()) +
  labs(x = NULL, y = "GMT increase relative to 1850-1900 (°C)", linetype = "Scenario")

b <- r10_curpol_curpledge_impren_rcbyear %>% 
  ungroup() %>% 
  filter(allocation == "PP1990", case != "IMP-REN") %>% 
  mutate(
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
    case = factor(case, levels = c("A", "E", "IMP-REN"),
                  labels = c("CurPol", "CurPledge", "IMP-REN"))) %>% 
  mutate(group = case_when(r10 %in% c("NAM", "EAS", "MEA", "EEA", "EUR", "APD") ~ "Earlier debtors",
                           TRUE ~ "Later debtors")) %>% 
  group_by(case, year, group) %>% 
  summarise(terr_GtCO2FFI = sum(terr_GtCO2FFI),
            terr_GtCO2FFI_cap = sum(terr_GtCO2FFI * 1e9) / sum(pop),
            debtyear = sum(debtyear),
            scenexceedanceshareyear = sum(scenexceedanceshareyear)) %>% 
  pivot_longer(-c(case, group, year)) %>% 
  mutate(name = factor(name, levels = c("terr_GtCO2FFI", "terr_GtCO2FFI_cap", 
                                        "debtyear", "scenexceedanceshareyear"),
                        labels = c("Emissions (GtCO\U2082-FFI yr\U207B\U00B9)", 
                                   "Emissions (tCO\U2082-FFI cap\U207B\U00B9 yr\U207B\U00B9)",
                                   "Carbon debt accrual (GtCO\U2082)",
                                   "Overshoot responsibility (%)"))) %>%
  ggplot(aes(x = year, y = value, colour = group, linetype = case)) +
  geom_line() +
  geom_line(aes(group = interaction(case, group)), 
            colour = "black", data = . %>% filter(year <= 2022),
            linewidth = 1, show.legend = F) +
  facet_wrap(~name, ncol = 2, scales = "free_y", strip.position = "left") +
  scale_colour_manual(values = c("#1f78b4","#33a02c")) +
  scale_x_continuous(breaks = c(1990, seq(2000,2100,20))) +
  scale_y_continuous(position = "right") +
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_blank(), 
        strip.placement = "inside", strip.text = element_text(hjust = 0),
        strip.text.y = element_text(size = 12),
        # Change y axis ticks to right side
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 11), panel.grid = element_blank()) +
  labs(x = NULL, y = NULL, linetype = "Scenario", colour = "Group")

wrap_plots(a,b, ncol = 2, widths = c(0.5,1)) +
  plot_annotation(tag_levels = list("a"), tag_prefix = "(", tag_suffix = ")", 
                  caption = paste0("Earlier debtors - ", paste0(r10order$r10label[1:6], ": ",r10order$r10labellong[1:6], collapse = ", "), 
                                   "\n", paste0("Later debtors - ", paste0(r10order$r10label[7:10], ": ",r10order$r10labellong[7:10], collapse = ", ")), collapse = ""))

ggsave(here("Manuscript", "Figures", "fig3.svg"),
       height = 6.6, width = 10.1)

# SI 
r10_curpol_curpledge_impren_rcbyear %>% 
  ungroup() %>% 
  filter(case != "IMP-REN",
         !grepl("2015", allocation)) %>% 
  mutate(
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
    case = factor(case, levels = c("A", "E", "IMP-REN"),
                  labels = c("CurPol", "CurPledge", "IMP-REN")),
    hjust = as.numeric(r10) / 8,
    scenexceedanceshareyear = scenexceedanceshareyear * scenexceedanceyear, 
    ppp_pf = ifelse(is.na(ppp_pf), "", ppp_pf)) %>% 
  filter(ppp_pf %in% c("", "PPP_1/sqrt(x)"), year >= 2020) %>% 
  select(r10, case, year, scenexceedanceshareyear, allocation, ppp_pf, hjust) %>% 
  group_by(case, year, allocation, ppp_pf) %>%
  mutate(scenexceedanceshareyear = scenexceedanceshareyear / sum(scenexceedanceshareyear),
         group = interaction(allocation, ppp_pf),
         group = factor(group, levels = c("PP1850.", "PP1990.",
                                          "PP1850adjATP.PPP_1/sqrt(x)",
                                          "PP1990adjATP.PPP_1/sqrt(x)"),
                        labels = c("Responsibility, 1850",
                                   "Responsibility, 1990",
                                   "Responsibility and Capability, 1850",
                                   "Responsibility and Capability, 1990"))) %>% 
  ggplot(aes(x = year, group = group)) +
  geom_path(aes(colour = group, y = scenexceedanceshareyear, label = r10, hjust = hjust), alpha = 1) +
  scale_x_continuous(breaks = c(seq(2020,2100,20))) +
  scale_y_continuous(labels = scales::percent_format(), position = "left") +
  scale_colour_manual(values = c("#1f78b4",  "#980043", "#a6cee3",  "#c994c7")) +
  scale_fill_manual(values = c("#1f78b4", "#980043", "#a6cee3", "#c994c7")) +
  facet_grid(fct_rev(case) ~ r10) +
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_blank(), strip.placement = "inside", strip.text = element_text(hjust = 0),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13), panel.grid = element_blank()) +
  guides(colour = guide_legend(nrow = 2)) +
  labs(x = NULL, y = "Temporal overshoot responsibility (% of total scenexceedance)",
       colour = "Allocation approach",
       fill = "Allocation approach",
       caption = paste0(paste0(r10order$r10label[1:5], ": ",r10order$r10labellong[1:5], collapse = ", "), 
                        "\n", paste0(r10order$r10label[6:10], ": ",r10order$r10labellong[6:10], collapse = ", "), collapse = ""))

ggsave(here("Manuscript", "Figures", "SI", "SI_fig3a1.png"),
       height = 8, width = 14)

r10_curpol_curpledge_impren_rcbyear %>%
  ungroup() %>%
  filter(allocation == "PP1990", case != "IMP-REN", year >= 1991) %>%
  select(r10, case, year, rcbyear) %>%
  mutate(
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
    case = factor(case, levels = c("A", "E", "IMP-REN"),
                  labels = c("CurPol",
                             "CurPledge", "IMP-REN"))) %>% 
  mutate(figgroup = 
           case_when(r10 %in% c("NAM", "EUR", "EAS", "MEA", "EEA", "APD") ~ "Earlier debtors, debt accrual before 2030",
                     r10 %in% c("SAS", "AFR", "LAC", "PAS") ~ "Later debtors, debt accrual after 2030")) %>% 
  group_by(case, figgroup) %>%
  mutate(hjust = case_when(
    r10 %in% c("EEA", "LAC") ~ 0.8,
    r10 %in% c("SAS") ~ 0.9,
    TRUE ~ 1
  )) %>% 
  ggplot(aes(x = year, linetype = case)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 2030, linetype = 2, alpha = 0.5) +
  geom_textpath(aes(colour = r10, y = rcbyear , label = r10, hjust = hjust), alpha = 1, size = 3,
                data = . %>% filter(case == "CurPol"), show.legend = F) +
  geom_path(aes(colour = r10, y = rcbyear ), alpha = 1, size = 0.5,
            data = . %>% filter(case == "CurPledge", year >= 2030), show.legend = F) +
  geom_text(x = 2100, y = -30, aes(label = ifelse(case == "Current policies", "Debt", "")),
            alpha = 1, hjust = 1, colour = "darkgrey", size = 3,
            data = . %>% distinct(case)) +
  geom_text(x = 2100, y = 30, aes(label = ifelse(case == "Current policies", "Credit", "")),
            alpha = 1, hjust = 1, colour = "darkgrey", size = 3,
            data = . %>% distinct(case)) +
  scale_x_continuous(breaks = c(1990, 2000, 2020, 2030, seq(2040,2100,20))) +
  scale_y_continuous(position = "left") +
  scale_colour_discrete_qualitative() +
  scale_fill_discrete_qualitative() +
  facet_wrap(~figgroup, ncol = 2, strip.position = "right") +
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_blank(), strip.placement = "inside", strip.text = element_text(hjust = 0),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 11), panel.grid = element_blank()) +
  guides(colour = "none", fill = "none") +
  labs(x = NULL, y = "Remaining budget (GtCO\U2082-FFI)",
       subtitle = "Solid line: CurPol, Dashed line: CurPledge")

ggsave(here("Manuscript", "Figures", "SI", "SI_fig3a2.png"),
       height = 3, width = 7)
    
r10_curpol_curpledge_impren_rcbyear %>% 
  ungroup() %>% 
  filter(allocation == "PP1990", case != "IMP-REN") %>% 
  mutate(
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
    case = factor(case, levels = c("A", "E", "IMP-REN"),
                  labels = c("CurPol", "CurPledge", "IMP-REN")),
    scenexceedanceshareyear = scenexceedanceshareyear * scenexceedanceyear) %>% 
  select(r10, case, year, scenexceedanceshareyear) %>% 
  group_by(case, year) %>%
  mutate(scenexceedanceshareyear = scenexceedanceshareyear / sum(scenexceedanceshareyear)) %>% 
  pivot_wider(names_from = case, values_from = scenexceedanceshareyear) %>% 
  ggplot(aes(x = `CurPol`, y = `CurPledge`)) +
  geom_textabline(linetype = 2, label = "identity") +
  geom_point(aes(colour = year)) +
  scale_colour_continuous_sequential() +
  facet_wrap(~r10, ncol = 5) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.width = unit(2.5, "cm"),
        strip.background = element_blank(), strip.placement = "inside", strip.text = element_text(hjust = 0),
        strip.text.x = element_text(size = 12),
        axis.title = element_text(size = 13), panel.grid = element_blank()) +
  labs(colour = "Year",
       caption = paste0(paste0(r10order$r10label[1:5], ": ",r10order$r10labellong[1:5], collapse = ", "), 
                        "\n", paste0(r10order$r10label[6:10], ": ",r10order$r10labellong[6:10], collapse = ", "), collapse = ""))
       
ggsave(here("Manuscript", "Figures", "SI", "SI_fig3b.png"),
       height = 8, width = 14)

r10_exp_heatwave_emf_temp_debt %>%
  filter(case != "IMP-REN", allocation == "PP1990") %>%
  mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
         case = factor(case, levels = c("A", "E", "IMP-REN"),
                       labels = c("CurPol", "CurPledge", "IMP-REN"))) %>% 
  ungroup() %>% 
  group_by(birth_year, r10, case) %>%
  mutate(min_drawdown_rate = ifelse(rcb2100 < 0, rcb2100 * 1e9 / pop_cmltv_rem_2025, 0),
         debt = ifelse(rcb2100 < 0, -rcb2100, 0)) %>% 
  ggplot(aes(x = birth_year, y = emf_impren_0.5 - 1)) +
  geom_hline(yintercept = 0, linetype = 2, size = .3) +
  geom_ribbon(aes(ymin = emf_impren_0.33 - 1, 
                  ymax = emf_impren_0.66 - 1,
                  fill = case), alpha = 0.2,
              linewidth = 0, show.legend = F) +
  geom_path(alpha = 1, size = 1, aes(colour = case)) +
  geom_point(aes(y = min_drawdown_rate / 10, colour = case), size = 5,
             data = . %>% filter(birth_year == 2020), shape = "-") +
  geom_text(aes(y = (min_drawdown_rate / 10), colour = case,
                label = paste0(round(min_drawdown_rate, 1))), size = 3,
            data = . %>% filter(birth_year == 2020),  hjust = 1.25,
            show.legend = F) +
  facet_grid(case~r10) +
  scale_colour_discrete_qualitative() +
  scale_x_continuous(breaks = c(1980, 2000, 2020)) +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%"),
                     breaks = seq(0.5,3,0.5),
                     sec.axis = sec_axis(~ . * 10, name = "Carbon drawdown obligation (tCO\U2082 cap\U207B\U00B9 yr\U207B\U00B9, 2025-2100)",
                                         breaks = seq(0,-20,-5))) +
  labs(x = "Cohort birth year",
       y = "Increased lifetime exposure relative to reference 1.5°C scenario",
       shape = "Cohort birth year",
       colour = "Scenario") +
  theme_bw() +
  theme(legend.position = "top",
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.background = element_blank(), strip.placement = "outside", 
        strip.text = element_blank(),
        strip.text.x = element_text(size = 13),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()) +
  plot_annotation(caption = paste0(paste0(r10order$r10label[1:5], ": ",r10order$r10labellong[1:5], collapse = ", "), 
                                   "\n", paste0(r10order$r10label[6:10], ": ",r10order$r10labellong[6:10], collapse = ", "), collapse = ""))

ggsave(here("Manuscript", "Figures", "fig4.svg"),
       height = 7, width = 10)

drawdown_data <- r10_curpol_curpledge_impren_rcbyear %>%
  group_by(model, case, r10, allocation, ppp_pf) %>% 
  summarise(debt2100 = -debtyear[year==2100],
            pop_cmltv_rem_20252100 = sum(pop[year >= 2025 & year <= 2100])) %>% 
  mutate(
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
    case = factor(case, levels = c("A", "E", "IMP-REN"),
                  labels = c("CurPol", "CurPledge", "IMP-REN"))) %>% 
  mutate(group = case_when(r10 %in% c("NAM", "EAS", "MEA", "EEA", "EUR", "APD") ~ "Earlier debtors",
                           TRUE ~ "Later debtors")) %>% 
  group_by(model, case, r10, allocation, ppp_pf) %>% 
  summarise(debt2100 = sum(debt2100),
            pop_cmltv_rem_20252100 = sum(pop_cmltv_rem_20252100)) %>% 
  mutate(drawdowncap_2100 = debt2100 * 1e9 / pop_cmltv_rem_20252100) %>% 
  filter(case != "IMP-REN", allocation == "PP1990")

r10_curpol_curpledge_impren_rcbyear %>% 
  ungroup() %>% 
  filter(allocation == "PP1990", case != "IMP-REN") %>% 
  mutate(
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
    case = factor(case, levels = c("A", "E", "IMP-REN"),
                  labels = c("CurPol", "CurPledge", "IMP-REN"))) %>% 
  mutate(group = case_when(r10 %in% c("NAM", "EAS", "MEA", "EEA", "EUR", "APD") ~ "Earlier debtors",
                           TRUE ~ "Later debtors")) %>% 
  group_by(case, year, r10) %>% 
  summarise(terr_GtCO2FFI = sum(terr_GtCO2FFI),
            pop = sum(pop)) %>% 
  left_join(drawdown_data) %>% 
  mutate(drawdowncap_2100 = ifelse(year >= 2025, drawdowncap_2100, NA_real_)) %>%
  ggplot(aes(x = year)) +
  geom_hline(yintercept = 0, linetype = 2, size = .3) +
  geom_path(aes(y = terr_GtCO2FFI),
            data = . %>% filter(year <= 2022), size = 1) +
  geom_path(aes(y = terr_GtCO2FFI, linetype = case),
            data = . %>% filter(year >= 2022), show.legend = F) +
  geom_ribbon(aes(ymin = (value * pop) / 1e9, ymax = 0, fill = name),
              data = . %>% pivot_longer(cols = matches("drawdowncap")) %>% 
                mutate(name = factor(name, levels = c("drawdowncap_2100"),
                                     labels = c("2100"))),
              alpha = 0.5, show.legend = F) +
  geom_path(aes(y = (value * pop) / 1e9, colour = name),
            data = . %>% pivot_longer(cols = matches("drawdowncap")) %>% 
              mutate(name = factor(name, levels = c("drawdowncap_2100"),
                                   labels = c("2100"))),
            alpha = 1, show.legend = F) +
  geom_text(aes(x = 2020, y = -12, label = paste0((round(debt2100,1)), " GtCO\U2082")),
            data = . %>% filter(year == 2020),
            hjust = 0.2, vjust = 0, size = 3) +
  scale_x_continuous(breaks = c(1990,seq(2025, 2100,25))) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7)) +
  scale_fill_manual(values = c("#d95f02")) +
  scale_colour_manual(values = c("#d95f02")) +
  labs(y = "GtCO\U2082 yr\U207B\U00B9", x = NULL,
       linetype = NULL,
       caption = paste0(paste0(r10order$r10label[1:5], ": ",r10order$r10labellong[1:5], collapse = ", "), 
                        "\n", paste0(r10order$r10label[6:10], ": ",r10order$r10labellong[6:10], collapse = ", "), collapse = ""))+
  facet_grid(case~r10) +
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_blank(), 
        strip.placement = "inside", strip.text = element_text(hjust = 0),
        strip.text.y = element_text(size = 12),
        # Change y axis ticks to right side
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 11), panel.grid = element_blank())

ggsave(here("Manuscript", "Figures", "SI", "SI_fig3c.png"),
       height = 5, width = 10)

a_data <- r10_curpol_curpledge_impren_debt %>%
  ungroup() %>%
  filter(allocation == "PP1990", case != "IMP-REN") %>%
  select(r10, case, rcb2100, pop_cmltv_rem_2025) %>%
  group_by(case) %>% 
  mutate(global_min_drawdown_rate = sum(ifelse(rcb2100 < 0, rcb2100 * 1e9, 0)) / sum(pop_cmltv_rem_2025)) %>% 
  group_by(r10, case) %>%
  mutate(min_drawdown_rate = ifelse(rcb2100 < 0, rcb2100 * 1e9 / pop_cmltv_rem_2025, 0),
         debt = ifelse(rcb2100 < 0, -rcb2100, 0))

a_curpol_relminrateglb <- a_data %>% filter(case == "A") %>% 
  pull(global_min_drawdown_rate)

a_curpledge_relminrateglb <- a_data %>% filter(case == "E") %>% 
  pull(global_min_drawdown_rate)

a_curpol <- a_data %>%
  filter(case == "A") %>% 
  mutate(
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
    case = factor(case, levels = c("A", "E"),
                  labels = c("CurPol", "CurPledge")),
    hjust = as.numeric(r10) / 10) %>%
  ggplot(aes(x = r10)) +
  geom_hline(yintercept = 0, linetype = 2, size = .3) +
  geom_hline(aes(yintercept = global_min_drawdown_rate, colour = case), linetype = 2, size = .3) +
  geom_segment(aes(y = global_min_drawdown_rate, yend = min_drawdown_rate, colour = case),
               position = position_dodge(width = 0.2)) +
  geom_point(aes(y = min_drawdown_rate, colour = case),
             position = position_dodge(width = 0.2)) +
  geom_text(aes(y = global_min_drawdown_rate, 
                vjust = ifelse(min_drawdown_rate < global_min_drawdown_rate, -0.5, 1.5),
                label = 
                  paste0(round(min_drawdown_rate * pop_cmltv_rem_2025 / 1e9,0), "Gt")),
            size = 2.5, colour = "black") +
  scale_colour_discrete_qualitative(drop = F) +
  scale_fill_discrete_qualitative(drop = F) +
  scale_size_continuous(breaks = scales::pretty_breaks(n = 4)) +
  scale_y_continuous(sec.axis = sec_axis(~ . / a_curpol_relminrateglb, name = "Relative to global average",
                                         labels = scales::dollar_format(suffix = "x", prefix = ""))) +
  coord_cartesian(ylim = c(-20, 0)) +
  facet_wrap(~case, scales = "free_y") + # Allow different y scales for each facet
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_blank(), strip.placement = "inside", 
        strip.text = element_text(hjust = 0),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 11), panel.grid = element_blank()) +
  guides(colour = "none", fill = "none",
         size = guide_legend(nrow = 1)) +
  labs(x = NULL, y = "tCO2capita-1yr-1")

a_curpledge <- a_data %>%
  filter(case == "E") %>% 
  mutate(
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
    case = factor(case, levels = c("A", "E"),
                  labels = c("CurPol", "CurPledge")),
    hjust = as.numeric(r10) / 10) %>%
  ggplot(aes(x = r10)) +
  geom_hline(yintercept = 0, linetype = 2, size = .3) +
  geom_hline(aes(yintercept = global_min_drawdown_rate, colour = case), linetype = 2, size = .3) +
  geom_segment(aes(y = global_min_drawdown_rate, yend = min_drawdown_rate, colour = case),
               position = position_dodge(width = 0.2)) +
  geom_point(aes(y = min_drawdown_rate, colour = case),
             position = position_dodge(width = 0.2)) +
  geom_text(aes(y = global_min_drawdown_rate, 
                vjust = ifelse(min_drawdown_rate < global_min_drawdown_rate, -0.5, 1.5),
                label = 
                  paste0(round(min_drawdown_rate * pop_cmltv_rem_2025 / 1e9,0), "Gt")),
            size = 2.5, colour = "black") +
  scale_colour_discrete_qualitative(drop = F) +
  scale_fill_discrete_qualitative(drop = F) +
  scale_size_continuous(breaks = scales::pretty_breaks(n = 4)) +
  scale_y_continuous(sec.axis = sec_axis(~ . / a_curpledge_relminrateglb, name = "Relative to global average",
                                         labels = scales::dollar_format(suffix = "x", prefix = ""),
                                         breaks = c(0,2,4,6))) +
  coord_cartesian(ylim = c(-20, 0)) +
  facet_wrap(~case, scales = "free_y") + # Allow different y scales for each facet
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_blank(), strip.placement = "inside", 
        strip.text = element_text(hjust = 0),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 11), panel.grid = element_blank()) +
  guides(colour = "none", fill = "none",
         size = guide_legend(nrow = 1)) +
  labs(x = NULL, y = "tCO2capita-1yr-1")

b <- r10_curpol_curpledge_impren_rcbyear %>% 
  filter(allocation == "PP1990", case != "IMP-REN") %>% 
  left_join(r10_analysisdata %>% select(r10, pop)) %>% 
  group_by(model, case, year) %>% 
  summarise(terr_GtCO2FFI = sum(terr_GtCO2FFI),
            pop = sum(pop)) %>% 
  left_join(a_data %>% group_by(case) %>% summarise(
    global_min_drawdown_rate = unique(global_min_drawdown_rate),
    total_drawdown = round(sum(debt),0))) %>% 
  mutate(case = factor(case, levels = c("A", "E"),
                       labels = c("CurPol", "CurPledge")),
         global_min_drawdown_rate = ifelse(year < 2025, NA, global_min_drawdown_rate)) %>% 
  ggplot(aes(x = year, y = terr_GtCO2FFI * 1e9 / pop, colour = case, group = case)) +
  geom_hline(yintercept = 0, linetype = 2, size = .3) +
  geom_path() +
  geom_ribbon(aes(ymin = global_min_drawdown_rate, ymax = 0, fill = case),
              alpha = 0.3, linewidth = 0) +
  geom_path(aes(y = global_min_drawdown_rate), linetype = 2) +
  geom_text(aes(y = global_min_drawdown_rate / 2,
                label = paste0("Total: ",total_drawdown, "Gt")),
            x = 2063, size = 3, colour = "black",
            data = . %>% filter(year == 2063)) +
  geom_path(colour = "black", data = . %>% filter(year <= 2022)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_x_continuous(breaks = c(1990, 2025, seq(2040, 2100,20))) +
  facet_wrap(~case, scales = "free_x", ncol = 1) +
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_blank(), strip.placement = "inside", 
        strip.text = element_text(hjust = 0),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 11), panel.grid = element_blank()) +
  guides(colour = "none", fill = "none",
         size = guide_legend(nrow = 1)) +
  labs(x = NULL, y = "tCO2capita-1yr-1")

wrap_plots(b, wrap_plots(a_curpol, a_curpledge + plot_layout(tag_level = 'new'), ncol = 1)) +
  plot_annotation(caption = paste0(paste0(r10order$r10label[1:5], ": ",
                                          r10order$r10labellong[1:5], collapse = ", "), 
                                   "\n", paste0(r10order$r10label[6:10], ": ",
                                                r10order$r10labellong[6:10], 
                                                collapse = ", "), collapse = ""),
                  tag_levels = list("a"), tag_prefix = "(", tag_suffix = ")")

ggsave(here("Manuscript", "Figures", "SI", "SI_fig3d.png"),
       height = 6, width = 10)
