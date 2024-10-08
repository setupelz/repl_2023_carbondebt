# Replication archive for: "Using net-zero carbon debt to track climate overshoot responsibility"

# Contact for clarifications: [ANONYMISED]       

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
r10_analysisdata <- read_csv(here("Data", "processed", "2_analysisdata.csv")) %>%
  filter(iso3c != "ROW", year >= 1990) %>% 
  select(-iso3c) %>% 
  group_by(r10, year) %>% 
  summarise(across(everything(), ~ sum(.))) %>% 
  arrange(year)

# Calculate population from year to 2050
r10_poprem <- tibble(.rows = 0)

for (i in 1991:2020) {
  
  pop_rem_loop <- r10_analysisdata %>% 
    filter(year >= i & year <= 2050) %>% 
    summarise(pop_yearto2050 = sum(pop, na.rm = T)) %>% 
    mutate(year = i)
  
  r10_poprem <- rbind(r10_poprem, pop_rem_loop)
  
}

# RCB quantities
rcb <- read_csv(here("Data", "processed", "rcbquantities.csv"))

# Regional RCBs from 1991 to 2020
r10_rcb19912020 <- read_csv(here("Data", "processed", "r10_rcb19912020.csv")) 

# Assessed pathways
r10_ndclts_impren_emiss <- read_csv(here("Data", "processed", "r10_ndclts_impren_emiss.csv"))

# Heatwave EMFs and additional years
r10_exp_heatwave_emf <- read_csv(here("Data", "processed", "r10_exp_heatwave_emf.csv"))

# Temperatures
global_temps <- read_csv(here("Data", "processed", "r10_ndclts_impren_temp.csv"))

# ASSESS MODELLED NDC / NET-ZERO PATHWAYS --------------------------------------

# Determine regional remaining carbon budget allocation evolution along assessed paths
r10_ndclts_impren_rcbyear <- r10_ndclts_impren_emiss %>%
  filter(year >= 1991) %>% 
  mutate(cdr_GtCO2 = ifelse(terr_GtCO2 < 0, -terr_GtCO2, 0),
         terr_GtCO2 = ifelse(terr_GtCO2 < 0 , 0, terr_GtCO2)) %>% 
  group_by(case, aggregate, r10) %>% 
  mutate(terr_GtCO2_cmltv_1991 = cumsum(terr_GtCO2),
         terr_GtCO2_cmltv_2015 = cumsum(ifelse(year >= 2015, terr_GtCO2, 0)),
         cdr_GtCO2_cmltv = cumsum(cdr_GtCO2)) %>% 
  left_join(r10_rcb19912020 %>% distinct(r10, category, ppp_pf)) %>% 
  left_join(r10_rcb19912020 %>% filter(year == 1991, grepl(category, pattern = "1990|1850")) %>% 
              select(r10, category, ppp_pf, rcb1991 = rcb)) %>% 
  left_join(r10_rcb19912020 %>% filter(year == 2015, grepl(category, pattern = "2015")) %>% 
              select(r10, category, ppp_pf, rcb2015 = rcb)) %>% 
  arrange(model, case, aggregate, r10, category, ppp_pf, year) %>% 
  group_by(model, case, aggregate, r10, category, ppp_pf) %>%
  mutate(rcbyear = ifelse(grepl(category, pattern = "2015"), 
          rcb2015 - lag(terr_GtCO2_cmltv_2015, default = 0),
          rcb1991 - lag(terr_GtCO2_cmltv_1991, default = 0))) %>% 
  arrange(model, case, aggregate, category, ppp_pf, r10, year) %>% 
  group_by(model, case, aggregate, category, ppp_pf, year) %>% 
  mutate(exceedanceyear = ifelse(sum(-rcbyear) > 0, sum(-rcbyear), 0),
         debtyear = ifelse(-rcbyear > 0, -rcbyear, 0)) %>% 
  group_by(r10, model, case, aggregate, category, ppp_pf) %>% 
  mutate(exceedancecmltv = cumsum(exceedanceyear),
         debtcmltv = cumsum(debtyear)) %>% 
  group_by(model, case, aggregate, category, ppp_pf, year) %>% 
  mutate(exceedanceshareyear = (debtyear / sum(debtyear) * exceedanceyear) / exceedanceyear,
         exceedancesharecmltv = (debtcmltv / sum(debtcmltv) * exceedancecmltv) / exceedancecmltv) %>% 
  arrange(model, case, aggregate, r10, category, ppp_pf, year) 

# Determine regional net-zero carbon debt associated with assessed paths
r10_ndclts_impren_debt <- r10_ndclts_impren_emiss %>%
  filter(year >= 1991) %>% 
  mutate(cdr_GtCO2 = ifelse(terr_GtCO2 < 0, -terr_GtCO2, 0),
         terr_GtCO2 = ifelse(terr_GtCO2 < 0 , 0, terr_GtCO2)) %>% 
  group_by(case, aggregate, r10) %>% 
  mutate(terr_GtCO2_cmltv_1991 = cumsum(terr_GtCO2),
         terr_GtCO2_cmltv_2015 = cumsum(ifelse(year >= 2015, terr_GtCO2, 0)),
         cdr_GtCO2_cmltv = cumsum(cdr_GtCO2)) %>% 
  filter(year == 2100) %>% 
  left_join(r10_rcb19912020 %>% distinct(r10, category, ppp_pf)) %>% 
  left_join(r10_rcb19912020 %>% filter(year == 1991, grepl(category, pattern = "1990|1850")) %>% 
              select(r10, category, ppp_pf, rcb1991 = rcb)) %>% 
  left_join(r10_rcb19912020 %>% filter(year == 2015, grepl(category, pattern = "2015")) %>% 
              select(r10, category, ppp_pf, rcb2015 = rcb)) %>% 
  arrange(model, case, aggregate, r10, category, ppp_pf, year) %>% 
  mutate(rcb2100 = ifelse(grepl(category, pattern = "2015"), 
          rcb2015 - terr_GtCO2_cmltv_2015,
          rcb1991 - terr_GtCO2_cmltv_1991)) %>% 
  select(model, r10, case, aggregate, category, ppp_pf, rcb1991, 
         terr_GtCO2_19912100 = terr_GtCO2_cmltv_1991, rcb2100, 
         gtco2_cdr_2100 = cdr_GtCO2_cmltv) %>% 
  mutate(ppp_pf = ifelse(is.na(ppp_pf), "NA", ppp_pf)) %>% 
  # Determine pathway 1.5 RCB exceedances, total regional nz debts and credits
  group_by(model, case, aggregate, category, ppp_pf) %>% 
  mutate(exceedance = -(rcb %>% filter(rcb == "rcb1990_nz") %>% pull(gtco2) - sum(terr_GtCO2_19912100)),
         totaldebt = sum(ifelse(rcb2100 < 0, rcb2100, NA_real_), na.rm = T),
         totalcredit = sum(ifelse(rcb2100 > 0, rcb2100, NA_real_), na.rm = T),
         drawdown_resp = ifelse(rcb2100 < 0, rcb2100 / totaldebt * exceedance, 0)) %>% 
  left_join(r10_analysisdata %>% 
              filter(year >= 1991) %>% 
              mutate(pop_cmltv = cumsum(pop),
                     pop_cmltv_rem_2050 = pop_cmltv[year == 2100] - pop_cmltv[year == 2050]) %>% 
              filter(year == 2100) %>% 
              select(r10, pop_cmltv_rem_2050)) %>% 
  ungroup() %>% 
  arrange(model, case, aggregate, r10, category, ppp_pf) 

# Combine extreme heatwave EMFs and carbon debt
r10_exp_heatwave_emf_temp_debt <- left_join(r10_exp_heatwave_emf,
                                               r10_ndclts_impren_debt,
                                               by = c("case", "aggregate", "r10"))

# FIGURE 2 ---------------------------------------------------------------------

a <- global_temps %>% 
  filter(quantile %in% c(0.33, 0.5, 0.66), case %in% c("A", "E"), aggregate == "Median") %>% 
  pivot_wider(names_from = quantile, values_from = gmt) %>% 
  mutate(case = factor(case, levels = c("A", "E", "IMP-REN"),
                       labels = c("CurPol", "CurPledge", "IMP-REN"))) %>% 
  ggplot(aes(year, linetype = case)) +
  geom_hline(yintercept = 1.5, linetype = 2) +
  geom_ribbon(aes(ymin = `0.33`, ymax = `0.66`), alpha = 0.1, show.legend = F) +
  geom_textpath(aes(y = `0.5`, label = case, hjust = 0.9), size = 3,
                show.legend = F) +
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
  labs(x = NULL, y = "Temperature (C)", linetype = "Scenario",
       subtitle = "GMT increase relative to 1850-1900")

b <- r10_ndclts_impren_rcbyear %>%
  ungroup() %>%
  filter(category == "PP1990", case != "IMP-REN", year >= 1991) %>%
  select(r10, case, aggregate, year, rcbyear) %>%
  mutate(
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
    case = factor(case, levels = c("A", "E", "IMP-REN"),
                  labels = c("CurPol",
                             "CurPledge", "IMP-REN"))) %>% 
  pivot_wider(names_from = aggregate, values_from = rcbyear) %>%
  mutate(figgroup = 
           case_when(r10 %in% c("NAM", "EUR", "EAS", "MEA", "EEA", "APD") ~ "Debt accrual before 2030",
                     r10 %in% c("SAS", "AFR", "LAC", "PAS") ~ "Debt accrual after 2030")) %>% 
  group_by(case, figgroup) %>%
  mutate(hjust = case_when(
    r10 %in% c("EEA", "LAC") ~ 0.8,
    TRUE ~ 1
  )) %>% 
  ggplot(aes(x = year, linetype = case)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_textpath(aes(colour = r10, y = Median, label = r10, hjust = hjust), alpha = 1, size = 3,
                data = . %>% filter(case == "CurPol")) +
  geom_path(aes(colour = r10, y = Median), alpha = 1, size = 0.5,
            data = . %>% filter(case == "CurPledge", year >= 2030)) +
  geom_text(x = 2100, y = -30, aes(label = ifelse(case == "Current policies", "Debt", "")),
            alpha = 1, hjust = 1, colour = "darkgrey", size = 3,
            data = . %>% distinct(case)) +
  geom_text(x = 2100, y = 30, aes(label = ifelse(case == "Current policies", "Credit", "")),
            alpha = 1, hjust = 1, colour = "darkgrey", size = 3,
            data = . %>% distinct(case)) +
  scale_x_continuous(breaks = c(1990, seq(2000,2100,20))) +
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
  guides(colour = "none", fill = "none", linetype = "none") +
  labs(x = NULL, y = "Remaining budget (GtCO2)",
       subtitle = "Regional budget consumption")

# b <- r10_ndclts_impren_rcbyear %>% 
#   filter(category == "PP1990", case != "IMP-REN", r10 == "R10NORTH_AM") %>% 
#   select(r10, case, aggregate, year, exceedanceyear) %>% 
#   mutate(
#     r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
#     case = factor(case, levels = c("A", "E", "IMP-REN"),
#                   labels = c("Current policies", 
#                              "All pledges and net-zero targets", "IMP-REN"))) %>% 
#   pivot_wider(names_from = aggregate, values_from = exceedanceyear) %>% 
#   ggplot(aes(x = year)) +
#   geom_ribbon(aes(ymin = Min, ymax = Max), fill = "black", alpha = 0.3) +
#   geom_path(aes(y = Median), colour = "black") +
#   facet_wrap(~fct_rev(case), scales = "free", ncol = 1)  +
#   scale_x_continuous(breaks = c(1990, seq(2000,2100,20))) +
#   scale_y_continuous(position = "right") +
#   scale_fill_discrete_qualitative() +
#   facet_wrap(~fct_rev(case), ncol = 1, strip.position = "left") +
#   theme_bw() +
#   theme(legend.position = "top",
#         strip.background = element_blank(), strip.placement = "inside", strip.text = element_text(hjust = 0),
#         strip.text.x = element_text(size = 12),
#         axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
#         axis.text = element_text(size = 12),
#         axis.title = element_text(size = 13), panel.grid = element_blank()) +
#   guides(fill = "none") +
#   labs(x = NULL, y = NULL,
#        subtitle = "Global 1.5C RCB exceedance (GtCO2)")

# c <- r10_ndclts_impren_rcbyear %>% 
#   ungroup() %>% 
#   filter(category == "PP1990", case != "IMP-REN") %>% 
#   mutate(
#     r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
#     case = factor(case, levels = c("A", "E", "IMP-REN"),
#                   labels = c("Current policies", 
#                              "All pledges and net-zero targets", "IMP-REN")),
#     hjust = as.numeric(r10) / 10,
#     exceedanceshareyear = exceedanceshareyear * exceedanceyear) %>% 
#   select(r10, case, aggregate, year, exceedanceshareyear, hjust) %>% 
#   group_by(case, year, aggregate) %>%
#   mutate(exceedanceshareyear = exceedanceshareyear / sum(exceedanceshareyear)) %>% 
#   pivot_wider(names_from = aggregate, values_from = exceedanceshareyear) %>% 
#   ggplot(aes(x = year)) +
#   geom_textpath(aes(colour = r10, y = Median, label = r10, hjust = hjust), alpha = 1, size = 3) +
#   scale_x_continuous(breaks = c(1990, seq(2000,2100,20))) +
#   scale_y_continuous(labels = scales::percent_format(), 
#                      position = "right") +
#   coord_cartesian(ylim = c(0,0.4)) +
#   scale_colour_discrete_qualitative() +
#   facet_wrap(~fct_rev(case), ncol = 2, strip.position = "left") +
#   theme_bw() +
#   theme(legend.position = "top",
#         strip.background = element_blank(), strip.placement = "inside", strip.text = element_text(hjust = 0),
#         strip.text.x = element_text(size = 12),
#         axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
#         axis.text = element_text(size = 12),
#         axis.title = element_text(size = 13), panel.grid = element_blank()) +
#   guides(fill = "none", colour = "none") +
#   labs(x = NULL, y = NULL,
#        subtitle = "Exceedance responsibility (% of GtCO2)")

wrap_plots(a,b, ncol = 2, widths = c(0.5,1)) +
  plot_annotation(tag_levels = list("a"), tag_prefix = "(", tag_suffix = ")", 
                  caption = paste0(paste0(r10order$r10label[1:5], ": ",r10order$r10labellong[1:5], collapse = ", "), 
                                   "\n", paste0(r10order$r10label[6:10], ": ",r10order$r10labellong[6:10], collapse = ", "), collapse = ""))

ggsave(here("Manuscript", "Figures", "fig2.svg"),
       height = 4, width = 10)

 # SI 

r10_ndclts_impren_rcbyear %>% 
  ungroup() %>% 
  filter(case != "IMP-REN") %>% 
  mutate(
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
    case = factor(case, levels = c("A", "E", "IMP-REN"),
                  labels = c("CurPol", "CurPledge", "IMP-REN")),
    hjust = as.numeric(r10) / 8,
    exceedanceshareyear = exceedanceshareyear * exceedanceyear, 
    ppp_pf = ifelse(is.na(ppp_pf), "", ppp_pf)) %>% 
  filter(ppp_pf %in% c("", "PPP_1/sqrt(x)"), year >= 2020) %>% 
  select(r10, case, aggregate, year, exceedanceshareyear, category, ppp_pf, hjust) %>% 
  group_by(case, year, aggregate, category, ppp_pf) %>%
  mutate(exceedanceshareyear = exceedanceshareyear / sum(exceedanceshareyear)) %>% 
  pivot_wider(names_from = aggregate, values_from = exceedanceshareyear) %>% 
  ggplot(aes(x = year, group = interaction(category, ppp_pf))) +
  geom_path(aes(colour = interaction(category, ppp_pf), y = Median, label = r10, hjust = hjust), alpha = 1) +
  scale_x_continuous(breaks = c(seq(2020,2100,20))) +
  scale_y_continuous(labels = scales::percent_format(), position = "left") +
  scale_colour_manual(values = c("#1f78b4", "#33a02c", "#980043", "#a6cee3",  "#b2df8a", "#c994c7")) +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#980043", "#a6cee3", "#b2df8a", "#c994c7")) +
  facet_grid(fct_rev(case) ~ r10) +
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_blank(), strip.placement = "inside", strip.text = element_text(hjust = 0),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13), panel.grid = element_blank()) +
  guides(colour = guide_legend(nrow = 3)) +
  labs(x = NULL, y = "Temporal overshoot responsibility (% of total exceedance)",
       colour = "Allocation approach",
       fill = "Allocation approach",
       caption = paste0(paste0(r10order$r10label[1:5], ": ",r10order$r10labellong[1:5], collapse = ", "), 
                        "\n", paste0(r10order$r10label[6:10], ": ",r10order$r10labellong[6:10], collapse = ", "), collapse = ""))

ggsave(here("Manuscript", "Figures", "SI", "SI_fig2.png"),
       height = 8, width = 14)

r10_ndclts_impren_rcbyear %>% 
  ungroup() %>% 
  filter(category == "PP1990", case != "IMP-REN", 
         aggregate == "Median") %>% 
  mutate(
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
    case = factor(case, levels = c("A", "E", "IMP-REN"),
                  labels = c("CurPol", "CurPledge", "IMP-REN")),
    exceedanceshareyear = exceedanceshareyear * exceedanceyear) %>% 
  select(r10, case, aggregate, year, exceedanceshareyear) %>% 
  group_by(case, year, aggregate) %>%
  mutate(exceedanceshareyear = exceedanceshareyear / sum(exceedanceshareyear)) %>% 
  pivot_wider(names_from = case, values_from = exceedanceshareyear) %>% 
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
       
ggsave(here("Manuscript", "Figures", "SI", "SI_fig2b.png"),
       height = 8, width = 14)

# FIGURE 3 ---------------------------------------------------------------------

a_data <- r10_ndclts_impren_debt %>%
  ungroup() %>%
  filter(category == "PP1990", case != "IMP-REN", aggregate == "Median") %>%
  select(r10, case, aggregate, rcb2100, pop_cmltv_rem_2050) %>%
  group_by(case, aggregate) %>% 
  mutate(global_min_drawdown_rate = sum(ifelse(rcb2100 < 0, rcb2100 * 1e9, 0)) / sum(pop_cmltv_rem_2050)) %>% 
  group_by(r10, case, aggregate) %>%
  mutate(min_drawdown_rate = ifelse(rcb2100 < 0, rcb2100 * 1e9 / pop_cmltv_rem_2050, 0),
         debt = ifelse(rcb2100 < 0, -rcb2100, 0)) %>%
  pivot_wider(names_from = aggregate, values_from = min_drawdown_rate)

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
  geom_segment(aes(y = global_min_drawdown_rate, yend = `Median`, colour = case),
               position = position_dodge(width = 0.2)) +
  geom_point(aes(y = `Median`, colour = case),
             position = position_dodge(width = 0.2)) +
  scale_colour_discrete_qualitative(drop = F) +
  scale_fill_discrete_qualitative(drop = F) +
  scale_size_continuous(breaks = scales::pretty_breaks(n = 4)) +
  scale_y_continuous(sec.axis = sec_axis(~ . / a_curpol_relminrateglb, name = "Relative to global average",
                                         labels = scales::dollar_format(suffix = "x", prefix = ""))) +
  coord_cartesian(ylim = c(-24, 0)) +
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
  labs(x = NULL, y = "tCO2 / person / year")

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
  geom_segment(aes(y = global_min_drawdown_rate, yend = `Median`, colour = case),
               position = position_dodge(width = 0.2)) +
  geom_point(aes(y = `Median`, colour = case),
               position = position_dodge(width = 0.2)) +
  scale_colour_discrete_qualitative(drop = F) +
  scale_fill_discrete_qualitative(drop = F) +
  scale_size_continuous(breaks = scales::pretty_breaks(n = 4)) +
  scale_y_continuous(sec.axis = sec_axis(~ . / a_curpledge_relminrateglb, name = "Relative to global average",
                                         labels = scales::dollar_format(suffix = "x", prefix = ""),
                                         breaks = c(0,2,4,6,8))) +
  coord_cartesian(ylim = c(-24, 0)) +
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
  labs(x = NULL, y = "tCO2 / person / year")

b <- r10_exp_heatwave_emf_temp_debt %>%
  filter(case != "IMP-REN", category == "PP1990", aggregate == "Median") %>%
  mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
         case = factor(case, levels = c("A", "E", "IMP-REN"),
                       labels = c("CurPol", "CurPledge", "IMP-REN"))) %>% 
  ungroup() %>% 
  ggplot(aes(x = birth_year, y = add_impren_0.5, colour = case, fill = case)) +
  geom_hline(yintercept = 0, linetype = 2, size = .3) +
  geom_ribbon(aes(ymin = add_impren_0.33, ymax = add_impren_0.66), alpha = 0.2,
              linewidth = 0, show.legend = F) +
  geom_path(alpha = 1, size = 1) +
  facet_grid(case~r10) +
  scale_colour_discrete_qualitative() +
  scale_x_continuous(breaks = c(1980, 2000, 2020)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  labs(x = "Cohort birth year",
       y = "Lifetime years",
       shape = "Cohort birth year") +
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_blank(), strip.placement = "inside", 
        strip.text = element_text(hjust = 0),
        strip.text.x = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()) +
  guides(colour = "none")

wrap_plots(
  wrap_plots(a_curpol, a_curpledge + plot_layout(tag_level = 'new')) &
    plot_annotation(subtitle = "Minimum annual per capita carbon drawdown rate 2050-2100, to address regional budget exceedance"),
  b, ncol = 1, heights = c(0.7,1)) +  
  plot_annotation(tag_levels = list("a"), tag_prefix = "(", tag_suffix = ")", 
                  caption = paste0(paste0(r10order$r10label[1:5], ": ",r10order$r10labellong[1:5], collapse = ", "), 
                                   "\n", paste0(r10order$r10label[6:10], ": ",r10order$r10labellong[6:10], collapse = ", "), collapse = ""))

ggsave(here("Manuscript", "Figures", "fig3.png"),
       height = 8, width = 10)
