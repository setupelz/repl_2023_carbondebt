# Replication archive for: "Delaying Carbon Debt Drawdown Fails Younger Generations"

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
iso3c_tbl <- read_csv(here("Data", "countrygroups", "iso3c_region_mapping.csv")) %>% 
  select(country.name, iso3c, r10 = iamc_r10)

# Set consistent r10 ordering
r10order <- tibble(r10 = c("NAM", "EUR", "PAO", "FSU", "MEA", "EAS", "LAM", "PAS", "AFR", "SAS"),
                   r10label = c("NAM", "EUR", "APD", "EEA", "MEA", "EAS", "LAC", "SAP", "AFR", "SAS"),
                   r10labellong = c("North America", "Europe", "Asia-Pacific Developed",
                                    "Eastern Europe and West-Central Asia", "Middle East", "Eastern Asia",
                                    "Latin America and Caribbean", "South-East Asia and developing Pacific",
                                    "Africa", "Southern Asia"))

# Population
projected_pop <- read_csv(here("Data", "processed", "iso3c_popssp218502100.csv")) %>% 
  filter(r10 %in% r10order$r10, iso3c %in% iso3c_tbl$iso3c) 

# RCB quantities
rcb <- read_csv(here("Data", "processed", "rcbquantities.csv"))

# Regional RCBs from 1990 to 2020
r10_rcb19902020gtco2 <- read_csv(here("Data", "processed", "r10_rcb19902020.csv")) %>% 
  filter(year >= 1990, year <= 2020) 

# Assessed pathways
r10_ndclts_impren_emiss <- read_csv(here("Data", "processed", "r10_ndclts_impren_emiss.csv"))

# Heatwave EMFs
exp_heatwave_r10_emf <- read_csv(here("Data", "processed", "exp_heatwave_r10_emf.csv"))

# ASSESS MODELLED NDC / NET-ZERO PATHWAYS --------------------------------------

# Determine regional remaining carbon budget allocation evolution along assessed paths
r10_ndclts_impren_rcbyear <- r10_ndclts_impren_emiss %>%
  mutate(gtco2_cdr = ifelse(gtco2 < 0, -gtco2, 0),
         gtco2 = ifelse(gtco2 < 0 , 0, gtco2)) %>% 
  group_by(case, aggregate, r10) %>% 
  mutate(gtco2_cmltv = cumsum(gtco2),
         gtco2_cdr_cmltv = cumsum(gtco2_cdr)) %>% 
  left_join(r10_rcb19902020gtco2 %>% distinct(r10, category, ppp_pf)) %>% 
  left_join(r10_rcb19902020gtco2 %>% filter(year == 1990) %>% 
              select(r10, category, ppp_pf, rcb1990 = rcb)) %>% 
  mutate(rcbyear = rcb1990 - gtco2_cmltv)

# Determine regional net-zero carbon debt associated with assessed paths
r10_ndclts_impren_debt <- r10_ndclts_impren_emiss %>%
  mutate(gtco2_cdr = ifelse(gtco2 < 0, -gtco2, 0),
         gtco2 = ifelse(gtco2 < 0 , 0, gtco2)) %>% 
  group_by(case, aggregate, r10) %>% 
  mutate(gtco2_cmltv = cumsum(gtco2),
         gtco2_cdr_cmltv = cumsum(gtco2_cdr)) %>% 
  filter(year == 2100) %>% 
  left_join(r10_rcb19902020gtco2 %>% distinct(r10, category, ppp_pf)) %>% 
  left_join(r10_rcb19902020gtco2 %>% filter(year == 1990) %>% 
              select(r10, category, ppp_pf, rcb1990 = rcb)) %>% 
  mutate(rcb2100 = rcb1990 - gtco2_cmltv) %>% 
  select(r10, case, aggregate, category, ppp_pf, rcb1990, gtco2_19902100 = gtco2_cmltv, rcb2100, gtco2_cdr_19902100 = gtco2_cdr_cmltv) %>% 
  mutate(ppp_pf = ifelse(is.na(ppp_pf), "NA", ppp_pf)) %>% 
  # determine pathway 1.5 RCB exceedances, total regional nz debts and offsets
  group_by(case, aggregate, category, ppp_pf) %>% 
  mutate(exceedance = -(rcb %>% filter(rcb == "rcb1990_nz") %>% pull(gtco2) - sum(gtco2_19902100)),
         totaldebt = sum(ifelse(rcb2100 < 0, rcb2100, NA_real_), na.rm = T),
         totaloffset = sum(ifelse(rcb2100 > 0, rcb2100, NA_real_), na.rm = T),
         drawdown_resp = ifelse(rcb2100 < 0, rcb2100 / totaldebt * exceedance, 0)) %>% 
  left_join(projected_pop %>% group_by(r10, year) %>% 
              summarise(pop = sum(pop, na.rm = T)) %>% 
              mutate(pop_cmltv = cumsum(pop),
                     pop_cmltv_rem_2050 = pop_cmltv[year == 2100] - pop_cmltv[year == 2050]) %>% 
              filter(year == 2100) %>% 
              select(r10, pop_cmltv_rem_2050)) %>% 
  ungroup()

# Add in r10 exceedance drawdown responsibility
exp_heatwave_r10_emf_temp_r10debt <- left_join(exp_heatwave_r10_emf,
                                               r10_ndclts_impren_debt,
                                               by = c("case", "r10"))

# FIGURE 2 ---------------------------------------------------------------------

a <- r10_ndclts_impren_debt %>% 
  filter(category == "1_PP1990", case != "IMP-REN") %>% 
  distinct(case, aggregate, exceedance, totaldebt, totaloffset) %>% 
  mutate(totaldebt = -totaldebt, totaloffset = -totaloffset,
         case = factor(case, levels = c("A", "C", "E", "IMP-REN"),
                       labels = c("Current policies", "Current policies and\nnet-zero targets",
                                  "All pledges and\nnet-zero targets", "IMP-REN"))) %>% 
  rename(`NZ carbon debt` = totaldebt,
         `NZ carbon credit` = totaloffset) %>% 
  pivot_longer(-c(case, aggregate)) %>% 
  pivot_wider(names_from = aggregate, values_from = value) %>% 
  ggplot() +
  geom_col(aes(x = fct_rev(case), y = Median, fill = fct_rev(name)), position = "stack", width = 0.3,
           data = . %>% filter(name != "exceedance"), alpha = 0.5, show.legend = F) +
  geom_errorbar(aes(x = case, ymin = Min, ymax = Max, colour = fct_rev(name)), width = 0.2, linewidth = 0.8,
                data = . %>% filter(name != "exceedance"),  show.legend = F) +
  geom_point(aes(x = case, y = Median, shape = "Total 1.5C RCB exceedance"), size = 3,
             data = . %>% filter(name == "exceedance")) +
  geom_errorbar(aes(x = case, ymin = Min, ymax = Max), width = 0.1, linewidth = 0.8,
                data = . %>% filter(name == "exceedance")) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "red") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7)) +
  scale_fill_manual(values = c("maroon", "lightgrey")) +
  scale_colour_manual(values = c("maroon", "lightgrey")) +
  theme_bw() + 
  labs(x = NULL, y = "Total net-zero carbon debt (GtCO2-FFI)", fill = NULL,
       shape = NULL) +
  guides(shape = guide_legend(label.theme = element_text(size = 15))) +
  theme_bw() +
  theme(legend.position = "top", legend.box = "vertical", legend.direction = "vertical", legend.margin = unit(x = 0.5, units = "mm"),
        legend.justification = "left", legend.box.just = "left")

b <- r10_ndclts_impren_debt %>% 
  mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
         debt = -rcb2100) %>% 
  filter(category == "1_PP1990", case != "IMP-REN") %>% 
  distinct(case, aggregate, category, r10, debt, drawdown_resp) %>% 
  mutate(
    case = factor(case, levels = c("A", "C", "E", "IMP-REN"),
                  labels = c("Current policies", "Current policies and\nnet-zero targets",
                             "All pledges and\nnet-zero targets", "IMP-REN"))) %>% 
  pivot_wider(names_from = aggregate, values_from = c(debt, drawdown_resp)) %>% 
  group_by(case, category) %>% 
  ggplot(aes(x = fct_rev(case), fill = r10, colour = r10)) +
  geom_col(aes(y = debt_Median), position = position_dodge(width = 0.7), alpha = 0.5, width = 0.7) +
  geom_errorbar(aes(y = debt_Median, ymin = debt_Min, ymax = debt_Max), position = position_dodge(width = 0.7), width = 0.5) +
  geom_text(aes(y = ifelse(debt_Median < 0, ifelse(case == "IMP-REN", debt_Median, debt_Min), ifelse(case == "IMP-REN", debt_Median, debt_Max)), hjust = ifelse(debt_Median < 0, 1.1, -0.1), label = r10), 
            position = position_dodge(width = 0.7), size = 3, angle = 90) +
  geom_errorbar(aes(y = drawdown_resp_Median, ymin = drawdown_resp_Min, ymax = drawdown_resp_Max), position = position_dodge(width = 0.7), 
                colour = "black", width = 0.2) +
  geom_point(aes(y = drawdown_resp_Median, shape = "Regional 1.5C RCB exceedance responsibility"), position = position_dodge(width = 0.7), 
                colour = "black") +
  geom_hline(yintercept = 0, linetype = 2, colour = "red") +
  scale_fill_discrete_qualitative(palette = "Harmonic") +
  scale_colour_discrete_qualitative(palette = "Harmonic") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7)) +
  labs(x = NULL, y = "Regional net-zero carbon debt (GtCO2-FFI)", shape = NULL) +
  guides(fill = "none", colour = "none", 
         shape = guide_legend(label.theme = element_text(size = 15), override.aes = aes(size = 3.1))) +
  theme_bw() +
  theme(legend.position = "top", legend.box = "vertical", legend.direction = "vertical", legend.margin = unit(x = 0.5, units = "mm"),
        legend.justification = "left", legend.box.just = "left")

c <- r10_ndclts_impren_rcbyear %>% 
  filter(category == "1_PP1990", case != "IMP-REN", aggregate == "Median") %>% 
  mutate(
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
    case = factor(case, levels = c("A", "C", "E", "IMP-REN"),
                  labels = c("Current policies", "Current policies and net-zero targets",
                             "All pledges and net-zero targets", "IMP-REN"))) %>% 
  ggplot(aes(x = year, y = rcbyear, colour = r10)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_textpath(aes(label = r10), hjust = "auto",
                alpha = 0.8) +
  geom_text(x = 2100, y = -30, aes(label = ifelse(case == "Current policies", "Debt", "")), 
            alpha = 1, hjust = 1, colour = "darkgrey", size = 3,
            data = . %>% distinct(case)) +
  geom_text(x = 2100, y = 30, aes(label = ifelse(case == "Current policies", "Credit", "")),
            alpha = 1, hjust = 1, colour = "darkgrey", size = 3,
            data = . %>% distinct(case)) +
  scale_x_continuous(breaks = c(1990, seq(2000,2100,20))) +
  scale_colour_discrete_qualitative(palette = "Harmonic") +
  facet_wrap(~fct_rev(case)) +
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_blank(), strip.placement = "inside", strip.text = element_text(hjust = 0),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13), panel.grid = element_blank()) +
  guides(colour = "none") +
  labs(x = NULL, 
       y = "Remaining carbon budget allocation (GtCO2)")

wrap_plots(wrap_plots(a,b, ncol = 2, widths = c(0.85,1)), c, ncol = 1, heights = c(1, 0.7)) + 
  plot_annotation(tag_levels = list("a"), tag_prefix = "(", tag_suffix = ")", 
                  caption = "NAM: North America, EUR: Europe, APD: Asia-Pacific Developed, EEA: Eastern Europe and West-Central Asia, MEA: Middle East\nEAS: Eastern Asia, LAC: Latin America and Caribbean, SAP: South-East Asia and developing Pacific, AFR: Africa, SAS: Southern Asia") & 
  theme(legend.position = "top", legend.justification = "left",
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        panel.grid = element_blank())

ggsave(here("Manuscript", "Figures", "fig2.png"),
       height = 13, width = 13)

# FIGURE 3 ---------------------------------------------------------------------

exp_heatwave_r10_emf_temp_r10debt %>%
  filter(birth_year %in% c(2020), case != "IMP-REN", quantile == 0.66, category == "1_PP1990",
         aggregate == "Median") %>%
  mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
         case = factor(case, levels = c("A", "C", "E", "IMP-REN"),
                       labels = c("Current policies", "Current policies and all net-zero targets",
                                  "All pledges and net-zero targets", "IMP-REN"))) %>% 
  ungroup() %>% 
  mutate(drawdown_resp_share = drawdown_resp / -(totaldebt + totaloffset),
         debt_ratio = rcb2100 / rcb1990) %>% 
  ggplot(aes(y = -debt_ratio + 1)) +
  geom_hline(yintercept = 1, linetype = 2, linewidth = 0.5) +
  geom_errorbar(aes(xmin = emf_impren_0.33, xmax = emf_impren_0.66, colour = r10), alpha = 0.6) +
  geom_text(x = 8, y = 1.4, aes(label = ifelse(case == "Current policies", "Debt", "")), 
            alpha = 1, hjust = 1, colour = "darkgrey",
            data = . %>% distinct(case)) +
  geom_text(x = 8, y = 0.6, aes(label = ifelse(case == "Current policies", "Credit", "")), 
            alpha = 1, hjust = 1, colour = "darkgrey",
            data = . %>% distinct(case)) +
  geom_point(aes(x = emf_impren_0.5, colour = r10, size = drawdown_resp/50),  alpha = 0.6) +
  geom_point(aes(x = emf_impren_0.5, colour = r10), size = 0.5,  alpha = 0.8) +
  geom_text(aes(x = emf_impren_0.5, label = r10, colour = r10), angle = 45, size = 2.5, hjust = -0.4) +
  scale_x_continuous(labels = scales::dollar_format(prefix = "", suffix = "x"),
                     breaks = seq(1,9,1)) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = "x"),
                     breaks = seq(1,10,1), limits = c(0.5,10)) +  
  scale_size_continuous(range = c(1,15)) +
  facet_wrap(~fct_rev(case)) +
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_blank(), strip.placement = "inside", strip.text = element_text(hjust = 0),
        strip.text.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13), panel.grid = element_blank()) +
  guides(colour = "none",
         size = guide_legend(nrow = 1)) +
  labs(y = "Cumulative CO2-FFI emissions as multiples of regional allocations", 
       size = "Required average annual exceedance drawdown (GtCO2/yr, 2050-2100)",
       x = "Increase in 2020 birth cohort lifetime heatwave exposure relative to illustrative 1.5C pathway (IMP-REN, AR6)",
       caption = "NAM: North America, EUR: Europe, APD: Asia-Pacific Developed, EEA: Eastern Europe and West-Central Asia, MEA: Middle East\nEAS: Eastern Asia, LAC: Latin America and Caribbean, SAP: South-East Asia and developing Pacific, AFR: Africa, SAS: Southern Asia")

ggsave(here("Manuscript", "Figures", "fig3.png"),
       height = 7, width = 12)

r10_ndclts_impren_rcbyear %>%
  
  mutate(
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
    case = factor(case, levels = c("A", "C", "E", "IMP-REN"),
                  labels = c("Current policies", "Current policies and net-zero targets",
                             "All pledges and net-zero targets", "IMP-REN")),
    
    ppp_pf = ifelse(is.na(ppp_pf), "NA", ppp_pf),
    category = ifelse(grepl(category, pattern = "1990"), "PP1990", "PP1850")) %>% 
  
  filter(case != "IMP-REN", aggregate == "Median",
         ppp_pf %in% c("NA", "MER_1/sqrt(x)", "PPP_1/sqrt(x)")) %>%
  
  mutate(category = ifelse(ppp_pf == "NA", category, paste(category, ", ATP adjustment: ", ppp_pf))) %>% 
  
  ggplot(aes(x = year, y = rcbyear, colour = r10)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_textpath(aes(label = r10), hjust = "auto",
                alpha = 0.8) +
  scale_x_continuous(breaks = c(1990, seq(2000,2100,20))) +
  scale_colour_discrete_qualitative(palette = "Harmonic") +
  facet_grid(fct_rev(case) ~ category) +
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_blank(), strip.placement = "inside", strip.text = element_text(hjust = 0),
        strip.text.x = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13), panel.grid = element_blank()) +
  guides(colour = "none") +
  labs(x = NULL, 
       y = "Remaining carbon budget allocation (GtCO2)")

ggsave(here("Manuscript", "Figures", "SI", "SI_fig3c.png"),
       height = 10, width = 14)
