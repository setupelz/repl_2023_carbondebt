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
  mutate(r10 = ifelse(is.na(r10_iamc), NA_real_, r10_unif)) %>% 
  select(iso3c, r10)

# Set consistent r10 ordering
r10order <- tibble(r10 = c("R10NAM", "R10EUR", "R10PAO", "R10FSU", "R10EASPAS", "R10LAM", "R10AFRMEA", "R10SAS"),
                   r10label = c("NAM", "EUR", "APD", "EEA", "EASPAS", "LAC", "AFRMEA", "SAS"),
                   r10labellong = c("North America", "Europe", "Asia-Pacific Developed",
                                    "Eastern Europe and West-Central Asia", "Africa & Middle East", 
                                    "Eastern & South-East Asia and developing Pacific",
                                    "Latin America and Caribbean", "Southern Asia"))

# Population, aggregated to r10
r10_popproj <- read_xlsx(here("Data", "processed", "analysisdata.xlsx"),
                         sheet = "popproj") %>% 
  group_by(r10, year) %>% 
  summarise(pop = sum(pop)) %>% 
  arrange(year)

# RCB quantities
rcb <- read_csv(here("Data", "processed", "rcbquantities.csv"))

# Regional RCBs from 1990 to 2020
r10_rcb19902020gtco2 <- read_csv(here("Data", "processed", "r10_rcb19902020.csv")) %>% 
  filter(year >= 1990, year <= 2020) 

# Assessed pathways
r10_ndclts_impren_emiss <- read_csv(here("Data", "processed", "r10_ndclts_impren_emiss.csv"))

# Heatwave EMFs
r10_exp_heatwave_emf <- read_csv(here("Data", "processed", "r10_exp_heatwave_emf.csv"))

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
  mutate(rcbyear = rcb1990 - gtco2_cmltv) %>% 
  arrange(model, case, aggregate, category, ppp_pf, r10, year) %>% 
  group_by(model, case, aggregate, category, ppp_pf, year) %>% 
  mutate(exceedanceyear = ifelse(sum(-rcbyear) > 0, sum(-rcbyear), 0),
         debtyear = ifelse(-rcbyear > 0, -rcbyear, 0)) %>% 
  group_by(r10, model, case, aggregate, category, ppp_pf) %>% 
  mutate(exceedancecmltv = cumsum(exceedanceyear),
         debtcmltv = cumsum(debtyear)) %>% 
  group_by(model, case, aggregate, category, ppp_pf, year) %>% 
  mutate(exceedanceshareyear = debtyear / sum(debtyear),
         exceedancesharecmltv = debtcmltv / sum(debtcmltv))

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
  left_join(r10_popproj %>% 
              mutate(pop_cmltv = cumsum(pop),
                     pop_cmltv_rem_2050 = pop_cmltv[year == 2100] - pop_cmltv[year == 2050]) %>% 
              filter(year == 2100) %>% 
              select(r10, pop_cmltv_rem_2050)) %>% 
  ungroup()

# Add in r10 exceedance drawdown responsibility
r10_exp_heatwave_emf_temp_debt <- left_join(r10_exp_heatwave_emf,
                                               r10_ndclts_impren_debt,
                                               by = c("case", "r10"))

# FIGURE 2 ---------------------------------------------------------------------

a <- r10_ndclts_impren_rcbyear %>% 
  ungroup() %>% 
  filter(category == "1_PP1990", case != "IMP-REN") %>% 
  select(r10, case, aggregate, year, rcbyear) %>% 
  mutate(
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
    case = factor(case, levels = c("A", "C", "E", "IMP-REN"),
                  labels = c("Current policies", "Current policies and net-zero targets",
                             "All pledges and net-zero targets", "IMP-REN")),
    hjust = as.numeric(r10) / 8) %>% 
  pivot_wider(names_from = aggregate, values_from = rcbyear) %>% 
  ggplot(aes(x = year)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_ribbon(aes(fill = r10, ymin = Min, ymax = Max), alpha = 0.3) +
  geom_textpath(aes(colour = r10, y = Median, label = r10, hjust = hjust), alpha = 1, size = 3) +
  geom_text(x = 2100, y = -30, aes(label = ifelse(case == "Current policies", "Debt", "")), 
            alpha = 1, hjust = 1, colour = "darkgrey", size = 3,
            data = . %>% distinct(case)) +
  geom_text(x = 2100, y = 30, aes(label = ifelse(case == "Current policies", "Credit", "")),
            alpha = 1, hjust = 1, colour = "darkgrey", size = 3,
            data = . %>% distinct(case)) +
  scale_x_continuous(breaks = c(1990, seq(2000,2100,20))) +
  scale_y_continuous(position = "right") +
  scale_colour_discrete_qualitative() +
  scale_fill_discrete_qualitative() +
  facet_wrap(~fct_rev(case), ncol = 1, strip.position = "left") +
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_blank(), strip.placement = "inside", strip.text = element_text(hjust = 0),
        strip.text.x = element_text(size = 12),
        # Change y axis ticks to right side
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13), panel.grid = element_blank()) +
  guides(colour = "none", fill = "none") +
  labs(x = NULL, y = NULL,
       subtitle = "Remaining budget (GtCO2)")

b <- r10_ndclts_impren_rcbyear %>% 
  filter(category == "1_PP1990", case != "IMP-REN", r10 == "R10NAM") %>% 
  select(r10, case, aggregate, year, exceedanceyear) %>% 
  mutate(
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
    case = factor(case, levels = c("A", "C", "E", "IMP-REN"),
                  labels = c("Current policies", "Current policies and net-zero targets",
                             "All pledges and net-zero targets", "IMP-REN"))) %>% 
  pivot_wider(names_from = aggregate, values_from = exceedanceyear) %>% 
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = Min, ymax = Max), fill = "black", alpha = 0.3) +
  geom_path(aes(y = Median), colour = "black") +
  facet_wrap(~fct_rev(case), scales = "free", ncol = 1)  +
  scale_x_continuous(breaks = c(1990, seq(2000,2100,20))) +
  scale_y_continuous(position = "right") +
  scale_fill_discrete_qualitative() +
  facet_wrap(~fct_rev(case), ncol = 1, strip.position = "left") +
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_blank(), strip.placement = "inside", strip.text = element_text(hjust = 0),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13), panel.grid = element_blank()) +
  guides(fill = "none") +
  labs(x = NULL, y = NULL,
       subtitle = "Global exceedance (GtCO2)")

c <- r10_ndclts_impren_rcbyear %>% 
  ungroup() %>% 
  filter(category == "1_PP1990", case != "IMP-REN") %>% 
  mutate(
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
    case = factor(case, levels = c("A", "C", "E", "IMP-REN"),
                  labels = c("Current policies", "Current policies and net-zero targets",
                             "All pledges and net-zero targets", "IMP-REN")),
    hjust = as.numeric(r10) / 8,
    exceedanceshareyear = exceedanceshareyear * exceedanceyear) %>% 
  select(r10, case, aggregate, year, exceedanceshareyear, hjust) %>% 
  group_by(case, year, aggregate) %>%
  mutate(exceedanceshareyear = exceedanceshareyear / sum(exceedanceshareyear)) %>% 
  pivot_wider(names_from = aggregate, values_from = exceedanceshareyear) %>% 
  ggplot(aes(x = year)) +
  geom_ribbon(aes(fill = r10, ymin = Min, ymax = Max), alpha = 0.3) +
  geom_textpath(aes(colour = r10, y = Median, label = r10, hjust = hjust), alpha = 1, size = 3) +
  scale_x_continuous(breaks = c(1990, seq(2000,2100,20))) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,0.6), 
                     position = "right") +
  scale_colour_discrete_qualitative() +
  facet_wrap(~fct_rev(case), ncol = 1, strip.position = "left") +
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_blank(), strip.placement = "inside", strip.text = element_text(hjust = 0),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13), panel.grid = element_blank()) +
  guides(fill = "none", colour = "none") +
  labs(x = NULL, y = NULL,
       subtitle = "Exceedance responsibility (% of GtCO2)")

d <- r10_ndclts_impren_rcbyear %>% 
  ungroup() %>% 
  filter(category == "1_PP1990", case != "IMP-REN") %>% 
  mutate(
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
    case = factor(case, levels = c("A", "C", "E", "IMP-REN"),
                  labels = c("Current policies", "Current policies and net-zero targets",
                             "All pledges and net-zero targets", "IMP-REN")),
    hjust = as.numeric(r10) / 8,
    exceedancesharecmltv = exceedancesharecmltv * exceedancecmltv) %>% 
  select(r10, case, aggregate, year, exceedancesharecmltv, hjust) %>% 
  group_by(case, year, aggregate) %>%
  mutate(exceedancesharecmltv = exceedancesharecmltv / sum(exceedancesharecmltv)) %>% 
  pivot_wider(names_from = aggregate, values_from = exceedancesharecmltv) %>% 
  ggplot(aes(x = year)) +
  geom_ribbon(aes(fill = r10, ymin = Min, ymax = Max), alpha = 0.3) +
  geom_textpath(aes(colour = r10, y = Median, label = r10, hjust = hjust), alpha = 1, size = 3) +
  scale_x_continuous(breaks = c(1990, seq(2000,2100,20))) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,0.6),
                     position = "right") +
  scale_colour_discrete_qualitative() +
  facet_wrap(~fct_rev(case), ncol = 1, strip.position = "left") +
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_blank(), strip.placement = "inside", strip.text = element_text(hjust = 0),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13), panel.grid = element_blank()) +
  guides(fill = "none", colour = "none") +
  labs(x = NULL, y = NULL,
       subtitle = "Cumulative responsibility (% of GtCO2 x Yrs)")

wrap_plots(a,b,c,d, ncol = 4) 

ggsave(here("Manuscript", "Figures", "fig2.png"),
       height = 6, width = 14)

# SI 

a <- r10_ndclts_impren_rcbyear %>% 
  ungroup() %>% 
  filter(case != "IMP-REN") %>% 
  mutate(
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
    case = factor(case, levels = c("A", "C", "E", "IMP-REN"),
                  labels = c("Current policies", "Current policies and net-zero targets",
                             "All pledges and net-zero targets", "IMP-REN")),
    hjust = as.numeric(r10) / 8,
    exceedanceshareyear = exceedanceshareyear * exceedanceyear, 
    ppp_pf = ifelse(is.na(ppp_pf), "", ppp_pf)) %>% 
  filter(ppp_pf %in% c("", "PPP_1/sqrt(x)")) %>% 
  select(r10, case, aggregate, year, exceedanceshareyear, category, ppp_pf, hjust) %>% 
  group_by(case, year, aggregate, category, ppp_pf) %>%
  mutate(exceedanceshareyear = exceedanceshareyear / sum(exceedanceshareyear)) %>% 
  pivot_wider(names_from = aggregate, values_from = exceedanceshareyear) %>% 
  ggplot(aes(x = year, group = interaction(category, ppp_pf))) +
  geom_ribbon(aes(fill = interaction(category, ppp_pf), ymin = Min, ymax = Max), alpha = 0.6) +
  geom_path(aes(colour = interaction(category, ppp_pf), y = Median, label = r10, hjust = hjust), alpha = 1) +
  scale_x_continuous(breaks = c(1990, seq(2000,2100,20))) +
  scale_y_continuous(labels = scales::percent_format(), position = "left") +
  scale_colour_discrete_qualitative() +
  facet_grid(fct_rev(case) ~ r10) +
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_blank(), strip.placement = "inside", strip.text = element_text(hjust = 0),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13), panel.grid = element_blank()) +
  guides(fill = "none") +
  labs(x = NULL, y = NULL,
       title = "Regional exceedance responsibility (% of GtCO2)",
       colour = "Allocation approach")

b <- r10_ndclts_impren_rcbyear %>% 
  ungroup() %>% 
  filter(case != "IMP-REN") %>% 
  mutate(
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
    case = factor(case, levels = c("A", "C", "E", "IMP-REN"),
                  labels = c("Current policies", "Current policies and net-zero targets",
                             "All pledges and net-zero targets", "IMP-REN")),
    hjust = as.numeric(r10) / 8,
    exceedancesharecmltv = exceedancesharecmltv * exceedancecmltv,
    ppp_pf = ifelse(is.na(ppp_pf), "", ppp_pf)) %>% 
  filter(ppp_pf %in% c("", "PPP_1/sqrt(x)")) %>% 
  select(r10, case, aggregate, year, exceedancesharecmltv, category, ppp_pf, hjust) %>% 
  group_by(case, year, aggregate, category, ppp_pf) %>%
  mutate(exceedancesharecmltv = exceedancesharecmltv / sum(exceedancesharecmltv)) %>% 
  pivot_wider(names_from = aggregate, values_from = exceedancesharecmltv) %>% 
  ggplot(aes(x = year, group = interaction(category, ppp_pf))) +
  geom_ribbon(aes(fill = interaction(category, ppp_pf), ymin = Min, ymax = Max), alpha = 0.6) +
  geom_path(aes(colour = interaction(category, ppp_pf), y = Median, label = r10, hjust = hjust), alpha = 1) +
  scale_x_continuous(breaks = c(1990, seq(2000,2100,20))) +
  scale_y_continuous(labels = scales::percent_format(), position = "left") +
  scale_colour_discrete_qualitative() +
  facet_grid(fct_rev(case) ~ r10) +
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_blank(), strip.placement = "inside", strip.text = element_text(hjust = 0),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13), panel.grid = element_blank()) +
  guides(fill = "none") +
  labs(x = NULL, y = NULL,
       title = "Regional cumulative exceedance responsibility (% of GtCO2 x Yrs)",
       colour = "Allocation approach")

wrap_plots(a,b, ncol = 1) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave(here("Manuscript", "Figures", "SI", "SI_fig2.png"),
       height = 8, width = 14)

# FIGURE 3 ---------------------------------------------------------------------

r10_exp_heatwave_emf_temp_debt %>%
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
                     breaks = seq(1,10,1), limits = c(0.5,10), position = "left") +  
  scale_size_continuous(range = c(1,15)) +
  facet_wrap(~fct_rev(case), strip.position = "right", scales = "free_y") +
  theme_bw() +
  theme(legend.position = "bottom",
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
       height = 6, width = 14)
