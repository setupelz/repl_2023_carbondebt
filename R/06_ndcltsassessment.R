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
  mutate(pop_cmltv = cumsum(pop)) %>% 
  arrange(r10, year)

# Calculate population from year to 2050
r10_poprem <- tibble(.rows = 0)

for (i in 1990:2020) {
  
  pop_rem_loop <- r10_popproj %>% 
    filter(year >= i & year <= 2050) %>% 
    summarise(pop_yearto2050 = sum(pop, na.rm = T)) %>% 
    mutate(year = i)
  
  r10_poprem <- rbind(r10_poprem, pop_rem_loop)
  
}

# RCB quantities
rcb <- read_csv(here("Data", "processed", "rcbquantities.csv"))

# Regional RCBs from 1990 to 2020
r10_rcb19902020gtco2 <- read_csv(here("Data", "processed", "r10_rcb19902020.csv")) %>% 
  filter(year >= 1990, year <= 2020) 

# Assessed pathways
r10_ndclts_impren_emiss <- read_csv(here("Data", "processed", "r10_ndclts_impren_emiss.csv"))

# Heatwave EMFs and additional years
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
  mutate(rcbyear = rcb1990 - lag(gtco2_cmltv, default = 0)) %>% 
  arrange(model, case, aggregate, category, ppp_pf, r10, year) %>% 
  group_by(model, case, aggregate, category, ppp_pf, year) %>% 
  mutate(exceedanceyear = ifelse(sum(-rcbyear) > 0, sum(-rcbyear), 0),
         debtyear = ifelse(-rcbyear > 0, -rcbyear, 0)) %>% 
  group_by(r10, model, case, aggregate, category, ppp_pf) %>% 
  mutate(exceedancecmltv = cumsum(exceedanceyear),
         debtcmltv = cumsum(debtyear)) %>% 
  group_by(model, case, aggregate, category, ppp_pf, year) %>% 
  mutate(exceedanceshareyear = debtyear / sum(debtyear),
         exceedancesharecmltv = debtcmltv / sum(debtcmltv)) %>% 
  left_join(r10_popproj) %>% 
  left_join(r10_poprem)

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

# Combine extreme heatwave EMFs and carbon debt
r10_exp_heatwave_emf_temp_debt <- left_join(r10_exp_heatwave_emf,
                                               r10_ndclts_impren_debt,
                                               by = c("case", "aggregate", "r10"))

# FIGURE 2 ---------------------------------------------------------------------

a <- r10_ndclts_impren_rcbyear %>% 
  ungroup() %>% 
  filter(category == "1_PP1990", case != "IMP-REN") %>% 
  select(r10, case, aggregate, year, rcbyear) %>% 
  mutate(
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
    case = factor(case, levels = c("A", "E", "IMP-REN"),
                  labels = c("Current policies", 
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
    case = factor(case, levels = c("A", "E", "IMP-REN"),
                  labels = c("Current policies", 
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
    case = factor(case, levels = c("A", "E", "IMP-REN"),
                  labels = c("Current policies", 
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

wrap_plots(b,a,c, ncol = 3) +
  plot_annotation(tag_levels = list("a"), tag_prefix = "(", tag_suffix = ")", 
                  caption = "NAM: North America, EUR: Europe, APD: Asia-Pacific Developed, EEA: Eastern Europe and West-Central Asia, EASPAS: Eastern and South-East Asia and developing Pacific\nLAC: Latin America and Caribbean, AFRMEA: Africa and Middle East, SAS: Southern Asia")

ggsave(here("Manuscript", "Figures", "fig2.png"),
       height = 6, width = 12)

# SI 

r10_ndclts_impren_rcbyear %>% 
  ungroup() %>% 
  filter(case != "IMP-REN") %>% 
  mutate(
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
    case = factor(case, levels = c("A", "E", "IMP-REN"),
                  labels = c("Current policies",
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
  scale_colour_manual(values = c("#1f78b4", "#33a02c", "#a6cee3",  "#b2df8a")) +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#a6cee3", "#b2df8a")) +
  facet_grid(fct_rev(case) ~ r10) +
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_blank(), strip.placement = "inside", strip.text = element_text(hjust = 0),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13), panel.grid = element_blank()) +
  labs(x = NULL, y = NULL,
       colour = "Allocation approach",
       fill = "Allocation approach",
       caption = "NAM: North America, EUR: Europe, APD: Asia-Pacific Developed, EEA: Eastern Europe and West-Central Asia, EASPAS: Eastern and South-East Asia and developing Pacific\nLAC: Latin America and Caribbean, AFRMEA: Africa and Middle East, SAS: Southern Asia")

ggsave(here("Manuscript", "Figures", "SI", "SI_fig2.png"),
       height = 8, width = 14)

r10_ndclts_impren_rcbyear %>% 
  ungroup() %>% 
  filter(category == "1_PP1990", case != "IMP-REN", 
         aggregate == "Median") %>% 
  mutate(
    r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
    case = factor(case, levels = c("A", "E", "IMP-REN"),
                  labels = c("Current policies", 
                             "All pledges and net-zero targets", "IMP-REN")),
    exceedanceshareyear = exceedanceshareyear * exceedanceyear) %>% 
  select(r10, case, aggregate, year, exceedanceshareyear) %>% 
  group_by(case, year, aggregate) %>%
  mutate(exceedanceshareyear = exceedanceshareyear / sum(exceedanceshareyear)) %>% 
  pivot_wider(names_from = case, values_from = exceedanceshareyear) %>% 
  ggplot(aes(x = `Current policies`, y = `All pledges and net-zero targets`)) +
  geom_textabline(linetype = 2, label = "identity") +
  geom_point(aes(colour = year)) +
  scale_colour_continuous_sequential() +
  facet_wrap(~r10, ncol = 4) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.width = unit(2.5, "cm"),
        strip.background = element_blank(), strip.placement = "inside", strip.text = element_text(hjust = 0),
        strip.text.x = element_text(size = 12),
        axis.title = element_text(size = 13), panel.grid = element_blank()) +
  labs(colour = "Year",
       caption = "NAM: North America, EUR: Europe, APD: Asia-Pacific Developed, EEA: Eastern Europe and West-Central Asia, EASPAS: Eastern and South-East Asia and developing Pacific\nLAC: Latin America and Caribbean, AFRMEA: Africa and Middle East, SAS: Southern Asia")

ggsave(here("Manuscript", "Figures", "SI", "SI_fig2b.png"),
       height = 8, width = 14)

# FIGURE 3 ---------------------------------------------------------------------

a <- r10_exp_heatwave_emf_temp_debt %>%
  filter(birth_year %in% c(2020), case != "IMP-REN", aggregate == "Median", category == "1_PP1990") %>%
  mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
         case = factor(case, levels = c("A", "E"),
                       labels = c("Current policies",
                                  "All pledges and net-zero targets"))) %>% 
  ungroup() %>% 
  mutate(drawdown_resp_cap = drawdown_resp * 1e9 / pop_cmltv_rem_2050,
         debt_ratio = rcb2100 / rcb1990) %>% 
  ggplot(aes(y = -debt_ratio + 1)) +
  geom_hline(yintercept = 1, linetype = 2, linewidth = 0.3, colour = "darkgrey") +
  geom_vline(xintercept = 1, linetype = 2, linewidth = 0.3, colour = "darkgrey") +
  geom_errorbar(aes(xmin = emf_impren_0.33, xmax = emf_impren_0.66, colour = r10), alpha = 0.6) +
  geom_text(x = 8, y = 1.4, aes(label = ifelse(case == "Current policies", "Debt", "")), 
            alpha = 1, hjust = 1, colour = "darkgrey",
            data = . %>% distinct(case)) +
  geom_text(x = 8, y = 0.6, aes(label = ifelse(case == "Current policies", "Credit", "")), 
            alpha = 1, hjust = 1, colour = "darkgrey",
            data = . %>% distinct(case)) +
  geom_point(aes(x = emf_impren_0.5, colour = r10, size = drawdown_resp_cap),  alpha = 0.6) +
  geom_point(aes(x = emf_impren_0.5, colour = r10), size = 0.5,  alpha = 0.8) +
  geom_text(aes(x = emf_impren_0.5, label = r10, colour = r10), angle = 45, size = 2.5, hjust = -0.4) +
  scale_x_continuous(labels = scales::dollar_format(prefix = "", suffix = "x"),
                     breaks = seq(1,9,1)) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = "x"),
                     breaks = seq(1,10,1), limits = c(0.5,10), position = "left") +  
  scale_size_continuous(range = c(1,15)) +
  facet_wrap(~fct_rev(case), strip.position = "right", scales = "free_y", ncol = 2) +
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_blank(), strip.placement = "inside", strip.text = element_text(hjust = 0),
        strip.text.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13), panel.grid = element_blank()) +
  guides(colour = "none",
         size = guide_legend(nrow = 1)) +
  labs(y = "Regional budget consumption factor in 2100", 
       size = "Required regional average annual exceedance drawdown (tCO2/capita/yr, 2050-2100)",
       x = "Increase in 2020 birth cohort lifetime extreme heatwave exposure relative to illustrative 1.5C pathway (Factor)")

b <- r10_exp_heatwave_emf_temp_debt %>%
      filter(birth_year %in% c(1990, 2000, 2010, 2020), case != "IMP-REN", category == "1_PP1990", aggregate == "Median") %>%
  left_join(r10_ndclts_impren_rcbyear %>% 
              ungroup() %>% 
              filter(year %in% c(1990, 2000, 2010, 2020), case != "IMP-REN", category == "1_PP1990", aggregate == "Median") %>% 
              select(case, aggregate, r10, birth_year = year, rcbyear, pop_yearto2050)) %>% 
  mutate(r10 = factor(r10, levels = r10order$r10, labels = r10order$r10label),
         case = factor(case, levels = c("A", "E"),
                       labels = c("Current policies", 
                                  "All pledges and net-zero targets")),
         rcb_pc_2050 = rcbyear * 1e9 / pop_yearto2050) %>% 
  ungroup() %>% 
  ggplot(aes(x = add_impren_0.5, y = rcb_pc_2050)) +
  geom_texthline(yintercept = 2.63500292, linetype = 4, colour = "#8856a7", linewidth = .3, label = "ECPC1990", size = 2,
                 hjust = 0.9) +
  geom_hline(yintercept = 0, linetype = 2, colour = "darkgrey", linewidth = .3) +
  geom_vline(xintercept = 0, linetype = 2, colour = "darkgrey", linewidth = .3) +
  geom_errorbar(aes(xmin = add_impren_0.33, xmax = add_impren_0.66, colour = fct_rev(case)), alpha = 0.6) +
  geom_point(aes(shape = factor(birth_year), colour = fct_rev(case))) +
  facet_grid(~r10) +
  scale_colour_discrete_diverging() +
  scale_x_continuous(breaks = c(0, 5,10,15)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  labs(y = "Remaining budget (tCO2/capita/year, to 2050)",
       x = "Increase in lifetime years with extreme heatwave exposure relative to illustrative 1.5C pathway (Years)",
       shape = "Cohort birth year",
       colour = "Scenario") +
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_blank(), strip.placement = "inside", strip.text = element_text(hjust = 0),
        strip.text.x = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  guides(shape = guide_legend(nrow = 1, order = 1))

wrap_plots(b,a, ncol = 1, heights = c(0.7,1)) +  
  plot_annotation(tag_levels = list("a"), tag_prefix = "(", tag_suffix = ")", 
                  caption = "NAM: North America, EUR: Europe, APD: Asia-Pacific Developed, EEA: Eastern Europe and West-Central Asia, EASPAS: Eastern and South-East Asia and developing Pacific\nLAC: Latin America and Caribbean, AFRMEA: Africa and Middle East, SAS: Southern Asia")
  
ggsave(here("Manuscript", "Figures", "fig3.png"),
       height = 10, width = 10)
