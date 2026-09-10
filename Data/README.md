# Data

Inputs by source, the explorer inputs, and the pipeline outputs. Files listed
in `MANIFEST.md5` are attached to the GitHub release rather than tracked in
git (five inputs above 20 MB and the fourteen `processed/` outputs); download
them into the folders named there and check with `md5sum -c MANIFEST.md5`.

| Folder | Content | Source |
| --- | --- | --- |
| `countrygroups/` | ISO3 to region mapping (IAMC R10 and analysis groups). | Own compilation. |
| `equity_data/` | Population (`population.csv`), GDP at market and PPP rates (World Bank WDI, `API_NY.GDP.MKTP.*`), national fossil and land-use CO2 (Global Carbon Budget 2023, `National_*_Carbon_Emissions_2023v1.0.xlsx`), SSP country data (`SspDb_country_data_2013-06-12.csv`, release asset), and the GMST response fields (`GMST_response_*.csv`, 1851 file a release asset). | World Bank, Global Carbon Project, IIASA SSP database, own FaIR runs. |
| `fairparams/` | FaIR v2.2 calibrated constrained parameter sets (calibration 1.4.1), species configurations, solar and volcanic forcing, and the 1750-2022 scaled emissions. | FaIR calibration data (Smith et al.), as used by the `00_tempassessment*` notebooks. |
| `impacts/` | Lifetime heatwave exposure and cohort-size netCDFs (both release assets). | Grant, Thiery et al., lifetime exposure framework. |
| `pathways/` | AR6 scenario database extracts (`ar6_all/`, `ar6_impren/`), NDC and LTS pathway inputs and net-zero target data (`nz_targets/`, historical annual emissions 1830-2022 a release asset), and the assessed scenario set (`scenarios/`). | IPCC AR6 WGIII scenario database (IIASA), Net Zero Tracker, own processing. |
| `explorer/` | The three inputs of the net-zero carbon debt explorer (R10 population, GDP and CO2 panel; R10 historical fossil CO2; REMIND-MAgPIE R10 pathways for current policies and pledges) and `rcbs.yaml`, a pinned copy of the remaining carbon budgets in the `fair-shares` library (Lamboll et al. 2023). | Explorer build, `../Code/explorer/build.py`. |
| `processed/` | Outputs of the numbered scripts in `../Code/` (all release assets), plus `README.Rmd`. | This archive. |

Third-party inputs keep the licence of their source. The MIT licence of this
archive covers the code, the outputs and the figures.
