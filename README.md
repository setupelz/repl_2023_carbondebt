# Using net-zero carbon debt to track climate overshoot responsibility

Replication archive for:

> Pelz, S., Ganti, G., Lamboll, R., Grant, L., Smith, C., Pachauri, S., Rogelj, J.,
> Riahi, K., Thiery, W. and Gidden, M.J. (2025). Using net-zero carbon debt to
> track climate overshoot responsibility. *Proceedings of the National Academy of
> Sciences* 122(13), e2409316122. https://doi.org/10.1073/pnas.2409316122

Archive releases: https://github.com/setupelz/repl_2023_carbondebt/releases
(the large inputs and the pipeline outputs are attached there, see Data).
Interactive explorer: https://setupelz.com/carbondebt/

## What is here

| Folder | Content |
| --- | --- |
| `Code/` | The analysis pipeline, R scripts and two Jupyter notebooks, numbered in run order (below). |
| `Code/explorer/` | Build script for the interactive explorer (Python, `fair-shares`). |
| `Data/` | Inputs by source (`countrygroups/`, `equity_data/`, `fairparams/`, `impacts/`, `pathways/`), explorer inputs (`explorer/`), and `MANIFEST.md5` for the release-attached files. Pipeline outputs go to `Data/processed/`. |
| `Manuscript/Figures/` | Main and SI figures as written by the scripts. |
| `site/` | The net-zero carbon debt explorer, a static page. |

## Pipeline

Run in numeric order from the project root (`replication_nzcdebt.Rproj`).

| Script | Does |
| --- | --- |
| `00_prep_ar6_filter.R` | Filter and pre-process the AR6 scenario database. |
| `00_tempassessment.ipynb`, `00_tempassessment_impren.ipynb` | FaIR temperature assessment of the AR6 and NDC/LTS pathways (Python, `environment.yml`). |
| `01_remainingcarbonbudget.R`, `01a_remainingcarbonbudget_SI.R` | Remaining carbon budgets for countries and regions; SI variants of the warming-contribution allocations. |
| `02_carbondebt_systematic.R` | Net-zero carbon debt across the AR6 database. |
| `03_nztargetassessment.R` | Net-zero targets for the analysis countries. |
| `04_ndcltspathwayprocessing.R` | Pre-processing of the assessed NDC and LTS pathways. |
| `05_heatwaveexposure.R` | Lifetime exposure to extreme heatwaves under the assessed pathways. |
| `06_ndcltsassessment.R` | Evaluation of the assessed pathways. |

R packages are loaded with `pacman` at the top of each script. The FaIR
notebooks use the conda environment in `environment.yml` (`fair==2.2.0`).

## Data

Everything under `Data/` is in the repository except the files listed in
`Data/MANIFEST.md5`, which are attached to the release instead because of
their size: the five largest inputs (SSP database, GMST response fields,
historical emissions, heatwave and cohort netCDFs) and the fourteen pipeline
outputs in `Data/processed/`. Download them from the release page into the
folders named in the manifest and verify:

```bash
md5sum -c Data/MANIFEST.md5
```

With the outputs in place the figure scripts run without rerunning the FaIR
notebooks. Sources and licences of the third-party inputs: `Data/README.md`.

## Explorer

`site/` is the net-zero carbon debt explorer (originally a 2024 Shiny app for
stakeholder engagement in the ELEVATE project, rebuilt as a static page). It
applies the carbon-debt accounting of the paper, debt = cumulative emissions
minus fair-share allocation, across three temperature goals, six accounting
start years, two future-emissions scenarios and two capability adjustments for
the IAMC R10 regions. Remaining carbon budgets come from the `fair-shares`
library (Lamboll et al. 2023). Inputs are the three CSVs in `Data/explorer/`;
`Code/explorer/build.py` writes `site/assets/data.json`:

```bash
cd Code/explorer && uv run python build.py
cd ../../site && python3 -m http.server     # local preview
```

## Citation

See `CITATION.cff`. Cite the paper for the method and results, and the
archive release for the code and data.

## Licence

MIT for the code and figures in this archive (`LICENSE`). Third-party inputs
keep their own licences, listed in `Data/README.md`.
