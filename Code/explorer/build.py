"""Generate site/assets/data.json for the net-zero carbon debt explorer.

Iterates over all input combinations (temperature goal x start year x scenario
x capability) and calls fair-shares to allocate remaining carbon budgets across
the IAMC R10 regions. Inputs are the three CSVs in Data/explorer/. Run from
this folder with `uv run python build.py` (uv resolves fair-shares from GitHub).
"""

from __future__ import annotations

import json
import gzip
from pathlib import Path

import pandas as pd
import yaml

from fair_shares.library.allocations.budgets.per_capita import (
    equal_per_capita_budget,
    per_capita_adjusted_budget,
)

# ---------------------------------------------------------------------------
# Paths

ROOT = Path(__file__).resolve().parents[2]
DATA_RAW = ROOT / "Data" / "explorer"
SITE_OUT = ROOT / "site" / "assets"
SITE_OUT.mkdir(parents=True, exist_ok=True)

# ---------------------------------------------------------------------------
# Inputs

# Remaining carbon budgets are loaded from the fair-shares package so the
# citation stays aligned with the library. Lamboll et al. (2023) is the
# canonical source used by the 2024 Shiny version.
def _load_rcbs() -> tuple[list[dict], dict]:
    """Return (temperature_goals, source_meta).

    Values come from Data/explorer/rcbs.yaml, a pinned copy of the
    fair-shares library's data/rcbs/rcbs.yaml (Lamboll et al. 2023).
    """
    rcbs_path = DATA_RAW / "rcbs.yaml"
    data = yaml.safe_load(rcbs_path.read_text())["rcb_data"]["lamboll_2023"]

    # Lamboll reports 2°C at 90% as an aux value in the header comment —
    # we pin it explicitly here because the yaml tracks 66% / 83% only.
    LAMBOLL_2_90 = 500

    return (
        [
            {
                "key": "1p5_50",
                "label": "1.5 °C (50% probability)",
                "rcb_gtco2": int(data["scenarios"]["1.5p50"]),
            },
            {
                "key": "2_66",
                "label": "2 °C (66% probability)",
                "rcb_gtco2": int(data["scenarios"]["2p66"]),
            },
            {
                "key": "2_90",
                "label": "2 °C (90% probability)",
                "rcb_gtco2": LAMBOLL_2_90,
            },
        ],
        {
            "citation": "Lamboll et al. (2023), Nature Climate Change",
            "doi": "10.1038/s41558-023-01848-5",
            "url": "https://doi.org/10.1038/s41558-023-01848-5",
            "baseline_year": int(data["baseline_year"]),
            "unit": "GtCO₂",
            "note": "Remaining carbon budget from the 2023 baseline onwards.",
        },
    )


TEMPERATURE_GOALS, RCB_SOURCE = _load_rcbs()

START_YEARS = [1900, 1950, 1990, 2015, 2020, 2023]

SCENARIOS = [
    {"key": "A", "label": "Current policies"},
    {"key": "E", "label": "NDCs and pledges"},
]

CAPABILITIES = [
    {"key": "none", "label": "None (equal per capita)"},
    {"key": "gdp_ppp", "label": "1 / GDP (PPP) — capability weighting"},
]

LAST_PATHWAY_YEAR = 2050
EMISSION_CATEGORY = "co2-ffi"
HIST_END_YEAR = 2022  # last year of historical emissions before scenarios kick in

# ---------------------------------------------------------------------------
# Data loading


def load_raw():
    """Load the three input datasets from Data/explorer/."""
    indicators = pd.read_csv(DATA_RAW / "allindicators.csv")
    indicators["start_year"] = indicators["start_year"].astype(int)
    indicators["end_year"] = indicators["end_year"].astype(int)

    hist = pd.read_csv(DATA_RAW / "prodco2ffi.csv")
    hist = hist.rename(columns={"CO2": "gtco2_hist"})

    paths = pd.read_csv(DATA_RAW / "r10_ndclts_impren_emiss.csv")

    return indicators, hist, paths


def annual_timeseries(
    indicators: pd.DataFrame,
    column: str,
    extend_to: tuple[int, int] | None = None,
) -> pd.DataFrame:
    """Extract annual region×year time series from the panel (where start_year == end_year).

    Returns a DataFrame indexed by (iso3c, unit) with year-string columns, as expected
    by fair-shares. `iso3c` here carries IAMC R10 codes.

    If ``extend_to=(lo, hi)`` is given, the column range is padded via
    forward-fill (backwards in time) and back-fill (forwards in time) so that
    fair-shares' year-coverage validation passes when the underlying data has
    a narrower observation window than the allocation window.
    """
    diag = indicators[indicators["start_year"] == indicators["end_year"]].copy()
    diag["year"] = diag["end_year"]
    wide = diag.pivot(index="iamc_r10", columns="year", values=column)
    wide.columns = [int(y) for y in wide.columns]
    wide = wide.dropna(axis=1, how="all")

    if extend_to is not None:
        lo, hi = extend_to
        full = range(lo, hi + 1)
        wide = wide.reindex(columns=full)
        wide = wide.ffill(axis=1).bfill(axis=1)

    wide.columns = [str(y) for y in wide.columns]
    wide.index.name = "iso3c"
    wide["unit"] = "dimensionless"
    wide = wide.set_index("unit", append=True)
    return wide


def fill_pathway(paths: pd.DataFrame, hist: pd.DataFrame) -> pd.DataFrame:
    """Pathway rows from r10_ndclts_impren_emiss.csv cover 1990–2050 for cases A and E,
    aggregates Max/Median/Min. Pre-1990 years are filled from historical."""
    paths = paths[(paths["year"] <= LAST_PATHWAY_YEAR) & paths["case"].isin(["A", "E"])]
    paths = paths.rename(columns={"r10": "iamc_r10"})[
        ["case", "aggregate", "iamc_r10", "year", "gtco2"]
    ]
    # Extend back to 1850 so the chart can show full historical series
    regions = paths["iamc_r10"].unique()
    cases = paths["case"].unique()
    aggregates = paths["aggregate"].unique()
    full_idx = pd.MultiIndex.from_product(
        [cases, aggregates, regions, range(1850, LAST_PATHWAY_YEAR + 1)],
        names=["case", "aggregate", "iamc_r10", "year"],
    )
    paths = paths.set_index(["case", "aggregate", "iamc_r10", "year"]).reindex(full_idx)
    paths = paths.reset_index()
    paths = paths.merge(hist, on=["iamc_r10", "year"], how="left")
    paths["gtco2"] = paths["gtco2"].fillna(paths["gtco2_hist"])
    return paths[["case", "aggregate", "iamc_r10", "year", "gtco2"]]


# ---------------------------------------------------------------------------
# Allocation


def allocate_shares(
    population_ts: pd.DataFrame,
    gdp_ts: pd.DataFrame,
    allocation_year: int,
    capability: str,
) -> pd.Series:
    """Run fair-shares allocation for this (start_year, capability) combo. Returns
    a Series indexed by R10 region giving relative shares that sum to 1."""
    if capability == "none":
        result = equal_per_capita_budget(
            population_ts=population_ts,
            allocation_year=allocation_year,
            emission_category=EMISSION_CATEGORY,
        )
    else:
        # Reference GDP year matches the old R logic: clamp start_year to [1990, 2019]
        ref_year = min(max(allocation_year, 1990), 2019)
        result = per_capita_adjusted_budget(
            population_ts=population_ts,
            allocation_year=allocation_year,
            emission_category=EMISSION_CATEGORY,
            gdp_ts=gdp_ts,
            capability_weight=1.0,
            capability_per_capita=False,
            capability_functional_form="power",
            capability_exponent=1.0,
            capability_reference_year=ref_year,
        )
    shares = result.relative_shares_cumulative_emission[str(allocation_year)]
    shares = shares.droplevel(["unit", "emission-category"])
    return shares


# ---------------------------------------------------------------------------
# Per-combination computation


def compute_combo(
    *,
    temperature_goal: dict,
    start_year: int,
    scenario_key: str,
    capability: str,
    population_ts: pd.DataFrame,
    gdp_ts: pd.DataFrame,
    hist: pd.DataFrame,
    paths: pd.DataFrame,
    pop_annual: pd.DataFrame,  # (region, year) -> annual pop (people)
):
    rcb = temperature_goal["rcb_gtco2"]

    # Historical emissions window: start_year → HIST_END_YEAR (or zero if start_year == 2023)
    if start_year >= 2023:
        hist_cum = hist.iloc[0:0]
        hist_window = {r: 0.0 for r in population_ts.index.get_level_values(0).unique()}
    else:
        hist_window_df = hist[
            (hist["year"] >= start_year) & (hist["year"] <= HIST_END_YEAR)
        ]
        hist_window = (
            hist_window_df.groupby("iamc_r10")["gtco2_hist"].sum().to_dict()
        )

    total_historical = sum(hist_window.values())
    total_pool = rcb + total_historical  # GtCO2

    # Run fair-shares allocation
    shares = allocate_shares(
        population_ts=population_ts,
        gdp_ts=gdp_ts,
        allocation_year=start_year,
        capability=capability,
    )

    # Absolute allocations
    allocation_start_year = shares * total_pool  # GtCO2, from start_year onwards
    # allocation remaining from 2023 onwards = allocation_start_year - hist_window
    allocation_2023 = {
        region: allocation_start_year[region] - hist_window.get(region, 0.0)
        for region in shares.index
    }

    # Scenario cumulative emissions 2023..2050 for each aggregate (Max / Median / Min)
    scenario_paths = paths[
        (paths["case"] == scenario_key)
        & (paths["year"] >= 2023)
        & (paths["year"] <= LAST_PATHWAY_YEAR)
    ]
    cumulative_2023_2050 = (
        scenario_paths.groupby(["iamc_r10", "aggregate"])["gtco2"].sum().unstack()
    )

    regions = list(shares.index)
    carbon_debt = {}  # positive = debt (overshoot), negative = credit
    per_capita_ratio = {}
    for region in regions:
        region_debt = {}
        region_ratio = {}
        for agg in ("Max", "Median", "Min"):
            emitted = float(cumulative_2023_2050.loc[region, agg])
            alloc = float(allocation_2023[region])
            region_debt[agg] = round(emitted - alloc, 2)
            # ratio: (historical + projected) / allocation_start_year
            total_cumulative = hist_window.get(region, 0.0) + emitted
            ratio_val = (
                total_cumulative / float(allocation_start_year[region])
                if float(allocation_start_year[region]) > 0
                else None
            )
            region_ratio[agg] = (
                round(ratio_val, 3) if ratio_val is not None else None
            )
        carbon_debt[region] = region_debt
        per_capita_ratio[region] = region_ratio

    # Per-capita emissions paths from 1850 through LAST_PATHWAY_YEAR
    paths_scenario = paths[paths["case"] == scenario_key]
    pc_emissions = {}
    for region in regions:
        region_paths = paths_scenario[paths_scenario["iamc_r10"] == region].sort_values(
            "year"
        )
        region_pop = pop_annual.loc[region] if region in pop_annual.index else None
        historical = []
        projected = []
        for _, row in region_paths.iterrows():
            year = int(row["year"])
            agg = row["aggregate"]
            if agg != "Median":
                continue
            pop = None
            if region_pop is not None and str(year) in region_pop.index:
                pop = float(region_pop[str(year)])
            if pop and year <= HIST_END_YEAR:
                historical.append(
                    [year, round(row["gtco2"] * 1e9 / pop, 3)]
                )
        # Projected Max/Min per year (for range shading) and Median
        projected_by_year = {}
        for _, row in region_paths[region_paths["year"] >= 2023].iterrows():
            year = int(row["year"])
            agg = row["aggregate"]
            if region_pop is None or str(year) not in region_pop.index:
                continue
            pop = float(region_pop[str(year)])
            if pop <= 0:
                continue
            per_cap = round(row["gtco2"] * 1e9 / pop, 3)
            projected_by_year.setdefault(year, {})[agg] = per_cap
        projected_series = []
        for year in sorted(projected_by_year):
            pb = projected_by_year[year]
            if "Median" in pb and "Max" in pb and "Min" in pb:
                projected_series.append([year, pb["Min"], pb["Median"], pb["Max"]])
        pc_emissions[region] = {"historical": historical, "projected": projected_series}

    return {
        "rcb_gtco2": rcb,
        "total_budget_pool_gtco2": round(total_pool, 2),
        "allocation_start_year": {
            r: round(float(allocation_start_year[r]), 2) for r in regions
        },
        "allocation_2023": {r: round(allocation_2023[r], 2) for r in regions},
        "historical_emissions_window": {
            r: round(float(hist_window.get(r, 0.0)), 2) for r in regions
        },
        "carbon_debt": carbon_debt,
        "per_capita_ratio": per_capita_ratio,
        "per_capita_emissions": pc_emissions,
    }


# ---------------------------------------------------------------------------
# Main


def main():
    print("Loading raw data...")
    indicators, hist, paths_raw = load_raw()
    paths = fill_pathway(paths_raw, hist)

    population_ts = annual_timeseries(indicators, "pop", extend_to=(1900, LAST_PATHWAY_YEAR))
    gdp_ts = annual_timeseries(indicators, "gdp2017ppp", extend_to=(1900, LAST_PATHWAY_YEAR))

    # Extract annual pop table for per-capita emissions rendering (region, year)
    pop_annual = (
        indicators[indicators["start_year"] == indicators["end_year"]]
        .pivot(index="iamc_r10", columns="end_year", values="pop")
    )
    pop_annual.columns = [str(int(y)) for y in pop_annual.columns]

    results = {}

    combo_count = 0
    total_combos = (
        len(TEMPERATURE_GOALS) * len(START_YEARS) * len(SCENARIOS) * len(CAPABILITIES)
    )

    for tg in TEMPERATURE_GOALS:
        for start_year in START_YEARS:
            for scenario in SCENARIOS:
                for capability in CAPABILITIES:
                    combo_count += 1
                    key = f"{tg['key']}|{start_year}|{scenario['key']}|{capability['key']}"
                    if combo_count % 20 == 0 or combo_count == total_combos:
                        print(f"  [{combo_count}/{total_combos}] {key}")
                    results[key] = compute_combo(
                        temperature_goal=tg,
                        start_year=start_year,
                        scenario_key=scenario["key"],
                        capability=capability["key"],
                        population_ts=population_ts,
                        gdp_ts=gdp_ts,
                        hist=hist,
                        paths=paths,
                        pop_annual=pop_annual,
                    )

    payload = {
        "version": 1,
        "generated_at": pd.Timestamp.utcnow().isoformat(),
        "source": "fair-shares",
        "rcb_source": RCB_SOURCE,
        "inputs": {
            "temperature_goals": TEMPERATURE_GOALS,
            "start_years": START_YEARS,
            "scenarios": SCENARIOS,
            "capabilities": CAPABILITIES,
        },
        "regions": sorted(population_ts.index.get_level_values(0).unique().tolist()),
        "combinations": results,
    }

    out_path = SITE_OUT / "data.json"
    out_path.write_text(json.dumps(payload, separators=(",", ":")))
    print(f"Wrote {out_path} ({out_path.stat().st_size / 1024:.1f} KB)")

    gz_path = SITE_OUT / "data.json.gz"
    with gzip.open(gz_path, "wt") as f:
        json.dump(payload, f, separators=(",", ":"))
    print(f"Wrote {gz_path} ({gz_path.stat().st_size / 1024:.1f} KB gzipped)")


if __name__ == "__main__":
    main()
