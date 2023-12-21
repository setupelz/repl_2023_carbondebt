# Import Libraries
import os
import pandas as pd
import pyam
import matplotlib.pyplot as plt
import aneris.convenience
import silicone.time_projectors as timeproj
import silicone.multiple_infillers as mi
import silicone.database_crunchers as cr

# LOAD DATA --------------------------------------------------------------------

# Read and pivot the processed CO2-FFI emissions paths
allpaths_global = pd.read_csv("Data/processed/allpaths_global_tidy.csv")
allpaths_global = allpaths_global.pivot_table(index=['Model', 'Scenario', 'Region', 'Variable', 'Unit'], columns='Year',  values='Value').reset_index()

# Read and filter historical AR6 CO2-FFI emissions
histar6_co2ffi = pd.read_csv("Data/pathways/rcmip-emissions-annual-means-v5-1-0.csv")
histar6_co2ffi = histar6_co2ffi.query("Region == 'World' & Variable == 'Emissions|CO2|MAGICC Fossil and Industrial' & Mip_Era == 'CMIP6' & Model == 'MESSAGE-GLOBIOM'")
histar6_co2ffi = histar6_co2ffi[['Model', 'Scenario', 'Region', 'Variable', 'Unit', '2005', '2010', '2015', '2020']]
histar6_co2ffi['Variable'] = 'Emissions|CO2|Energy and Industrial Processes'

# Read infilling database from AR6
infillar6 = pd.read_csv("Data/pathways/1652361598937-ar6_emissions_vetted_infillerdatabase_10.5281-zenodo.6390768.csv")

# HARMONISATION AND INFILLING --------------------------------------------------
def convert_to_iamc_style(inp, idx=("Model", "Scenario", "Region", "Variable", "Unit")):
    out = inp.copy()
    
    # Convert only the specified columns in idx to lowercase
    for col in idx:
        if col in out.columns:
            out.rename(columns={col: col.lower()}, inplace=True)
    
    # Convert idx to lowercase to match the new column names
    idx_lower = [col.lower() for col in idx if col in out.columns]
    
    # Set the index
    out = out.set_index(idx_lower)
    
    # Convert column names to integers, if they are not already
    if not all(isinstance(col, int) for col in out.columns):
        out.columns = out.columns.map(int)
        
    return out

# Convert DataFrame
allpaths_global_iamc = convert_to_iamc_style(allpaths_global)


# Convert DataFrame
allpaths_global_iamc = convert_to_iamc_style(allpaths_global)


# Convert DataFrame
allpaths_global_iamc = convert_to_iamc_style(allpaths_global)
histar6_co2ffi_iamc = convert_to_iamc_style(histar6_co2ffi)

# Set overrides
overrides = pd.DataFrame([
    {'variable': 'Emissions|CO2|Energy and Industrial Processes', 'method': 'reduce_offset_2030'},
])

# Harmonize to historical AR6 data
allpaths_global_iamc_harmonised_py = aneris.convenience.harmonise_all(
    scenarios = allpaths_global_iamc,
    history = histar6_co2ffi_iamc,
    year = 2020
)
