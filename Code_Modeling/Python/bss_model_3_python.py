#!/usr/bin/env python3
"""
analysis.py

This script replicates the functionality of your R code in Python.
It performs the following tasks:
  1. Data Extraction:
     - Changes the working directory.
     - Unzips a given ZIP file.
     - Searches for a CSV file matching the pattern and reads it.
  2. Data Preparation & Stan Modeling:
     - Loads and cleans the data using pandas.
     - Checks for duplicates and aggregates returns if needed.
     - Pivots the data into wide format.
     - Prepares matrices for factor returns and macro variables.
     - Defines dimensions for Stan.
     - Provides the Stan model code as a string.
     - Cleans the returns matrix (removing rows with any NAs).
     - Compiles and fits the Stan model using CmdStanPy.
     - Prints a summary and saves the fitted model.
"""

import os
import zipfile
import glob
import re
import gc
import pickle

import pandas as pd
import numpy as np

# Import CmdStanModel from cmdstanpy. Ensure that CmdStan is installed and configured.
from cmdstanpy import CmdStanModel

# -----------------------------
# Script 1: Data Extraction
# -----------------------------

# Set working directory (adjust the path if needed)
working_dir = "/export/home/dor/dlucko/Downloads"
os.chdir(working_dir)

# Unzip the file into a directory "Data_Panel_Extracted"
zip_filename = "Data_Panel_ZIP.zip"
extract_dir = "Data_Panel_Extracted"
with zipfile.ZipFile(zip_filename, 'r') as zip_ref:
    zip_ref.extractall(extract_dir)

# List and find the CSV file matching pattern "^panel_subset\.csv$"
# Use glob to recursively search for the file
pattern = os.path.join(extract_dir, '**', 'panel_subset.csv')
files = glob.glob(pattern, recursive=True)

if len(files) == 0:
    raise FileNotFoundError("panel_subset.csv not found in the unzipped contents.")

# Read the CSV file into a DataFrame
panel_subset = pd.read_csv(files[0])
print("Preview of extracted data:")
print(panel_subset.head())

# For clarity, assign panel_data to panel_subset (as in the R script)
panel_data = panel_subset.copy()

# -----------------------------
# Script 2: Data Preparation & Stan Modeling
# -----------------------------

# Import additional libraries for data manipulation
# (pandas and numpy were already imported)
# Clean up memory before processing
gc.collect()

# --- Data Preparation ---

# Select the relevant columns
cols_to_keep = [
    'PERMNO', 'date', 'returns', 'rf', 'mktrf', 'smb', 'hml', 'rmw', 
    'cma', 'umd', 'effective_federal_funds_rate', 'inflation_expectation', 'pcepi'
]
panel_subset = panel_data[cols_to_keep].copy()

# Convert the date column to datetime format
panel_subset['date'] = pd.to_datetime(panel_subset['date'])

# Clean the "returns" column:
# Remove any character that is not a digit, decimal point, or minus sign and convert to float.
panel_subset['returns'] = panel_subset['returns'].astype(str).str.replace(r'[^0-9\.-]', '', regex=True).astype(float)

# Create a new column "excess_return" as returns minus rf.
panel_subset['excess_return'] = panel_subset['returns'] - panel_subset['rf']

# Check for duplicates based on PERMNO and date
duplicates = (panel_subset.groupby(["PERMNO", "date"])
                         .size()
                         .reset_index(name="n")
                         .query("n > 1"))
if not duplicates.empty:
    print("Duplicates found; aggregating by taking the mean excess_return.")

# Create a wide-format returns matrix with rows = stocks and columns = dates.
# Aggregate duplicates by taking the mean of excess_return.
returns_wide = panel_subset.pivot_table(index='PERMNO', 
                                          columns='date', 
                                          values='excess_return', 
                                          aggfunc='mean')

# --- Factor Returns and Macro Variables DataFrames ---

# Group by date and take the first available value for each required column.
factor_macro = (panel_subset.groupby('date')
                              .agg({
                                  'mktrf': 'first',
                                  'smb': 'first',
                                  'hml': 'first',
                                  'rmw': 'first',
                                  'cma': 'first',
                                  'umd': 'first',
                                  'effective_federal_funds_rate': 'first',
                                  'inflation_expectation': 'first',
                                  'pcepi': 'first'
                              })
                              .reset_index()
                              .sort_values('date'))

# Rename columns for consistency with Stan data names
factor_macro.rename(columns={'effective_federal_funds_rate': 'eff_fed',
                             'inflation_expectation': 'infl_exp'}, inplace=True)

# Remove dates with missing factor or macro data.
factor_macro = factor_macro.dropna()
valid_dates = factor_macro['date'].tolist()

# Ensure that returns_wide only has columns corresponding to valid dates.
# Since the columns of returns_wide are timestamps, filter them accordingly.
# Create a list of valid date strings that match the column names (if necessary)
returns_wide = returns_wide.loc[:, returns_wide.columns.isin(valid_dates)]

# Convert the returns_wide DataFrame to a NumPy matrix.
# The index (PERMNO) serves as the row identifier.
returns_matrix = returns_wide.to_numpy()
# (The stock identifiers are available as returns_wide.index if needed.)

# Create matrices for Stan:
# Factor returns: select the columns mktrf, smb, hml, rmw, cma, umd.
factor_returns = factor_macro[['mktrf', 'smb', 'hml', 'rmw', 'cma', 'umd']].to_numpy()

# Macro variables: select the columns eff_fed, infl_exp, pcepi.
macro_vars = factor_macro[['eff_fed', 'infl_exp', 'pcepi']].to_numpy()

# Define dimensions for Stan:
N = returns_matrix.shape[0]       # number of stocks
T_val = factor_returns.shape[0]   # number of time periods
K = factor_returns.shape[1]       # number of factors (6)
M = macro_vars.shape[1]           # number of macro variables (3)

# --- Stan Model Setup ---

# Stan model code as a multi-line string.
stan_code = r"""
data {
  int<lower=1> N;
  int<lower=1> T;
  int<lower=1> K;
  int<lower=1> M;
  matrix[T, K] F;
  matrix[T, M] X;
  matrix[N, T] y;
}
parameters {
  matrix[N, K] beta0;
  matrix[K, M] Gamma;
  vector<lower=0>[K] sigma_beta;
  real<lower=0> sigma_y;
  matrix[T, K] beta[N];
  vector[K] mu_beta;
  vector<lower=0>[K] sigma_mu;
}
model {
  mu_beta ~ normal(0, 1);
  sigma_mu ~ cauchy(0, 2.5);
  for (i in 1:N) {
    beta0[i] ~ normal(mu_beta, sigma_mu);
    beta[i][1] ~ multi_normal(beta0[i]', diag_matrix(square(sigma_beta)));
  }
  to_vector(Gamma) ~ normal(0, 1);
  sigma_beta ~ cauchy(0, 2.5);
  sigma_y ~ cauchy(0, 2.5);
  for (i in 1:N) {
    for (t in 2:T) {
      beta[i][t] ~ multi_normal(beta[i][t-1] + (Gamma * X[t]')', diag_matrix(square(sigma_beta)));
    }
  }
  for (i in 1:N) {
    for (t in 1:T) {
      y[i, t] ~ normal(dot_product(beta[i][t], F[t,]), sigma_y);
    }
  }
}
"""

# --- Clean the Returns Matrix ---

# Check and report the number of missing values (NAs) in the returns matrix.
na_count = np.isnan(returns_matrix).sum()
print("Number of NAs in returns_matrix:", na_count)

# Remove any stock (row) with at least one NA value.
returns_matrix_clean = returns_matrix[~np.isnan(returns_matrix).any(axis=1)]
print("Number of stocks before cleaning:", returns_matrix.shape[0])
print("Number of stocks after cleaning:", returns_matrix_clean.shape[0])
N_clean = returns_matrix_clean.shape[0]

# --- Fit the Stan Model ---

# Prepare data dictionary for Stan
stan_data = {
    "N": N_clean,
    "T": T_val,
    "K": K,
    "M": M,
    "F": factor_returns,
    "X": macro_vars,
    "y": returns_matrix_clean
}

# Compile the Stan model from the provided code.
model = CmdStanModel(model_code=stan_code)

# Fit the model using 4 chains and 10 parallel chains (cores).
# Here we set 500 warmup iterations and 500 sampling iterations per chain (total 1000 iterations per chain).
fit = model.sample(data=stan_data,
                   chains=4,
                   parallel_chains=10,
                   iter_warmup=500,
                   iter_sampling=500,
                   seed=123)

# Print a summary of the fitted model
print(fit.summary())

# Save the fitted model object using pickle
output_file = os.path.join(working_dir, "stan_fit_output.pkl")
with open(output_file, "wb") as f_out:
    pickle.dump(fit, f_out)

print(f"Fitted model saved to {output_file}")
