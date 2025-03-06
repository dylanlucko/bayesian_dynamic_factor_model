#!/usr/bin/env Rscript
# analysis.R
#library(dplyr)
#install.packages("V8", type = "source")
#library(V8)
# -----------------------------
# Script 1: Data Extraction
# -----------------------------
# Set working directory (adjust the path if needed)
setwd("/export/home/dor/dlucko/Downloads")

# Unzip the file
unzip("Data_Panel_ZIP.zip", exdir = "Data_Panel_Extracted")

# List and find the CSV file
files <- list.files("Data_Panel_Extracted", 
                    pattern = "^panel_subset\\.csv$", 
                    recursive = TRUE, 
                    full.names = TRUE)

if (length(files) == 0) {
  stop("panel_subset.csv not found in the unzipped contents.")
}

# Read the CSV and assign it to panel_data
panel_subset <- read.csv(files[1])
cat("Preview of extracted data:\n")
print(head(panel_subset))
panel_data <- panel_subset

# -----------------------------
# Script 2: Data Preparation & Stan Modeling
# -----------------------------
# Load necessary libraries
library(dplyr)
library(tidyr)
library(rstan)
rstan_options(auto_write = TRUE)
# Use 10 cores as required (we set cores in stan() call as well)
options(mc.cores = 10)
pkgbuild::has_build_tools(debug = TRUE)

# Clean up memory before processing
gc()

# Prepare the data from panel_data
panel_subset <- panel_data %>%
  select(PERMNO, date, returns, rf, mktrf, smb, hml, rmw, cma, umd,
         effective_federal_funds_rate, inflation_expectation, pcepi) %>%
  mutate(date = as.Date(date)) %>%
  mutate(returns = as.numeric(gsub("[^0-9.-]", "", returns))) %>%
  mutate(excess_return = returns - rf)

# Check for duplicates
duplicates <- panel_subset %>%
  group_by(PERMNO, date) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)
if(nrow(duplicates) > 0){
  message("Duplicates found; aggregating by taking the mean excess_return.")
}

# Create a wide-format returns matrix: rows = stocks, columns = dates.
returns_wide <- panel_subset %>%
  select(PERMNO, date, excess_return) %>%
  pivot_wider(names_from = date, values_from = excess_return, values_fn = mean)

# Create the factor returns and macro variables data frames
factor_macro <- panel_subset %>%
  group_by(date) %>%
  summarise(mktrf = first(mktrf),
            smb   = first(smb),
            hml   = first(hml),
            rmw   = first(rmw),
            cma   = first(cma),
            umd   = first(umd),
            eff_fed = first(effective_federal_funds_rate),
            infl_exp = first(inflation_expectation),
            pcepi = first(pcepi),
            .groups = "drop") %>%
  arrange(date)

# Remove dates with missing factor or macro data.
valid_dates <- factor_macro %>% filter(complete.cases(.)) %>% pull(date)
factor_macro <- factor_macro %>% filter(date %in% valid_dates)
returns_wide <- returns_wide %>% select(PERMNO, all_of(as.character(valid_dates)))

# Convert returns_wide to a matrix
returns_matrix <- as.matrix(returns_wide[,-1])
rownames(returns_matrix) <- returns_wide$PERMNO

# Create matrices for Stan
factor_returns <- as.matrix(factor_macro %>% select(mktrf, smb, hml, rmw, cma, umd))
macro_vars     <- as.matrix(factor_macro %>% select(eff_fed, infl_exp, pcepi))

# Define dimensions for Stan
N <- nrow(returns_matrix)       # number of stocks
T_val <- nrow(factor_returns)   # number of time periods
K <- ncol(factor_returns)       # number of factors (6)
M <- ncol(macro_vars)           # number of macro variables (3)

# Stan model code (as a single string)
stan_code <- "
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
"

# Check and clean the returns matrix for NAs
na_count <- sum(is.na(returns_matrix))
message("Number of NAs in returns_matrix: ", na_count)
returns_matrix_clean <- returns_matrix[complete.cases(returns_matrix), ]
message("Number of stocks before cleaning: ", nrow(returns_matrix))
message("Number of stocks after cleaning: ", nrow(returns_matrix_clean))
N_clean <- nrow(returns_matrix_clean)

# Fit the Stan model using 10 cores
fit <- stan(model_code = stan_code,
            data = list(N = N_clean,
                        T = T_val,
                        K = K,
                        M = M,
                        F = factor_returns,
                        X = macro_vars,
                        y = returns_matrix_clean),
            iter = 1000,
            chains = 4,
            cores = 10,
            verbose = TRUE,
            refresh = 1,
            seed = 123)

# Print and save the fitted model
print(fit)
saveRDS(fit, file = "/export/home/dor/dlucko/Downloads/stan_fit_output.rds")
