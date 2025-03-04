# Load necessary libraries
library(dplyr)
library(tidyr)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
pkgbuild::has_build_tools(debug = TRUE)

# -----------------------------
# 1. Prepare the Data
# -----------------------------
# Assume your original long-format data frame is named panel_data and has the columns:
# "PERMNO", "date", "returns", "rf", "mktrf", "smb", "hml", "rmw", "cma", "umd",
# "effective_federal_funds_rate", "inflation_expectation", "pcepi", etc.
gc()
panel_data <- read.csv("D:/Bayesian_State_State_Data/Data_Panel/panel_data.csv")
gc()
# Subset only the necessary columns.
# Subset only the necessary columns.
# Subset only the necessary columns.
panel_subset <- panel_data %>%
  select(PERMNO, date, returns, rf, mktrf, smb, hml, rmw, cma, umd,
         effective_federal_funds_rate, inflation_expectation, pcepi)

# Convert date column to Date format and clean returns.
panel_subset <- panel_subset %>%
  mutate(date = as.Date(date)) %>%
  mutate(returns = as.numeric(gsub("[^0-9.-]", "", returns)))

# Calculate the excess return for each observation.
panel_subset <- panel_subset %>%
  mutate(excess_return = returns - rf)

# Check for duplicates and aggregate by taking the mean if necessary.
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
  pivot_wider(names_from = date, values_from = excess_return,
              values_fn = mean)

# For factor returns and macro variables, assume these are common for each date.
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

# Convert returns_wide to a matrix: rows = stocks, columns = dates.
returns_matrix <- as.matrix(returns_wide[,-1])
rownames(returns_matrix) <- returns_wide$PERMNO

# Create the factor returns matrix: T x 6 (mktrf, smb, hml, rmw, cma, umd)
factor_returns <- as.matrix(factor_macro %>% select(mktrf, smb, hml, rmw, cma, umd))

# Create the macro variables matrix: T x 3 (eff_fed, infl_exp, pcepi)
macro_vars <- as.matrix(factor_macro %>% select(eff_fed, infl_exp, pcepi))

# Set dimensions for the Stan model.
N <- nrow(returns_matrix)       # Number of stocks
T <- nrow(factor_returns)       # Number of time periods (dates)
K <- ncol(factor_returns)       # Number of factors = 6
M <- ncol(macro_vars)           # Number of macro variables = 3

# -----------------------------
# 2. Stan Model Code with Macro-Driven Drift and Hierarchical Structure
# -----------------------------
# In this model, each stock i has a matrix beta[i] of dimensions T x K,
# where each row represents the factor loadings at time t.
stan_code <- "
data {
  int<lower=1> N;       // Number of stocks
  int<lower=1> T;       // Number of time periods
  int<lower=1> K;       // Number of factors
  int<lower=1> M;       // Number of macro variables
  matrix[T, K] F;       // Factor returns (T x K)
  matrix[T, M] X;       // Macro variables (T x M)
  matrix[N, T] y;       // Excess returns for each stock (N x T)
}
parameters {
  matrix[N, K] beta0;             // Initial beta for each stock
  matrix[K, M] Gamma;             // Macro influence on beta drift
  vector<lower=0>[K] sigma_beta;  // Std dev for beta evolution for each factor
  real<lower=0> sigma_y;          // Measurement error std dev
  
  // Latent states: each stock's beta is a matrix of dimensions T x K.
  matrix[T, K] beta[N];
  
  // Hyperparameters for hierarchical prior on initial beta
  vector[K] mu_beta;
  vector<lower=0>[K] sigma_mu;
}
model {
  // Hyperpriors for the common initial beta parameters
  mu_beta ~ normal(0, 1);
  sigma_mu ~ cauchy(0, 2.5);
  
  // Hierarchical prior for each stock's initial beta
  for (i in 1:N) {
    beta0[i] ~ normal(mu_beta, sigma_mu);
    beta[i][1] ~ multi_normal(beta0[i]', diag_matrix(square(sigma_beta)));
  }
  
  // Priors for Gamma, sigma_beta, and sigma_y
  to_vector(Gamma) ~ normal(0, 1);
  sigma_beta ~ cauchy(0, 2.5);
  sigma_y ~ cauchy(0, 2.5);
  
  // State evolution: macro-driven beta drift
  for (i in 1:N) {
    for (t in 2:T) {
      // Convert the macro effect to a row_vector: (Gamma * X[t]')' gives a row_vector.
      beta[i][t] ~ multi_normal(beta[i][t-1] + (Gamma * X[t]')', diag_matrix(square(sigma_beta)));
    }
  }
  
  // Measurement equation: observed excess returns are generated by latent betas and factor returns
  for (i in 1:N) {
    for (t in 1:T) {
      y[i, t] ~ normal(dot_product(beta[i][t], F[t,]), sigma_y);
    }
  }
}
"

# -----------------------------
# 3. Compile and Fit the Stan Model
# -----------------------------
fit <- stan(model_code = stan_code,
            data = list(N = N,
                        T = T,
                        K = K,
                        M = M,
                        F = factor_returns,   # T x K matrix of factor returns
                        X = macro_vars,       # T x M matrix of macro variables
                        y = returns_matrix),  # N x T matrix of excess returns
            iter = 2000,
            chains = 4,
            seed = 123)

# Print summary of results
print(fit)