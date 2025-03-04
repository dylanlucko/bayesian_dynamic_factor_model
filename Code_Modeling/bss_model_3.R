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

# Convert date to Date format and clean returns.
panel_subset <- panel_subset %>%
  mutate(date = as.Date(date)) %>%
  mutate(returns = as.numeric(gsub("[^0-9.-]", "", returns)))

# Calculate excess return
panel_subset <- panel_subset %>%
  mutate(excess_return = returns - rf)

# Check for duplicates and aggregate (taking the mean) if necessary.
duplicates <- panel_subset %>%
  group_by(PERMNO, date) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)
if(nrow(duplicates) > 0){
  message("Duplicates found; aggregating by taking the mean excess_return.")
}

# Create wide-format returns matrix: rows = stocks, columns = dates.
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

# Create factor returns matrix: T x 6 (mktrf, smb, hml, rmw, cma, umd)
factor_returns <- as.matrix(factor_macro %>% select(mktrf, smb, hml, rmw, cma, umd))

# Create macro variables matrix: T x 3 (eff_fed, infl_exp, pcepi)
macro_vars <- as.matrix(factor_macro %>% select(eff_fed, infl_exp, pcepi))

# Set dimensions for the Stan model.
N <- nrow(returns_matrix)       # Number of stocks
T <- nrow(factor_returns)       # Number of time periods (dates)
K <- ncol(factor_returns)       # Number of factors = 6
M <- ncol(macro_vars)           # Number of macro variables = 3

# -----------------------------
# 2. Stan Model Code with Non-centered Hierarchical Prior and Macro-Driven Drift
# -----------------------------
# -----------------------------
# 2. Stan Model Code with Non-centered Hierarchical Prior and Macro-Driven Drift
# -----------------------------
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
  matrix[N, K] beta0_raw;         // Non-centered initial beta (standard normal)
  matrix[K, M] Gamma;             // Macro influence on beta drift
  vector<lower=0>[K] sigma_beta;  // Std dev for beta evolution for each factor
  real<lower=0> sigma_y;          // Measurement error std dev
  
  // Latent states: each stock's beta is a matrix of dimensions T x K.
  matrix[T, K] beta[N];
  
  // Hyperparameters for hierarchical prior on initial beta
  vector[K] mu_beta;
  vector<lower=0>[K] sigma_mu;
}
transformed parameters {
  matrix[N, K] beta0; // Transformed initial beta for each stock
  for (i in 1:N) {
    // Apply non-centered parameterization by converting beta0_raw to a vector.
    beta0[i] = to_row_vector(mu_beta + sigma_mu .* to_vector(beta0_raw[i]));
  }
}
model {
  // Hyperpriors for the common initial beta parameters
  mu_beta ~ normal(0, 1);
  sigma_mu ~ cauchy(0, 2.5);
  
  // Non-centered hierarchical prior for initial beta:
  to_vector(beta0_raw) ~ normal(0, 1);
  for (i in 1:N) {
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
  
  // Measurement equation: observed excess returns are generated by latent betas and factor returns.
  for (i in 1:N) {
    for (t in 1:T) {
      y[i, t] ~ normal(dot_product(beta[i][t], F[t,]), sigma_y);
    }
  }
}
"

# Check for missing values in the returns matrix
na_count <- sum(is.na(returns_matrix))
message("Number of NAs in returns_matrix: ", na_count)

# Option 1: Remove stocks (rows) with any NA values
if(na_count > 0) {
  returns_matrix_clean <- returns_matrix[complete.cases(returns_matrix), ]
  message("Number of stocks before cleaning: ", nrow(returns_matrix))
  message("Number of stocks after cleaning: ", nrow(returns_matrix_clean))
} else {
  returns_matrix_clean <- returns_matrix
}

# Adjust N accordingly
N_clean <- nrow(returns_matrix_clean)

# Now, use returns_matrix_clean in the Stan data:
fit <- stan(model_code = stan_code,
            data = list(N = N_clean,
                        T = T,
                        K = K,
                        M = M,
                        F = factor_returns,   # T x K matrix of factor returns
                        X = macro_vars,       # T x M matrix of macro variables
                        y = returns_matrix_clean),  # N_clean x T matrix of excess returns
            iter = 1000,
            chains = 4,
            cores = 4,
            verbose = TRUE,
            refresh = 1,
            seed = 123,
            control = list(adapt_delta = 0.95))  # Increase target acceptance rate

print(fit)