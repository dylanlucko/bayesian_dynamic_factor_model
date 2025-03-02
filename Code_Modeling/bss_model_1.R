# ---------------------------------------------------------------
# INSTALL PACKAGES IF NEEDED
# install.packages("bsts")
# install.packages("dplyr")
# ---------------------------------------------------------------
library(bsts)
library(dplyr)

# Suppose final_df is already merged:
# final_df = crsp_equities_df %>%
#   inner_join(factors_macro, by = "date")

# 1) Filter for a single stock (for demonstration)
#    In practice, you might run separate models or do a hierarchical approach.
single_stock_df <- final_df %>%
  filter(PERMNO == 10000) %>%     # Just an example PERMNO
  arrange(date)

# 2) Construct the response and predictors
#    Excess return = RET - rf
single_stock_df <- single_stock_df %>%
  mutate(
    excess_ret = RET - rf,  # e.g., rf in decimal format
    mkt_rf     = mktrf,     # rename for convenience
    infl_exp   = infl_exp   # assume your monthly inflation exp
  )

# 3) Build the state-space "AddLocalLevel" and "AddDynamicRegression" specification
ss <- list()
# Add a local level term for the intercept if desired
ss <- AddLocalLevel(ss, single_stock_df$excess_ret)

# Define your regressors:
predictors <- single_stock_df %>%
  select(mkt_rf, smb, hml, rmw, cma, umd, eff_fed_funds_rate, infl_exp) %>%
  as.matrix()

# Add dynamic regression for time-varying coefficients
ss <- AddDynamicRegression(ss,
                           y = single_stock_df$excess_ret,
                           x = predictors)

# 4) Fit the BSTS model
model_bsts <- bsts(excess_ret ~ 0,     # no static regressors in formula
                   state.specification = ss,
                   data = single_stock_df,
                   niter = 2000,            # increase for real usage
                   ping = 0,               # reduce console printing
                   seed = 123)

# 5) Summarize results
summary(model_bsts)
plot(model_bsts)

# 'model_bsts' now contains MCMC draws for the time-varying coefficients:
coefficients_bsts <- model_bsts$coefficients
# This is a 3D array: iteration x time x coefficient
