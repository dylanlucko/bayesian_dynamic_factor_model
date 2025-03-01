
######################################
# Clean Up Fama French 5 Factor Data #
######################################


library(readr)
library(dplyr)
library(lubridate)

# Define file path
file_path <- "C:/Users/dlucko/Documents/GitHub/bayesian_dynamic_factor_model-/Data_CRSP_Fama_French_Factors_Raw/fama_french_5_factor_w_momentum.csv"

# Read CSV file
fama_french_data <- read_csv(file_path)

# Rename 'dateff' to 'date' and convert to Date format
fama_french_data <- fama_french_data %>%
  rename(date = dateff) %>%
  mutate(date = ymd(date))  # Convert to Date format assuming YYYYMMDD format

# View the first few rows
head(fama_french_data)
