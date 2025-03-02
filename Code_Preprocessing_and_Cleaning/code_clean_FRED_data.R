
######################################################################
# Let's create a function to clean and combine all our FRED datasets #
######################################################################


library(readxl)
library(dplyr)

combine_excel_files <- function(folder_path) {
  # List all Excel files in the folder (change pattern if your files have a different extension)
  files <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)
  
  # Loop through each file and process it
  df_list <- lapply(files, function(file) {
    # Read the "Monthly" sheet
    data <- read_excel(file, sheet = "Monthly")
    
    # Identify the variable column (assuming only two columns: observation_date and one variable column)
    var_col <- setdiff(names(data), "observation_date")
    
    # Create a name based on the file name (without extension) for the variable column
    file_base <- tools::file_path_sans_ext(basename(file))
    
    # Select and rename the variable column
    data <- data %>%
      select(observation_date, all_of(var_col)) %>%
      rename(!!file_base := all_of(var_col))
    
    return(data)
  })
  
  # Merge all data frames by the "observation_date" column using a full join
  combined_df <- Reduce(function(x, y) full_join(x, y, by = "observation_date"), df_list)
  
  return(combined_df)
}

# Usage:
folder_path <- "C:/Users/dlucko/Documents/GitHub/bayesian_dynamic_factor_model-/Data_FRED_Raw"
fred_data <- combine_excel_files(folder_path)

fred_data <- fred_data %>%
  rename(date = observation_date, effective_federal_funds_rate = EFFR, inflation_expectation = MICH, pcepi= PCEPI, pcepi_pc1 = PCEPI_PC1, pceplfe = PCEPILFE, pcepilfe_pc1 = PCEPILFE_PC1)

fred_data <- fred_data %>% mutate(date = as.Date(date))


# View the first few rows of the combined data frame
head(fred_data)


write.csv(fred_data, "C:/Users/dlucko/Documents/GitHub/bayesian_dynamic_factor_model-/Data_FRED_Cleaned/fred_data_cleaned.csv")
