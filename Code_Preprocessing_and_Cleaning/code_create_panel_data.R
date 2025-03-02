library(dplyr)
library(readr)
library(lubridate)

# Define file paths
path_fred <- "C:/Users/dlucko/Documents/GitHub/bayesian_dynamic_factor_model-/Data_FRED_Cleaned"
path_fama_french <- "C:/Users/dlucko/Documents/GitHub/bayesian_dynamic_factor_model-/Data_CRSP_Fama_French_Factors_Cleaned"
path_equities_zip <- "C:/Users/dlucko/Documents/GitHub/bayesian_dynamic_factor_model-/Data_CRSP_Annual_Equities_Cleaned/Equities_Cleaned_ZIP.zip"

# Create a temporary directory for ZIP extraction
temp_dir <- tempfile()
dir.create(temp_dir)

# Extract ZIP file to temp directory
unzip(path_equities_zip, exdir = temp_dir)

# List files **recursively** inside the extracted folder (to find files inside subfolders)
path_equities <- list.files(temp_dir, full.names = TRUE, pattern = "\\.csv$", recursive = TRUE)

# Debugging: Print the extracted file paths to ensure we find the right ones
print(path_equities)

# Read all CSV files found inside the extracted ZIP folder
equities_data <- lapply(path_equities, read_csv) %>% bind_rows()

# Ensure the date format is consistent
equities_data <- equities_data %>% mutate(date = as.Date(date))

# List and load FRED data files
fred_files <- list.files(path_fred, full.names = TRUE, pattern = "\\.csv$")
fred_data <- lapply(fred_files, read_csv) %>% bind_rows() %>% mutate(date = as.Date(date))

# List and load Fama-French factor files
fama_french_files <- list.files(path_fama_french, full.names = TRUE, pattern = "\\.csv$")
fama_french_data <- lapply(fama_french_files, read_csv) %>% bind_rows() %>% mutate(date = as.Date(date))

# Merge all datasets based on date
final_dataset <- equities_data %>%
  inner_join(fama_french_data, by = "date") %>%
  inner_join(fred_data, by = "date")


# Define base directory
base_path <- "D:/Bayesian_State_State_Data/"

# Create "Data_Panel" directory if it does not exist
data_panel_path <- file.path(base_path, "Data_Panel")
if (!dir.exists(data_panel_path)) {
  dir.create(data_panel_path)
  print(paste("Created directory:", data_panel_path))
} else {
  print(paste("Directory already exists:", data_panel_path))
}

# Define the output file path
output_file <- file.path(data_panel_path, "panel_data.csv")

# Save the merged dataset
write_csv(final_dataset, output_file)

# Print confirmation message
print(paste("Final dataset saved to:", output_file))

# Clean up extracted ZIP files
unlink(temp_dir, recursive = TRUE)
