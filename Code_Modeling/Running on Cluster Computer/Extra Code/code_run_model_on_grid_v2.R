
setwd("/export/home/dor/dlucko/Downloads")# Define the path to the ZIP file


unzip("Data_Panel_ZIP.zip", exdir = "Data_Panel_Extracted")

# 3. Recursively list files in the extraction folder
files <- list.files("Data_Panel_Extracted", 
                    pattern = "^panel_subset\\.csv$", 
                    recursive = TRUE, 
                    full.names = TRUE)

# 4. Check if the CSV was found
if (length(files) == 0) {
  stop("panel_subset.csv not found in the unzipped contents.")
}

# 5. Read the CSV
panel_subset <- read.csv(files[1])

# 6. Verify the data
head(panel_subset)

panel_data <- panel_subset