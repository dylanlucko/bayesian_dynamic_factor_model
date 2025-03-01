
library(dplyr)


# Here, let us first load in the equities data for pre-processing and cleaning.
# I reference an external drive as the file is too large to keep on github. 


# Load required libraries
library(readr)      # For reading CSV files
library(readxl)     # For reading Excel files (if needed)
library(dplyr)      # For data manipulation
library(lubridate)  # For working with dates
library(ggplot2)    # For visualization

# File path
file_path <- "D:/Bayesian_State_State_Data/Equitites/us_equities_data_2000_2024.csv"

# Load data
df <- read_csv(file_path)

# Convert date to Date format
df <- df %>% mutate(date = as.Date(date))

# Display first few rows
print(head(df))

# Check structure of dataset
print(str(df))

# Check date range
date_range <- range(df$date, na.rm = TRUE)
print(paste("Date Range:", date_range[1], "to", date_range[2]))

# Check granularity (Daily vs. Monthly)
granularity <- df %>% arrange(date) %>% mutate(date_diff = date - lag(date)) %>% count(date_diff)
print(granularity)

# Summary statistics
print(summary(df))

# Number of unique tickers
num_tickers <- df %>% distinct(TICKER  ) %>% nrow()
print(paste("Unique Tickers:", num_tickers))

# Most frequent tickers
top_tickers <- df %>% count(TICKER  , sort = TRUE) %>% head(10)
print(top_tickers)

# Check for missing values
missing_values <- colSums(is.na(df))
print(missing_values)

# Plot stock price trend for a sample stock (AAPL)
sample_ticker <- "AAPL"
df_sample <- df %>% filter(TICKER   == sample_ticker)

ggplot(df_sample, aes(x = date, y = PRC)) +
  geom_line(color = "blue") +
  labs(title = paste("Stock Price of", sample_ticker), x = "Date", y = "Price") +
  theme_minimal()

# Plot distribution of stock returns
ggplot(df, aes(x = RET)) +
  geom_histogram(bins = 50, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Stock Returns", x = "Daily Return", y = "Frequency") +
  theme_minimal()

length(unique(bss$TICKER))
length(unique(bss$EXCHCD))
