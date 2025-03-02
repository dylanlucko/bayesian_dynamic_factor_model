
##############################################################################
# Here, let us first load in the equities data for pre-processing and cleaning.
# I reference an external drive as the file is too large to keep on github. 
##############################################################################


# Load required libraries
library(readr)      # For reading CSV files
library(readxl)     # For reading Excel files (if needed)
library(dplyr)      # For data manipulation
library(lubridate)  # For working with dates
library(ggplot2)    # For visualization

# File path
file_path <- "D:/Bayesian_State_State_Data/Equitites/us_equities_data_2000_2024.csv"

# Load data
us_equities <- read_csv(file_path)

us_equities <- us_equities %>%
  rename(share_code = SHRCD, permanent_company_number = PERMCO, exchange_code = EXCHCD, primary_exchange = PRIMEXCH, company_name = COMNAM, price = PRC, bid_low_price = BIDLO, bid = BID, ask = ASK, volume = VOL, ticker= TICKER, returns = RET, spread = SPREAD,  returns_sans_dividends = RETX, shares_outstanding = SHROUT, shrenddt = SHRENDDT, value_weighted_return = vwretd, value_weighted_return_sans_dividend = vwretx, equal_weighted_return = ewretd, delisting_return = DLRET, declaration_date = DCLRDT, record_date = RCRDDT, cusip = CUSIP, share_class = SHRCLS, market_maker_count = MMCNT, return_on_sp_composite_index = sprtrn, ask_high_price = ASKHI, dividend_cash_amount = DIVAMT, factor_to_adjust_price = FACPR, cumulative_factor_to_adjust_price = CFACPR)

# Convert date to Date format
us_equities <- us_equities %>% mutate(date = as.Date(date))

# Display first few rows
print(head(us_equities))

# Check structure of dataset
print(str(us_equities))

us_equities <- us_equities %>%
  mutate(date = floor_date(date, "month"))  # Convert to first day of the month

head(us_equities)
# Save cleaned data
write.csv(us_equities, "C:/Users/dlucko/Documents/GitHub/bayesian_dynamic_factor_model-/Data_CRSP_Annual_Equities_Cleaned/us_equities_cleaned.csv")



###########################
###########################
#### Check data ###########
###########################
###########################


# Check date range
date_range <- range(us_equities$date, na.rm = TRUE)
print(paste("Date Range:", date_range[1], "to", date_range[2]))

# Check granularity (Daily vs. Monthly)
granularity <- us_equities %>% arrange(date) %>% mutate(date_diff = date - lag(date)) %>% count(date_diff)
print(granularity)

# Summary statistics
print(summary(us_equities))

# Number of unique tickers
num_tickers <- us_equities %>% distinct(ticker  ) %>% nrow()
print(paste("Unique Tickers:", num_tickers))

# Most frequent tickers
top_tickers <- us_equities %>% count(ticker  , sort = TRUE) %>% head(10)
print(top_tickers)

# Check for missing values
missing_values <- colSums(is.na(df))
print(missing_values)

# Plot stock price trend for a sample stock (AAPL)
sample_ticker <- "AAPL"
df_sample <- us_equities %>% filter(ticker   == sample_ticker)

ggplot(df_sample, aes(x = date, y = price)) +
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
