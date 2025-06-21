rm(list = ls())

suppressMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(lubridate)
})

############################### FUNCTIONS ###############################

get_contract_year <- function(date, year_code) {

  # Return NA if any arguments are NA
  if (is.na(date) || is.na(year_code)) {
    return(NA)
  }

  # Extract the year from timestamp
  data_year <- as.integer(format(date, "%Y"))
  
  # Check if year code is 2 digits
  if (year_code >= 10) {

    # Look for the closest VALID matches in 1900s or 2000s
    valid_years <- seq(1900, 2999, by = 1)
    valid_years <- valid_years[
      which(valid_years %% 100 == year_code & valid_years >= data_year)
    ]
    
    # Return the first valid year
    if (length(valid_years) > 0) {
      return(valid_years[1])
    } else {
      # If no valid years, return NA
      return(NA)
    }
  }
  
  # If year code is 1 digit, find closest match in the next 20 years
  candidate_years <- data_year:(data_year + 20)
  match_year <- candidate_years[which(candidate_years %% 10 == year_code)][1]
  
  return(match_year)
}

########################### LOAD & FILTER DATA ###########################

# Define paths
current_dir <- dirname(sys.frame(1)$ofile)
cme_data_path <- file.path(current_dir, "../data/CME_raw_data.csv")

# Load CME futures data
futures_df <- read_csv(cme_data_path, show_col_types = FALSE) %>%
  mutate(date = as.Date(ts_event)) %>%
  select(
    Date = date,
    Ticker = symbol,
    Price = close,
    Volume = volume,
  )

# Exclude spreads
futures_df <- futures_df %>%
  filter(!grepl("-", Ticker)) %>%
  filter(!grepl(":", Ticker))

# Remove weekend data
futures_df <- futures_df %>%
  filter(wday(Date) %in% 2:6)

######################### EXTRACT CONTRACT DATA #########################

# Extract year code (last 1 or 2 numbers)
futures_df <- futures_df %>%
  mutate(
    Year_Code  = str_extract(Ticker, "[0-9]{1,2}$") %>% as.integer()
  )

# Extract month code (single letter followed by 1 or 2 digits)
futures_df <- futures_df %>%
  mutate(Month_Code = str_extract(Ticker, "[A-Z](?=[0-9]{1,2}$)"))

# Extract underlying (ie strip out year and month codes)
futures_df <- futures_df %>%
  mutate(Underlying = str_remove(Ticker, "[A-Z][0-9]{1,2}$"))

# Get contract year based on date and codes
futures_df <- futures_df %>%
  mutate(
    Contract_Year = mapply(get_contract_year, Date, Year_Code)
  )

# Load futures month-code map
month_code_path <- file.path(current_dir, "../utils/month_code_map.csv")
month_code_map <- read_csv(month_code_path, show_col_types = FALSE)

# Left join data with month code map
futures_df <- futures_df %>%
  left_join(
    month_code_map %>% select(Month_Code, Contract_Month),
    by = "Month_Code"
  )

# Re-format tickers to more readable format Underlying_ContractMonth_ContractYear
futures_df <- futures_df %>%
  mutate(
    Ticker = paste0(Underlying, "_", Contract_Month, "_", Contract_Year)
  ) %>%
  select(Date, Ticker, Underlying, Price, Volume)

####################### FIND MOST LIQUID CONTRACTS #######################

# Find most liquid contracts per day (use which.max)
liquid_df <- futures_df %>%
  group_by(Date, Underlying) %>%
  slice(which.max(Volume)) %>%
  ungroup() %>%
  select(Date, Ticker, Underlying, Volume)

# Create groups for run-length analysis
run_length_df <- liquid_df %>%
  group_by(Underlying) %>%
  arrange(Date) %>%
  mutate(
    Ticker_Change = Ticker != lag(Ticker, default = first(Ticker)),
    Run_ID = cumsum(Ticker_Change)
  ) %>%
  ungroup()

# Analyze run-lengths of liquid tickers for each underlying
run_length_df <- run_length_df %>%
  group_by(Underlying, Ticker, Run_ID) %>%
  summarise(
    Start_Date = min(Date),
    End_Date = max(Date),
    Run_Length = n(),
    .groups = "drop"
  ) %>%
  arrange(Underlying, Start_Date)

######################## DETERMINE REQUIRED DATA ########################

# Same day data for liquid contracts
required_df <- liquid_df %>%
  select(Date, Ticker, Underlying)

# Prior-day data for liquid contracts
prior_day_df <- liquid_df %>%
  select(Date, Ticker, Underlying) %>%
  mutate(Date = case_when(
    wday(Date) %in% 3:6 ~ Date - 1,   # Tue - Fri
    wday(Date) == 2 ~ Date - 3,       # Mon (previous Friday)
    TRUE ~ NA                         # Sat/Sun (shouldn't be any)
  ))

# Combine current and prior-day required data
required_df <- required_df %>%
  bind_rows(prior_day_df)

########################## HANDLE MISSING DATA ##########################

# Summmaise date ranges for each ticker
date_summary_df <- futures_df %>%
  group_by(Ticker) %>%
  summarise(
    Start_Date = min(Date),
    End_Date = max(Date),
    Total_Days = n_distinct(Date),
    .groups = "drop"
  )

# Generate a complete set of trading dates
all_dates <- seq.Date(from = min(futures_df$Date), to = max(futures_df$Date), by = "day")
all_dates <- all_dates[wday(all_dates) %in% 2:6]

# Check for any dates with no liquid contracts
missing_liquidity_df <- liquid_df %>%
  select(Date, Underlying, Ticker) %>%
  group_by(Underlying) %>%
  complete(Date = all_dates) %>%
  filter(is.na(Ticker))

# Check for missing required data
missing_required_df <- required_df %>%
  select(Date, Underlying, Ticker) %>%
  anti_join(
    futures_df %>% select(Date, Underlying, Ticker),
    by = c("Date", "Underlying", "Ticker")
  ) %>%
  filter(Date %in% all_dates)

# Add required missing data
futures_df <- futures_df %>%
  bind_rows(missing_required_df) %>%
  group_by(Ticker) %>%
  arrange(Date) %>%
  mutate(Volume = ifelse(is.na(Volume), 0, Volume)) %>%
  fill(Price, .direction = "down")

######################## CALCULATE RETURN SERIES ########################
