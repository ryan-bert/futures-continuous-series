suppressMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

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

######################### FORMAT CONTRACT NAMES #########################

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




####################### FIND MOST LIQUID CONTRACTS #######################


######################## CALCULATE RETURN SERIES ########################
