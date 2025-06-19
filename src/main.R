suppressMessages({
  library(dplyr)
  library(readr)
})

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

# Extract underlying future
futures_df <- futures_df %>%
  mutate(Underlying = gsub("..$", "", Ticker)) %>%
  select(Date, Ticker, Underlying, Price, Volume)

# Find biggest volume contracts on each day
liquid_contract_df <- futures_df %>%
  group_by(Date, Underlying) %>%
  slice(which.max(Volume)) %>%
  ungroup()

#! Include 2 year-digits in Ticker (to avoid duplicates)