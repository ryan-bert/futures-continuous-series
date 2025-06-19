suppressMessages({
  library(dplyr)
  library(readr)
})

# Define paths
current_dir <- dirname(sys.frame(1)$ofile)
cme_data_path <- file.path(current_dir, "../data/CME_raw_data.csv")

# Load ES futures data
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
  filter(!grepl("-", Ticker))

# Summarize the data by ticker
ticker_summary_df <- futures_df %>%
  group_by(Ticker) %>%
  summarise(
    Start_Date = min(Date),
    End_Date = max(Date),
    Total_Records = n(),
    .groups = "drop"
  ) %>%
  arrange(Ticker)