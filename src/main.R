suppressMessages({
  library(dplyr)
  library(readr)
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









####################### FIND MOST LIQUID CONTRACTS #######################

# Find biggest volume contracts on each day
liquid_contract_df <- futures_df %>%
  group_by(Date, Underlying) %>%
  slice(which.max(Volume)) %>%
  ungroup()

liquid_contract_df <- liquid_contract_df %>%
  group_by(Underlying) %>%
  arrange(Date) %>%
  mutate(
    Same_Contract = Ticker == lag(Ticker, default = first(Ticker)),
    Contract_Group = cumsum(!Same_Contract),
  ) %>%
  ungroup()

liquid_contract_df <- liquid_contract_df %>%
  group_by(Underlying, Contract_Group) %>%
  summarise(
    Ticker = first(Ticker),
    Start_Date = min(Date),
    End_Date = max(Date),
    Count = n(),
    .groups = "drop"
  ) %>%
  arrange(Underlying, Start_Date)

######################## CALCULATE RETURN SERIES ########################
