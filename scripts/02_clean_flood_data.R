
# Bailey Binando & Lillian Pates

# Load Packages
library(tidyverse)
library(readxl)
library(here)
library(slider)

# Load Data
observed_flow <- read_csv(here("data", "raw", "daily_hydrology_pozarica_1952_2025.csv"))
predicted_flow <- read_csv(here("data", "raw", "forecasted_flow.csv"))
flood_events <- read_xlsx(here("data", "raw", "poza_rica_floods_1971_2025_combined.xlsx"))

# Clean
# prep for dataset join 
# convert str date to dttm
forecast_flow <- predicted_flow |>
  rename(pred_flow = flow) |> # rename var.
  mutate(
    date = mdy(date)
  )

floods <- flood_events |>
  mutate(
    date = ymd(date), 
    flood_flag = 1 # create binary flood indicator
  ) |>
  select(date, flood_flag, cause, flood_type) # keep only certain cols.

# join datasets
combine <- forecast_flow |>
  left_join(observed_flow, by = "date") |>
  left_join(floods, by = "date") |>
  mutate(
    flood_flag = if_else(is.na(flood_flag), 0, flood_flag)
  )

# create multiple flood windows
# multi-day floods; day reported != to actual day of flood
flood_window <- combine |>
  mutate(
    flood_event = flood_flag,
    flood_window_m2_p2 = slide_int(flood_event, max, .before = 2, .after = 2),
    flood_window_m3_p3 = slide_int(flood_event, max, .before = 3, .after = 3),
    flood_window_m2_p1 = slide_int(flood_event, max, .before = 2, .after = 1),
    flood_window_m2_p3 = slide_int(flood_event, max, .before = 2, .after = 3)
  )

# check
head(flood_window)
tail(flood_window)
summary(flood_window)
any(duplicated(flood_window$date))
colSums(is.na(flood_window))

# Write File
write_csv(combine, here("data", "processed", "flood_timeseries_clean.csv"))


