# Fixes to activity_date to date and time
library(tidyverse)
library(janitor)
library(scales)
library(ggridges)
library(viridis)

df <- read_csv("activities.csv")
df <- janitor::clean_names(df) # Clean column names

# Delete columns with all rows NA
df <- df |> select(where(\(x) !all(is.na(x))))

# Change to POSIXct datetime and add 8 hours due to Strava export differences
df$activity_date <- df$activity_date |>
    lubridate::parse_date_time("mdyIMs p") |> # month, day, year, hour, minute, sec, pm/am
    (`+`)(hours(8)) # adds hours(8), implicit function using ()()

# Add date and wday column
df_date <- df |> 
    mutate(
        date = lubridate::as_date(activity_date), .after = activity_date,
        wday = wday(date, label = TRUE)
    )

# Run activities only, change activity_date as time column
df_clean <- df_date  |>
    filter(activity_type == "Run") |> 
    rename(time = activity_date)
    
# Change to time
# The variable time is a datetime (POSIXct)
# Use hms to remove date data (we plot time in the interval 00:00-24:00, irrespective of date)
# then back to POSIXct as ggplot only receives datetime
df_datetime <- df_clean
df_datetime$time <- hms::hms(second(df_clean$time), minute(df_clean$time), hour(df_clean$time)) |> 
    as.POSIXct()

