# Clean and add date and times
library(tidyverse)

df_datetime <- df_speed

# Change to datetime type (POSIXct)
df_datetime$activity_date <- df_speed$activity_date |>
    lubridate::parse_date_time("mdyIMs p") |> # month, day, year, hour, minute, sec, pm/am
    (`+`)(hours(8)) # adds hours(8) due to Strava export difference, implicit function using ()()

# Make date column from activity_date
df_month_week_day <- df_datetime |> 
    mutate(date = lubridate::as_date(activity_date), .after = activity_date) |> 
    
    # Reuse activity_date column as time
    rename(time = activity_date)

# Remove date component of the datetime (places all the dates of the time column to 1970)
df_month_week_day$time <- hms::hms(second(df_month_week_day$time), minute(df_month_week_day$time), hour(df_month_week_day$time)) |> 
    as.POSIXct()

# Add month, week, weekday columns
df_month_week_day <- df_month_week_day |> 
    mutate(
        month = lubridate::month(date,  label = TRUE),
        week  = lubridate::week(date),
        wday  = lubridate::wday(date, label = TRUE)
    ) |> 
    relocate(month:wday, .after = date)

