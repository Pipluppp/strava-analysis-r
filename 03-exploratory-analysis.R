# Exploratory analysis
library(tidyverse)

# Limit to 2023
df_month_week_day <- df_month_week_day |> filter(date < "2024-01-01")

# Mileage by wday    
ggplot(df_month_week_day) +
    geom_col(aes(x = wday, y = distance_km))

# Mileage by month
ggplot(df_month_week_day) +
    geom_col(aes(x = month, y = distance_km)) 

# Mileage by week
ggplot(df_month_week_day) +
    geom_col(aes(x = week, y = distance_km)) 

# If graphing average pace by month, the average must be weighted
# Just do scatterplots
ggplot(df_month_week_day) +
    geom_point(aes(x = distance_km, y = average_pace_elapsed_km))

ggplot(df_month_week_day) +
    geom_point(aes(x = moving_time_m, y = average_pace_elapsed_km))

# Box plot of distance by month
ggplot(df_month_week_day) + 
    geom_boxplot(aes(x = month, y = distance_km))


# Mileage for the day than distance of a run
# since I tend to do multiple Strava run sessions (warm up, cooldown, etc.)

# New dataframe for mileage per day
df_daily_mileage <- df_month_week_day |> 
    group_by(date) |> 
    summarise(mileage = sum(distance_km)) |> 
    
    mutate(month = lubridate::month(date, label = TRUE)) |> 
    mutate(week = lubridate::week(date))

# Scatterplot using daily mileage
df_daily_mileage |>  
    ggplot(aes(x = date, y = mileage)) +
    geom_point() + 
    geom_smooth(method = "lm")

# Column chart of mileage per month
df_daily_mileage |> 
    ggplot(aes(x = month, y = mileage)) +
    geom_col()

# Column chart of mileage per week
df_daily_mileage |>  ggplot(aes(x = week, y = mileage)) +
    geom_col()

# Boxplot using daily mileage
df_daily_mileage |> 
    ggplot(aes(x = month, y = mileage)) +
    geom_boxplot()

# Barplot of number of days I ran each month
df_daily_mileage |> 
    ggplot(aes(x = month)) +
    geom_bar() +
    labs(
        title = "Ran the most days during March",
        x = "Month", y = "Number of days"
    )
