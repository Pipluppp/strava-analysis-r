# Exploratory analysis
library(tidyverse)

df_month_week_day <- df_month_week_day

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
    geom_boxplot(aes(x = month, y = moving_time_m))

