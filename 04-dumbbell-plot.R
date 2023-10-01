# Recreate Dumbbell plot
# https://github.com/marcusvolz/strava_py/tree/main#dumbbell-plot
# https://r-graph-gallery.com/web-extended-dumbbell-plot-ggplot2.html
library(scales)

# Undo removal of date component in the activity_date
# Get activity start and end datetime
df_start_times <- df_month_week_day |> 
    select(moving_time_h, moving_time_m, moving_time_s, date) |> 
    
    # mutate(start_time = df_datetime$activity_date) |> 
    # mutate(end_time = start_time + moving_time_s) |> 
    
    mutate(start_time = df_month_week_day$time) |> 
    mutate(end_time = start_time + moving_time_s) |> 
    mutate(yday = lubridate::yday(date))

# Plot structure
df_start_times |> ggplot() + 
    geom_segment(
            aes(x = start_time, y = yday,
            xend = end_time, yend = yday
        ),
        size = 0.2
    ) +
    geom_point(aes(x = start_time, y = yday, color = "coral3"), size = 0.5) + 
    geom_point(aes(x = end_time, y = yday, color = "cyan4"), size = 0.5) +
    scale_x_datetime(breaks = date_breaks("1 hour"), labels = date_format("%H:%M")) + 
    scale_y_continuous(breaks = seq(0, 262, by = 30)) +
    labs(
        title = "Dumbbell plot of duration of each run",
        x = "Time",
        y = "Day of the year",
        color = "Markers"
    ) +
    scale_color_manual(labels = c("Start", "End"), values = c("cyan4", "coral3"))
    
ggsave("viz/fig-dumbbell.png", width = 12, height = 12)


