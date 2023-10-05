# Week history plot a la Strava, from https://github.com/marcusvolz/strava/blob/master/R/week_history_plot.R
df_month_week_day |>
    group_by(week) |> 
    summarise(week_mileage = sum(distance_km)) |> 
    ggplot() +
    geom_line(
        aes(x = week, y = week_mileage),
        color = "#fc4c02",
        linewidth = 0.6
    ) +
    geom_point(
        aes(x = week, y = week_mileage),
        shape = 21,
        fill = "white",
        color = "#fc4c02",
        stroke = 1,
        size = 1.5
    ) +
    geom_area(
        aes(x = week, y = week_mileage),
        fill = "#fc4c02",
        alpha = 0.1
    )
               