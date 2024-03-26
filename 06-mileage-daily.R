df_daily_mileage_2023 <- df_month_week_day |> 
    group_by(date) |> 
    summarise(mileage = sum(distance_km)) |> 
    mutate(month = lubridate::month(date, label = TRUE)) |> 
    mutate(week = lubridate::week(date))

df_daily_mileage_2023 |> 
    ggplot(aes(x = date, y = mileage)) +
    geom_col(width = 0.6) +
    scale_x_date(
        date_breaks = "month", 
        date_labels = "%b", 
        limits = as.Date(c("2023-01-01", "2023-12-31"))
    ) +
    labs(
        title = "Daily mileage (km)",
        x = "Date",
        y = "Mileage"
    ) +
    theme_minimal()

