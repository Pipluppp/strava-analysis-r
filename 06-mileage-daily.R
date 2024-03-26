df_daily_mileage |> 
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

