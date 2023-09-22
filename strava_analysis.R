#' ---
#' title: "strava_analysis"
#' format: html
#' editor: visual
#' output:
#'     df_print: paged
#' ---
#' 
#' ## Data tidy
#' 
## --------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(janitor)
data <- read_csv("activities.csv")
data <- data |> janitor::clean_names()
data

#' 
#' Select needed columns and only take runs
#' 
## --------------------------------------------------------------------------------------------------------------

data_select <- data |> 
    select(activity_id:activity_type) |> 
    filter (activity_type == "Run")

#' 
#' Separate into `date` and `time` columns
#' 
## --------------------------------------------------------------------------------------------------------------
data_split_date <- data_select |> 
    separate(activity_date, c('date', 'time'), sep = ", (?=[^,]*$)")
data_split_date

#' 
#' Turn date into proper date formate, make a day of the week `wday` column
#' 
## --------------------------------------------------------------------------------------------------------------

data_proper_date <- data_split_date |> 
    # turn to system's date format
    mutate(date = as.Date(date, format = "%b %d, %Y")) |> 
    mutate(wday = weekdays(date))

#' 
#' Complicated magic to finally handle time, and add the 8 hours (can't make timezone functions work)
#' 
## --------------------------------------------------------------------------------------------------------------
# Play with proper_time before appending to dataframe
proper_time <- data_proper_date$time

proper_time <- proper_time |> 
    strptime("%I:%M:%S %p",  tz = "UTC") |> 
    as.POSIXct()

proper_time <- hms::hms(second(proper_time), minute(proper_time), hour(proper_time))

# finally on datetime format that can be added to 
proper_time <- proper_time |> 
    as.POSIXct() + hours(8)



#' 
#' Append fixed time to dataframe
#' 
## --------------------------------------------------------------------------------------------------------------
data_fixed_time <- data_proper_date
data_fixed_time$time <- proper_time

#' 
#' ## Data visualizations
#' 
#' > The `time` being graphed is the start time of the activity
#' 
#' ### Bar chart
#' 
#' Chart runs by day of the week
#' 
## --------------------------------------------------------------------------------------------------------------
#| label: bar-chart

week <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
ggplot(
    data_proper_date, aes(x = wday)
) + 
    geom_bar() +
    # labels
    labs(x = "Day of the Week", y = "Count") +
    # order day of the weeks properly
    scale_x_discrete(limits = week)

#' 
#' ### Density plot
#' 
#' A density plot on the time of start of runs
#' 
## --------------------------------------------------------------------------------------------------------------
#| label: density

library(scales)
ggplot(data_fixed_time) + 
    geom_density(
        aes(x = time, y = after_stat(scaled)),
        fill = "darkcyan", alpha = 0.8
    ) +
    # fix x ticks
    scale_x_datetime(breaks = date_breaks("2 hours"), labels=date_format("%H:%M"))


#' 
#' ### Histogram
#' 
## --------------------------------------------------------------------------------------------------------------
#| label: histogram

ggplot(data_fixed_time) + 
    geom_histogram(aes(x = time), fill = "darkcyan", alpha = 0.8) +
    scale_x_datetime(
        breaks = date_breaks("2 hours"),  labels = date_format("%H:%M")
)

#' 
#' ### Multiple densities
#' 
#' Density plots like previous but superimposed across each day of the week
#' 
## --------------------------------------------------------------------------------------------------------------
#| label: density-multi

library(viridis)
ggplot(data_fixed_time) + 
    geom_density(
        aes(x = time, group = wday, fill = wday, y = after_stat(scaled)), alpha = 0.5
    ) + 
    # labels
    labs(
        x = "Time", 
        group = "Day of the week", fill = "Day of the week",
        ) +
    # x ticks by 
    scale_x_datetime(breaks = date_breaks("2 hours"), labels = date_format("%H:%M")) + 
    # fancier colors from viridis
    scale_fill_viridis(discrete = TRUE) 


#' 
#' ### Ridgeline
#' 
#' Like multiple densities but separated by day, less messier
#' 
## --------------------------------------------------------------------------------------------------------------
#| label: ridgeline

library(ggridges)
ggplot(data_fixed_time) +
    geom_density_ridges(
        aes(x = time, y = wday, fill = wday), 
        alpha = 0.7, scale = 1, show.legend = FALSE
    ) +
    labs(x = "Time", y = "Day") +
    # order the wday (day of the week)
    scale_y_discrete(limits = week) + 
    # x-axis name and intervals
    scale_x_datetime(breaks = date_breaks("2 hours"),  labels = date_format("%H:%M")) + 
    # coloring
    scale_fill_viridis(discrete = TRUE)


