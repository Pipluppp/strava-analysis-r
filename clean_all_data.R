# Clean all data
library(tidyverse)
library(janitor)

df <- read_csv("activities.csv")
df <- janitor::clean_names(df)

# Count NA in each column left, alternative: summarise(across(everything(), \(x) sum(is.na(x))))
summary_count_na <- colSums(is.na(df))

# Remove cols with more than 10 NA values
df_removed_na <- df |> 
    select(
        where(
            (\(x) sum(is.na(x)) < 10)
        )
    )

# Remove columns not of interest
df_remove_cols <- df_removed_na |> 
    select(!c(
        commute_10:filename, 
        prefer_perceived_exertion:from_upload,
        span_class_translation_missing_title_translation_missing_en_us_lib_export_portability_exporter_activities_horton_values_flagged_flagged_span,
        span_class_translation_missing_title_translation_missing_en_us_lib_export_portability_exporter_activities_horton_values_dirt_distance_dirt_distance_span)
    )

# Filter to run activities
df_runs <- df_remove_cols |> 
    filter(activity_type == "Run")

# Clean up distance columns
df_clean_distance <- df_runs |> 
    rename(distance_km = distance_7, distance_m = distance_18) |> 
    relocate(distance_km, .after = activity_type) |> 
    relocate(distance_m, .after = distance_km)

# Clean elapsed and moving time, add minutes version
df_time <- df_clean_distance |> 
    select(!elapsed_time_16) |> 
    rename(elapsed_time_s = elapsed_time_6, moving_time_s = moving_time) |> 
    relocate(moving_time_s, .before = elapsed_time_s) |> 
    
    # Add moving_time in minutes and hours
    mutate(moving_time_m = moving_time_s / 60, .after = moving_time_s) |> 
    mutate(moving_time_h = moving_time_m / 60, .after = moving_time_m) |> 
    
    # Add elapsed_time in minutes and hours
    mutate(elapsed_time_m = elapsed_time_s / 60, .after = elapsed_time_s) |> 
    mutate(elapsed_time_h = elapsed_time_m / 60, .after = elapsed_time_m)


# Average speed and pace
# Strava pace on site is elapsed_time / distance
# Strava speed from export is distance / moving_time
df_speed <- df_time |> 
    # Better names for speeds with type of time and unit
    rename(average_speed_moving_ms = average_speed) |> 
    rename(average_speed_elapsed_ms = span_class_translation_missing_title_translation_missing_en_us_lib_export_portability_exporter_activities_horton_values_avg_elapsed_speed_avg_elapsed_speed_span) |> 
    relocate(average_speed_elapsed_ms, .after = average_speed_moving_ms) |> 

    rename(max_speed_ms = max_speed) |> 
    
    ## 
    mutate(average_pace_moving_km = 1 / (average_speed_moving_ms / 1000 * 60), .after = average_speed_elapsed_ms) |> 
    mutate(average_pace_elapsed_km = elapsed_time_m / distance_km , .after = average_pace_moving_km)


