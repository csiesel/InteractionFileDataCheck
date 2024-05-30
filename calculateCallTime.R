if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
library(lubridate)


calculate_call_time <- function(start_date, delay_hours, start_window, end_window) {
  start_datetime <- tryCatch({
    ymd_hms(start_date)
  }, error = function(e) {
    stop("Invalid start date format. Ensure it's in 'YYYY-MM-DD HH:MM:SS' format.")
  })
  if (is.na(start_datetime)) {
    stop("Invalid start date format. Ensure it's in 'YYYY-MM-DD HH:MM:SS' format.")
  }

  target_datetime <- start_datetime + hours(delay_hours)

  is_in_window <- function(dt) {
    hour <- hour(dt)
    return((hour >= start_window) | (hour < end_window))
  }

  while (is_in_window(target_datetime)) {
    # Move the datetime to the end of the restricted time window
    target_datetime <- ceiling_date(target_datetime, unit = "day") + hours(end_window)
  }

  return(target_datetime)
}

# Example usage

start_date <- '2024-05-27 19:15:00'
start_window <- 20
end_window <- 8
delay_hours <- 26
ideal_call_time <- calculate_call_time(start_date, delay_hours, start_window, end_window)

# Print the result
print(ideal_call_time)
