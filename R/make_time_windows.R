# Takes a time series data.frame and returns iterable time windows of any size
#' Create iterable time windows
#' @description Creates iterable time windows for a data.frame
#' @usage make_time_windows(data = NULL, window_size_in_days = 3, window_step_in_days = 1)
#' @param data a data.frame with 2 columns. Column 1 must be a POSIXct object and column 2 must be the measurement values.
#' @param window_size_in_days a numeric indicating the width of the window size in day units.
#' @param window_step_in_days a numeric indicating the amount of day by which to move the window in day units.
#'
#' @return
#' A data.frame cotaining:
#'
#' window     iterator by which to subset the data.frame
#' values     raw measurement data
#' @export
#'
#' @examples
#' windowed_data <- function(data = df)
#'
#' @import lubridate
#' @importFrom purrr map_df
#' @importFrom dplyr filter
#'
make_time_windows <- function(data = NULL, window_size_in_days = 3, window_step_in_days = 1){

# Set parameters
window_size <- days(window_size_in_days) #Width of the window
window_step <- days(window_step_in_days) #Days to move the window

# Finding dates where the window does not exceed the last time point in the data
days_in_data <- seq(from = min(data[[1]]) - days(1), to = max(data[[1]] + days(1)), by = "1 day")
usable_dates <- days_in_data[!(days_in_data + window_step + window_size >= max(data[[1]]))]

# Creating a new data.frame where data is partitioned by window
windowed_data <- map_df(usable_dates,
                               ~ filter(data, datetime >= . + window_step & datetime <= . + window_step + window_size),
                               .id = "window")



return(windowed_data)
}
