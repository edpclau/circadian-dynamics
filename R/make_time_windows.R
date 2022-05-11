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
#' @importFrom lubridate days
#' @importFrom furrr future_map_dfr
#' @importFrom dplyr filter
#' @import lubridate
#'
make_time_windows <- function(data = NULL, window_size_in_days = 3, window_step_in_days = 1){

# Set parameters
window_size = days(window_size_in_days) #Width of the window

# Finding dates where the window does not exceed the last time point in the data
times <- data[['datetime']]
step = seq(from = min(times), to = max(times), by = paste(window_step_in_days, "day")) #days to move the window



#plan for paralelization
future::plan(future::multisession, workers = 2)

# Creating a new data.frame where data is partitioned by window
return( furrr::future_map_dfr(.x = step,
                               ~ filter(data, (datetime >= .x) & (datetime <= .x + window_size)),
                               .id = "window",
                               .options = furrr::furrr_options(seed = TRUE), packages = 'lubridate')
)




}


